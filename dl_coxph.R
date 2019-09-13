#!/usr/bin/env Rscript

#' ----------------------------------------------------------------------------
#' title: dl_coxph.R
#' description:
#'   Implementation of the distributed Cox Proportional Hazards algorithm based
#'   on an article from [Lu et al](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5009917/).
#'
#'   This implementation can be used with the PyTaskManager distributed learning
#'   infrastructure (see https://github.com/IKNL/pytaskmanager).
#'
#' author:
#'   Melle Sieswerda <m.sieswerda@iknl.nl>
#'   Anna van der Zalm <a.vanderzalm@iknl.nl>
#'   Gijs Geleijnse <g.geleijnse@iknl.nl>
#' date: 02-may-2018
#' license: MIT License
#' ----------------------------------------------------------------------------


# External libraries
library(rjson)
library(dplyr)

# Constants
MAX_COMPLEXITY = 250000

# ******************************************************************************
# ---- Helper functions ----
# ******************************************************************************

#' Write a string to STDOUT without the standard '[1]' prefix.
writeln <- function(x="", sep=" ") {
    cat(paste(paste(x, collapse=sep), "\n"))
}

#' Convert a list of rows into a dataframe
list.to.matrix <- function(l) {
  # sapply applies c() to the list and returns a matrix.
  # The matrix is then transposed
  return(t(sapply(l, c)))
}

#' Split the dataframe column wise into covariate, censor and time columns.
pre_process_data <- function(df, expl_vars, censor_col, time_col) {
  # Sort the dataframe/matrix by time
  sort_idx <- order(df[, time_col])
  df <- df[sort_idx, ]

  # Split dataframe into explanatory variables, time and censor columns
  Z <- df[, expl_vars]
  time <- df[, time_col]
  censor <- df[, censor_col]

  if (dim(as.matrix(Z))[2] == 1) {
    Z = as.matrix(Z)
  }

  # Return the list (dict)
  return(list(
    Z=Z,
    time=time,
    censor=censor
  ))
}

# ******************************************************************************
# ---- RPC entry points ----
# ******************************************************************************

#' Return a dataframe of unique event times
#'
#' Params:
#'   df: dataframe
#'   time_col: name of the column that contains the event/censor times
#'
#' Return:
#'   dataframe with columns time and Freq
RPC_get_unique_event_times_and_counts <- function(df, time_col, censor_col) {

  time <- df[df[, censor_col]==1, time_col]
  time <- sort(time)

  df_time <- as.data.frame(table(time), stringsAsFactors=F)

  return(df_time)
}

#' Compute the aggregate statistic of step 2
#' sum over all distinct times i
#'   sum the covariates of cases in the set of cases with events at time i.
#'
#' Params:
#'   df: dataframe
#'   expl_vars: list of explanatory variables (covariates) to use
#'   time_col: name of the column that contains the event/censor times
#'   censor_col: name of the colunm that explains whether an event occured or
#'               the patient was censored
#'
#' Return:
#'   numeric vector with sums and named index with covariates.
RPC_compute_summed_z <- function(df, expl_vars, time_col, censor_col) {

  data <- pre_process_data(df, expl_vars, censor_col, time_col)

  # Set condition to enable univariate Cox
  if (dim(data$Z)[2] > 1) {
    cases_with_events <- data$Z[data$censor == 1, ]
  } else {
    cases_with_events <- as.matrix(data$Z[data$censor == 1])
  }

  # Since an item can only be in a single set of events, we're essentially
  # summing over all cases with events.
  summed_zs <- colSums(cases_with_events)

  return(summed_zs)
}

#' Compute the three aggretated statistics needed for an iteration
#'
#' Params:
#'   df: dataframe
#'   expl_vars: list of explanatory variables (covariates) to use
#'   time_col: name of the column that contains the event/censor times
#'   censor_col: name of the colunm that explains whether an event occured or
#'               the patient was censored
#'   beta: vector of beta coefficients (length(beta) == length(expl_vars))
#'   times: vector of *globally* unique event times
#'
#' Return:
#'   list containing aggretated statistics
RPC_perform_iteration <- function(df, expl_vars, time_col, censor_col, beta, unique_event_times) {
    data <- pre_process_data(df, expl_vars, censor_col, time_col)

    D <- length(unique_event_times)
    m <- length(expl_vars)

    # initialize matrices for the aggregates we're about to compute
    agg1 <- array(dim=c(D))
    agg2 <- array(dim=c(D, m))
    dimnames(agg2) <- list(NULL, expl_vars)

    agg3 <- array(dim=c(D, m, m))
    dimnames(agg3) <- list(NULL, expl_vars, expl_vars)

    for (i in 1:D) {
      cat('.')
      # Compute the risk set at time t; this includes *all* patients that have a
      # survival time greater than or equal to the current time
      R_i <- as.matrix(data$Z[data$time >= unique_event_times[i], ])

      # aggregate 1: SUM_risk[exp(beta * z)]
      ebz <- exp(R_i %*% beta)
      agg1[i] <- sum(ebz)

      # aggregate 2: SUM_risk[z_r *exp(beta * z)]
      # Use apply to multiply each column (element-wise) in R_i with ebz
      z_ebz <- apply(R_i, 2, '*', ebz)

      # Undo the simplification that `apply` does in case of a single row in R_i
      if (nrow(R_i) == 1) {
        z_ebz <- t(z_ebz)
      }

      agg2[i, ] <- colSums(z_ebz)

      # aggregate 3: SUM_risk[z_r * z_q * exp(beta * z)]
      summed <- matrix(0, nrow=m, ncol=m)
      for (j in 1:nrow(R_i)) {
        # z_ebz[j, ]: numeric vector
        # the outer product creates a matrix:
        # | z1*z1 | z1*z2 | ... | z1*zm |
        # | z2*z1 | z2*z2 | ... | z2*zm |
        # | ...   | ...   | ... | ...   |
        # | zm*z1 | zm*z2 | ... | zm*zm |
        summed <- summed + z_ebz[j, ] %*% t(R_i[j, ])
      }

      agg3[i, , ] <- summed
    }
    writeln('')

    return(
      list(
        agg1=agg1,
        agg2=agg2,
        agg3=agg3
      )
    )
}

# ******************************************************************************
# ---- Server/orchestrator functions ----
# ******************************************************************************

#' Compute/count the *global* number of ties at each time point using the
#' tie-counts from the individual sites.
#'
#' Params:
#'   Ds: list of *local* tie-counts, indexed by site nr
#'
#' Return:
#'   numeric vector with tie-counts and named index with unique
#'   event times
compute_combined_ties <- function(Ds) {

  # Merge the list of event times & counts into a single data frame
  for (k in 1:length(Ds)) {
    # This only works if the joined columns have different names
    site_name <- sprintf("site_%i", k)
    colnames(Ds[[k]]) <- c("time", site_name)

    if (k == 1) {
      D <- Ds[[k]]
    } else {
      D <- merge(D, Ds[[k]], by="time", all=T)
    }
  }

  # Cast the time column to numeric to enable proper sorting
  D[, "time"] <- as.numeric(D$time)
  D <- D[order(D$time), ]

  # Set the time column as the index
  rownames(D) <- D$time

  # Drop the time column; drop=F ensures that the data frame is not
  # coerced to a vector in case of a single column.
  D <- D[, -1, drop=F]

  # The merge/join will have introduced NAs: set these to 0
  D[is.na(D)] <- 0

  # Sum the columns to get the total nr of ties for each time
  D_all <- rowSums(D)

  return(D_all)
}

#' Compute the primary derivative (vector of length beta)
compute_derivatives <- function(z_hat, D_all, aggregates) {

  # Sum the aggregate statistics for each site
  for (k in 1:length(aggregates)) {
    if (k == 1) {
      summed_agg1 <- aggregates[[k]]$agg1
      summed_agg2 <- aggregates[[k]]$agg2
      summed_agg3 <- aggregates[[k]]$agg3
    } else {
      summed_agg1 <- summed_agg1 + aggregates[[k]]$agg1
      summed_agg2 <- summed_agg2 + aggregates[[k]]$agg2
      summed_agg3 <- summed_agg3 + aggregates[[k]]$agg3
    }
  }

  # summed_agg1: vector with scalar for each time point
  # summed_agg2: vector of length m for each time point
  # summed_agg3: matrix of size (m x m) for each time point
  for (i in 1:length(D_all)) {
    # primary
    s1 <- D_all[[i]] * (summed_agg2[i, ] / summed_agg1[i])

    # secondary
    first_part <- (summed_agg3[i, , ] / summed_agg1[i])

    # the numerator is the outer product of agg2
    numerator <- summed_agg2[i, ] %*% t(summed_agg2[i, ])
    denominator <- summed_agg1[i] * summed_agg1[i]
    second_part <- numerator / denominator

    s2 <- D_all[[i]] * (first_part - second_part)

    if (i == 1) {
      total_p1 <- s1
      total_p2 <- s2

    } else {
      total_p1 <- total_p1 + s1
      total_p2 <- total_p2 + s2
    }
  }

  primary_derivative <- z_hat - total_p1
  secondary_derivative <- -total_p2

  return(list(
    primary=primary_derivative,
    secondary=secondary_derivative
  ))
}



# ******************************************************************************
# ---- Infrastructure functions ----
# ******************************************************************************

#' Run the method requested by the server
#'
#' Params:
#'   df: data frame containing the *local* dataset
#'   input_data: string containing serialized JSON; JSON should contain
#'               the keys 'method', 'args' and 'kwargs'
#'
#' Return:
#'   Requested method's output
dispatch_RPC <- function(df, input_data) {
  # Determine which method was requested and combine arguments and keyword
  # arguments in a single variable
  input_data <- fromJSON(input_data)
  method <- sprintf("RPC_%s", input_data$method)

  input_data$args <- readRDS(textConnection(input_data$args))
  input_data$kwargs <- readRDS(textConnection(input_data$kwargs))

  args <- c(list(df), input_data$args, input_data$kwargs)

  # Call the method
  writeln(sprintf("Calling %s", method))
  result <- do.call(method, args)

  # Serialize the result
  writeln("Serializing result")
  fp <- textConnection("result_data", open="w")
  saveRDS(result, fp, ascii=T)
  close(fp)
  result <- result_data

  return(result)
}


#' Wait for the results of a distributed task and return the task,
#' including results.
#'
#' Params:
#'   client: ptmclient::Client instance.
#'   task: list with the key id (representing the task id)
#'
#' Return:
#'   task (list) including results
wait_for_results <- function(client, task) {

  path = sprintf('/task/%s', task$id)

  while(TRUE) {
    r <- client$GET(path)

    if (content(r)$complete) {
      break

    } else {
      # Wait 30 seconds
      writeln("Sleeping ...")
      Sys.sleep(5)
    }
  }

  path = sprintf('/task/%s?include=results', task$id)
  r <- client$GET(path)

  return(content(r))
}


#' Create a data structure used as input for a call to the distributed
#' learning infrastructure.
create_task_input = function(method, ...) {
  # Construct the input_data list from the ellipsis.
  arguments <- list(...)

  if (is.null(names(arguments))) {
    args <- arguments
    kwargs <- list()

  } else {
    args <- arguments[names(arguments) == ""]
    kwargs <- arguments[names(arguments) != ""]
  }

  # Serialize the argument values to ASCII
  fp <- textConnection("arg_data", open="w")
  saveRDS(args, fp, ascii=T)
  close(fp)

  # Serialize the keyword argument values to ASCII
  fp <- textConnection("kwarg_data", open="w")
  saveRDS(kwargs, fp, ascii=T)
  close(fp)

  # Create the data structure
  input_data <- list(
    method=method,
    args=arg_data,
    kwargs=kwarg_data
  )

  return(input_data)
}


#' Execute a method on the distributed learning infrastructure.
#'
#' This entails ...
#'  * creating a task and letting the hubs execute the method
#'    specified in the 'input' parameter
#'  * waiting for all results to arrive
#'  * deserializing each sites' result using readRDS
#'
#' Params:
#'   client: ptmclient::Client instance.
#'   method: name of the method to call on the distributed learning
#'           infrastructure
#'   ...: (keyword) arguments to provide to method. The arguments are serialized
#'        using `saveRDS()` by `create_task_input()`.
#'
#' Return:
#'   return value of called method
call <- function(client, method, ...) {
  # Create the json structure for the call to the server
  input <- create_task_input(method, ...)

  task = list(
    "name"="CoxPH",
    "image"="ananyac/dl_coxph",
    "collaboration_id"=client$get("collaboration_id"),
    "input"=input,
    "description"=""
  )

  # Create the task on the server; this returs the task with its id
  r <- client$POST('/task', task)

  # Wait for the results to come in
  result_dict <- wait_for_results(client, content(r))

  # result_dict is a list with the keys _id, id, description, complete, image,
  # collaboration, results, etc. the entry "results" is itself a list with
  # one entry for each site. The site's actual result is contained in the
  # named list member 'result' and is encoded using saveRDS.
  sites <- result_dict$results
  results <- list()

  for (k in 1:length(sites)) {
    results[[k]] <- readRDS(textConnection(sites[[k]]$result))
  }

  return(results)
}


#' Mock an RPC call to all sites.
#'
#' Params:
#'   client: ptmclient::Client instance.
#'   method: name of the method to call on the distributed learning
#'           infrastructure
#'   ...: (keyword) arguments to provide to method. The arguments are serialized
#'        using `saveRDS()` by `create_task_input()`.
#'
#' Return:
#'   return value of called method
mock.call <- function(client, method, ...) {

  writeln(sprintf('** Mocking call to "%s" **', method))
  datasets <- client$datasets
  input_data <- create_task_input(method, ...)
  input_data <- toJSON(input_data)

  # Create a list to store the responses from the individual sites
  results <- list()

  # Mock calling the RPC method on each site
  for (k in 1:length(datasets)) {
    result <- dispatch_RPC(datasets[[k]], input_data)
    results[[k]] <- readRDS(textConnection(result))
  }

  writeln()
  return(results)
}


#' Run the distributed CoxPH algorithm.
#'
#' By specifying call.method=mock.call the algorithm runs locally.
#'
#' Params:
#'   client: ptmclient::Client instance.
#'   expl_vars: list of explanatory variables (covariates) to use
#'   time_col: name of the column that contains the event/censor times
#'   censor_col: name of the colunm that explains whether an event occured or
#'               the patient was censored
#'
#' Return:
#'   data.frame with beta, p-value and confidence interval for each explanatory
#'   variable.
dcoxph <- function(client, expl_vars, time_col, censor_col, call.method=call) {
  m <- length(expl_vars)

  # Ask all hubs to return their unique event times with counts
  writeln("Getting unique event times and counts")
  results <- call.method(client, "get_unique_event_times_and_counts", time_col, censor_col)

  Ds <- lapply(results, as.data.frame)

  D_all <- compute_combined_ties(Ds)
  unique_event_times <- as.numeric(names(D_all))

  complexity <- length(unique_event_times) * length(expl_vars)^2
  writeln("********************************************")
  writeln(c("Complexity:", complexity))
  writeln("********************************************")

  if (complexity > MAX_COMPLEXITY) {
    stop("*** This computation will be too heavy on the nodes! Aborting! ***")
  }

  # Ask all hubs to compute the summed Z statistic
  writeln("Getting the summed Z statistic")
  summed_zs <- call.method(client, "compute_summed_z", expl_vars, time_col, censor_col)

  # z_hat: vector of same length m
  # Need to jump through a few hoops because apply simplifies a matrix with one row
  # to a numeric (vector) :@
  z_hat <- list.to.matrix(summed_zs)
  z_hat <- apply(z_hat, 2, as.numeric)
  z_hat <- matrix(z_hat, ncol=m, dimnames=list(NULL, expl_vars))
  z_hat <- colSums(z_hat)


  # Initialize the betas to 0 and start iterating
  writeln("Starting iterations ...")
  beta <- beta_old <- rep(0, m)
  delta <- 0

  i = 1
  while (i <= 30) {
    writeln(sprintf("-- Iteration %i --", i))
    writeln("Beta's:")
    print(beta)
    writeln()

    writeln("delta: ")
    print(delta)
    writeln()

    aggregates <- call.method(client, "perform_iteration", expl_vars, time_col, censor_col, beta, unique_event_times)

    # Compute the primary and secondary derivatives
    derivatives <- compute_derivatives(z_hat, D_all, aggregates)
    # print(derivatives)

    # Update the betas
    beta_old <- beta
    beta <- beta_old - (solve(derivatives$secondary) %*% derivatives$primary)

    delta <- abs(sum(beta - beta_old))
    if (delta <= 10^-8) {
      writeln("Betas have settled! Finished iterating!")
      break
    }

    # Again!!?
    i <- i + 1
  }

  # Computing the standard errors
  SErrors <- NULL
  fisher <- solve(-derivatives$secondary)

  # Standard errors are the squared root of the diagonal
  for(k in 1:dim(fisher)[1]){
    se_k <- sqrt(fisher[k,k])
    SErrors <- c(SErrors, se_k)
  }

  # Calculating P and Z values
  zvalues <- (exp(beta)-1)/SErrors
  pvalues <- 2*pnorm(-abs(zvalues))
  pvalues <- format.pval(pvalues, digits = 1)

  # 95%CI = beta +- 1.96 * SE
  results <- data.frame("coef"=round(beta,5), "exp(coef)"=round(exp(beta), 5), "SE"=round(SErrors,5))
  results <- mutate(results, lower_ci=round(exp(coef - 1.96 * SE), 5))
  results <- mutate(results, upper_ci=round(exp(coef + 1.96 * SE), 5))
  results <- mutate(results, "Z"=round(zvalues, 2), "P"=pvalues)
  row.names(results) <- rownames(beta)

  return(results)
}


#' Run the distributed CoxPH algorithm locally
#'
#' Splits the provided data frame in `splits` equal parts and runs
#' `dcoxph()` with `call.method=mock.call`.
#'
#' Params:
#'   df: data frame containing the *full* dataset
#'   expl_vars: list of explanatory variables (covariates) to use
#'   time_col: name of the column that contains the event/censor times
#'   censor_col: name of the colunm that explains whether an event occured or
#'               the patient was censored
#'   splits: number of parts to split the data set in
#'
#' Return:
#'   data.frame with beta, p-value and confidence interval for each explanatory
#'   variable.
dcoxph.mock <- function(df, expl_vars, time_col, censor_col, splits=5) {

  datasets <- list()

  for (k in 1:splits) {
    datasets[[k]] <- df[seq(k, nrow(df), by=splits), ]
  }

  client <- MockClient(datasets)
  results <- dcoxph(client, expl_vars, time_col, censor_col, call.method=mock.call)
  return(results)
}


#' Entrypoint when excecuting this script using Rscript
#'
#' Wraps the docker input/output for `dispatch_RPC()`.
#' Deserialization/serialization is performed in `dipatch_RPC()` to enable
#' testing.
docker.wrapper <- function() {
  database_uri <- Sys.getenv("DATABASE_URI")
  writeln(sprintf("Using '%s' as database", database_uri))
  df <- read.csv(database_uri)

  # Read the contents of file input.txt into 'input_data'
  writeln("Loading input.txt")
  filename <- 'input.txt'
  input_data <- readChar(filename, file.info(filename)$size)

  writeln("Dispatching ...")
  result <- dispatch_RPC(df, input_data)

  # Write result to disk
  writeln("Writing result to disk .. ")
  writeLines(result, "output.txt")

  writeln("")
  writeln("[DONE!]")
}


# ******************************************************************************
# ---- main() ----
# ******************************************************************************
if (!interactive()) {
    docker.wrapper()
}

