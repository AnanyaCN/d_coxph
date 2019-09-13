# Libraries
library(httr)
library(rjson)

# ******************************************************************************
# ---- Helper functions ----
# ******************************************************************************

#' Write a string to STDOUT without the standard '[1]' prefix.
writeln <- function(x="", sep=" ") {
  cat(paste(paste(x, collapse=sep), "\n"))
}

# ******************************************************************************
# ---- class Client ----
# ******************************************************************************

#' Client class constructor
#'
#' Params:
#'   host: hostname (may include port)
#'   username: username
#'   password: password
#'
#' Return:
#'   Client
Client <- function(host, username, password, collaboration_id, api_path='') {
  # Function arguments are automatically available in the
  # environment, so no need to assign them explicitly.
  env <- environment()

  # Define the class methods. Attributes are accessed through
  # get/set
  self <- list(
    #' Convenient access to the environment
    dict = env,

    #' Generic getter
    get = function(x) {
      return(get(x, env))
    },

    #' Generic setter
    set = function(x, value) {
      assign(x, value, env)
    },

    #' Authenticate with the server; sets the access and refresh tokens.
    authenticate = function() {
      # Create the URL and data for the JSON body
      url <- paste(env$host, env$api_path, '/token/user', sep='')
      print(url)

      data <- list(
        username=env$username,
        password=env$password
      )

      r <- POST(url, body=data, encode="json")

      if (r$status_code != 200) {
        stop(sprintf("Could not authenticate: %s", http_status(r)$message))
      }

      # Apparently we were succesful. Retrieve the details from the server
      # response.
      response_data <- content(r)
      list2env(response_data, env)

      return("OK")
    },

    #' Refresh the access token using the refresh token
    refresh_token = function() {
      if (is.null(env$refresh_url)) {
        stop("Not authenticated!")
      }

      url <- paste(env$host, env$refresh_url, sep='')
      token <- sprintf('Bearer %s', env$refresh_token)

      r <- POST(url, add_headers(Authorization=token))

      if (r$status_code != 200) {
        stop("Could not refresh token!?")
      }

      # Apparently we were succesful. Retrieve the details from the server
      # response, which includes the key "access_token".
      response_data <- content(r)
      list2env(response_data, env)

      return("OK")
    },

    #' Perform a request to the server
    request = function(method, path, data=NULL, first_try=T) {
      url <- paste(env$host, env$api_path, path, sep='')
      token <- sprintf('Bearer %s', env$access_token)

      if (method == 'GET') {
        r <- GET(url, add_headers(Authorization=token))

      } else if (method == 'POST') {
        r <- POST(url, body=data, encode="json", add_headers(Authorization=token))

      } else if (method == 'PUT') {
        r <- PUT(url, body=data, encode="json", add_headers(Authorization=token))

      }

      if (r$status_code != 200) {
        msg <- sprintf("Request unsuccesful: %s", http_status(r)$message)

        if (first_try) {
          writeln(msg)
          writeln("Refreshing token ... ")
          self$refresh_token()

          r <- self$request(method, path, data, first_try=F)

        } else {
          stop(msg)
        }

      }

      return(r)
    },

    #' Perform a GET request to the server
    GET = function(path) {
      return(self$request("GET", path))
    },

    #' Perform a POST request to the server
    POST = function(path, data=NULL) {
      return(self$request("POST", path, data))
    },

    #' Perform a PUT request to the server
    PUT = function(path, data=NULL) {
      return(self$request("PUT", path, data))
    },

    #' Return a string representation of this Client
    repr = function() {
      return(sprintf("Client(host='%s', username='%s')", env$host, env$username))
    }
  )

  # Set the class name
  class(self) <- append(class(self), "Client")

  # Return the new object
  return(self)
}


# ******************************************************************************
# ---- class MockClient ----
# ******************************************************************************

#' Fake client
MockClient <- function(datasets) {
  env <- environment()
  self <- list(
    datasets = datasets
  )
  return(self)
}


