source("Client.R")
source("dl_coxph.R")
client <- Client("http://137.117.138.98:5000/api", "example_username", "example_password", 5) #please replace with correct username and password
client$authenticate()

# Parameters used to interpret the hub's datastore
expl_vars <- c("Age","Race2","Race3","Mar2","Mar3","Mar4","Mar5","Mar9",
               "Hist8520","hist8522","hist8480","hist8501","hist8201",
               "hist8211","grade","ts","nne","npn","er2","er4")
time_col <- "Time"
censor_col <- "Censor"
results <- dcoxph(client, expl_vars, time_col, censor_col)
