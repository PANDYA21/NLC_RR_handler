# Sys.setlocale(, "en_us")

oldwd <- getwd()
source("rookApp.R")
source("udfs.R")
source("udfsB.R")
source("udfs_watson.R")
load_or_install("rjson")
library('Rook')
library('httr')
load_or_install("data.table", repos='http://cran.rstudio.com/')
load_or_install("tidyr", repos='http://cran.rstudio.com/')
load_or_install("dplyr", repos='http://cran.rstudio.com/')
load_or_install("stringr", repos='http://cran.rstudio.com/')
load_or_install("splitstackshape", repos='http://cran.rstudio.com/')
classifier <<- "2374f9x69-nlc-6914" # the nlc id
on.exit(setwd(oldwd))

# # run the app
# app <- shinyApp(ui = ui, server = server)

# runApp(app, launch.browser = F, port = 1234)

if (Sys.getenv('VCAP_APP_PORT') == "") {
  # In case we're on a local system, run this:
  print('running locally')
  # runApp(app, port=1234, launch.browser=F)
  
} else {
  # In case we're on Cloudfoundry, run this:
  print('running on CF')
  
  # Starting Rook - webserver 
  library(Rook)
  myPort <- as.numeric(Sys.getenv('VCAP_APP_PORT'))
  print(Sys.getenv("CF_INSTANCE_IP"))
  print(Sys.getenv("PORT"))
  myPort <- as.numeric(Sys.getenv("PORT"))
  myInterface <- "0.0.0.0" # Sys.getenv("CF_INSTANCE_IP") # Sys.getenv('VCAP_APP_HOST')
  status <- -1
  
  if (as.integer(R.version[["svn rev"]]) > 59600) {
    status <- .Call(tools:::startHTTPD, myInterface, myPort)
  } else {
    status <- .Internal(startHTTPD(myInterface, myPort))
  }
  
  if (status == 0) {
    unlockBinding("httpdPort", environment(tools:::startDynamicHelp))
    assign("httpdPort", myPort, environment(tools:::startDynamicHelp))
    
    s <- Rhttpd$new()
    s$listenAddr <- myInterface
    s$listenPort <- myPort
    
    # Change this line to your own application. You can add more than one
    # application if you like
    #
    s$add(name = "summarize", 
          app = rookApp)
    s$print()
    Sys.sleep(24*7*3600)
    
    sink(stderr())
    write("prints to stderr", stderr())
    write("prints to stdout", stdout())
    
    # Now make the console go to sleep. Of course the web server will still be
    # running.
    # while (TRUE) Sys.sleep(24 * 60 * 60)
  }
}
