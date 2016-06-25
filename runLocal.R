library(Rook)
require("rjson")
classifier <<- "2374f9x69-nlc-6914" # the nlc id
setwd("~/GitHub/NLC_RR_handler/")
source("rookApp.R")
source("udfs.R")
source("udfs_watson.R")
source("udfsB.R")
myPort <- 1234
myInterface <- "10.0.50.252"
status <- -1

# R 2.15.1 uses .Internal, but the next release of R will use a .Call.
# Either way it starts the web server.
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
  # s$handler(appName = "summarize")
  
  s$print()
  
  # Now make the console go to sleep. Of course the web server will still be
  # running.
  # while (TRUE) Sys.sleep(24 * 60 * 60)
}