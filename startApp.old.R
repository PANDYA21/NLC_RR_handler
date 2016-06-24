# Sys.setlocale(, "en_us")

oldwd <- getwd()
# setwd("headprod/")
# source("udfs.R")
# source("udfsB.R")
# source("udfs_watson.R")
# load_or_install("rjson")
library('Rook')
library('httr')
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
  
  # Starting Rook server during CF startup phase - after 60 seconds start the actual Shiny server
  library(Rook)
  myPort <- as.numeric(Sys.getenv('VCAP_APP_PORT'))
  myInterface <- Sys.getenv('VCAP_APP_HOST')
  status <- -1
  
  # # R 2.15.1 uses .Internal, but the next release of R will use a .Call.
  # # Either way it starts the web server.
  # if (as.integer(R.version[["svn rev"]]) > 59600) {
  #   status <- .Call(tools:::startHTTPD, myInterface, myPort)
  # } else {
  #   status <- .Internal(startHTTPD(myInterface, myPort))
  # }
  # 
  # if (status == 0) {
    # unlockBinding("httpdPort", environment(tools:::startDynamicHelp))
    # assign("httpdPort", myPort, environment(tools:::startDynamicHelp))
    
    # getSettable <- function(default){
    #   function(obj = NA){if(!is.na(obj)){default <<- obj};
    #     default}
    # }
    # myHttpdPort <- getSettable(myPort)
    # unlockBinding("httpdPort", environment(tools:::startDynamicHelp))
    # assign("httpdPort", myHttpdPort, environment(tools:::startDynamicHelp))
    
    unlockBinding("httpdPort", environment(tools:::startDynamicHelp))
    assign("httpdPort", myPort, environment(tools:::startDynamicHelp))

    # s <- Rhttpd$new()
    # s$listenAddr <- myInterface
    # s$listenPort <- myPort
    # 
    # s$print()
    # Sys.sleep(5) # Sys.sleep(60)
    # s$stop()
    
    sink(stderr())
    write("prints to stderr", stderr())
    write("prints to stdout", stdout())
    
    rook = Rhttpd$new()
    rook$add(
      name ="summarize",
      app  = function(env) {
        req <- Request$new(env)
        res <- Response$new()
        if(req$post()){
          post <- req$POST() # the input from user
          # print(post)
          post.ans <- watson.nlc.processtextreturnclass(classifier, as.character(names(post)))
          res$write(toJSON(post.ans)) # response for the POST 
        }
        if(req$get()){
          res$write(toJSON("get requested")) # response for the GET
        }
        res$finish()
      }
    )
    # rook$listenAddr <- myInterface
    # rook$listenPort <- myPort
    # rook$start("summarize")
    rook$start(listen = myInterface, port = myPort, quiet = F)
    # rook$print()


# check for newer version
    # getSettable <- function(default){
    #   function(obj = NA){if(!is.na(obj)){default <<- obj};
    #     default}
    # }
    # myHttpdPort <- getSettable(myPort)
    # unlockBinding("httpdPort", environment(tools:::startDynamicHelp))
    # assign("httpdPort", myHttpdPort, environment(tools:::startDynamicHelp))

    # s <- Rhttpd$new()
    # s$listenAddr <- myInterface
    # s$listenPort <- myPort

    # s$print()
    # Sys.sleep(60)
    # s$stop()
  # }
}
