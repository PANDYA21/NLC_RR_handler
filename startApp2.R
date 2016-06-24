# source files and assign data

if (Sys.getenv('VCAP_APP_PORT') == "") {
  # In case we're on a local system, run this:
  print('running locally')
  runApp('hitandmiss',port=8000,launch.browser=F)
  
} else {
  # In case we're on Cloudfoundry, run this:
  print('running on CF')
  
  # Starting Rook server during CF startup phase - after 60 seconds start the actual Shiny server
  library(Rook)
  myPort <- as.numeric(Sys.getenv('VCAP_APP_PORT'))
  myInterface <- Sys.getenv('VCAP_APP_HOST')
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
    s$add(name = "summarize", 
          app = function(env) {
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
          })
    s$print()
    Sys.sleep(60)
    # s$stop()
  }
  
  
  # run shiny server
  sink(stderr())
  write("prints to stderr", stderr())
  write("prints to stdout", stdout())
}
