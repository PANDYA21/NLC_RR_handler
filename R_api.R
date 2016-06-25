require('Rook')
require("rjson")
classifier <<- "2374f9x69-nlc-6914" # the nlc id
setwd("~/GitHub/NLC_RR_handler/")
source("udfs.R")
source("udfs_watson.R")
source("udfsB.R")

rook$stop()
# rook = Rhttpd$new()
# rook$add(
#   name ="summarize",
#   app  = function(env) {
#     req <- Request$new(env)
#     res <- Response$new()
#     if(req$post()){
#       post <- req$POST() # the input from user
#       print(names(post))
#       post.ans <- watson.nlc.processtextreturnclass(classifier, as.character(names(post)))
#       print(post.ans)
#       res$write(toJSON(post.ans)) # response for the POST 
#     }
#     if(req$get()){
#       res$write(toJSON("get requested")) # response for the GET
#     }
#     res$finish()
#   }
# )
# 
# rook$listenAddr <- "10.0.50.252"
# rook$start(port = "1234")
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
# rook$listenAddr <- "127.0.0.1"
# rook$listenPort <- "1234"
# rook$start("summarize")
myInterface <- "127.0.0.1"
myPort <- "1234"
rook$start(listen = myInterface, port = myPort, quiet = F)
# rook$print()


## post with upload file
ress <- POST("http://127.0.0.1:1234/custom/summarize",encode = "multipart", body = upload_file("bb_history.csv"))
ress
cat(content(ress, "text"))
## post with string
ress <- POST("http://127.0.0.1:1234/custom/summarize", encode = "multipart", body = "I am 26", content_type("text"))
ress
cat(content(ress, "text"))
## post with string
ress <- POST("http://127.0.0.1:1234/custom/summarize", encode = "multipart", body = "I want cloths", content_type("text"))
ress
cat(content(ress, "text"))
## post with string
ress <- POST("http://127.0.0.1:1234/custom/summarize", encode = "multipart", 
             body = "I am 26. looking for cloth", content_type("text"))
ress
cat(content(ress, "text"))
## post with string
ress <- POST("http://10.0.50.252:1234/custom/summarize", encode = "multipart", 
             body = "I am 26. looking for cloth", content_type("text"))
ress
cat(content(ress, "text"))
## post with string on bluemix 
ress <- POST("https://nlc-rr-handler.eu-gb.mybluemix.net/custom/summarize", encode = "multipart", 
             body = "I am looking for 70 year old person", content_type("text"))
ress
cat(content(ress, "text"))
## get
ress <- GET("http://127.0.0.1:1234/custom/summarize")
cat(content(ress, "text"))
## get
ress <- GET("http://localhost:1234/custom/summarize")
cat(content(ress, "text"))


## post with JSON
ress <- POST("http://10.0.50.252:1234/custom/summarize", encode = "multipart", 
             body = "{\"user_pass\":\"nlc_rr_handler:hackathon@2016\", \"query\":\"I am 74\"}", content_type("text"))
ress
cat(content(ress, "text"))


## post with JSON
ress <- POST("http://10.0.50.252:1234/custom/summarize", encode = "multipart", 
             body = '{"user_pass":"nlc_rr_handler:hackathon@2016", 
             "query":"I am male", 
             "conv_id":"NULL"}', content_type("text"))
ress
fromJSON(content(ress, "text"))
fromJSON(content(ress, "text"))$conv_id
read.before <- readLines("22272.chat")

## post with continuing chat
ress <- POST("http://10.0.50.252:1234/custom/summarize", encode = "multipart", 
             body = '{"user_pass":"nlc_rr_handler:hackathon@2016", 
             "query":"I am 26", 
             "conv_id": 22272}', content_type("text"))
ress
fromJSON(content(ress, "text"))
read.after <- readLines("22272.chat")

# see the difference
read.before
read.after


## post with continuing chat
ress <- POST("http://10.0.50.252:1234/custom/summarize", encode = "multipart", 
             body = '{"user_pass":"nlc_rr_handler:hackathon@2016", 
             "query":"I am loking for cloths", 
             "conv_id": 22272}', content_type("text"))
ress
fromJSON(content(ress, "text"))
readLines("22272.chat")


## invalid question
ress <- POST("http://10.0.50.252:1234/custom/summarize", encode = "multipart", 
             body = '{"user_pass":"nlc_rr_handler:hackathon@2016", 
             "query":"i play ", 
             "conv_id":"start"}', content_type("application/json"))
ress
# fromJSON(content(ress, "text"))
ress$all_headers

## invalid question
ress <- POST("http://nlc-rr-handler.eu-gb.mybluemix.net/custom/summarize", encode = "multipart", 
             body = '{"user_pass":"nlc_rr_handler:hackathon@2016", 
             "query":"i play ", 
             "conv_id":"NULL"}', content_type("text"))
ress
ress$all_headers
fromJSON(content(ress, "text"))


# actual JSON passing
## invalid question
ressb <- POST("https://nlc-rr-handler.eu-gb.mybluemix.net/custom/summarize", encode = "multipart", 
             body = toJSON(list(user_pass="nlc_rr_handler:hackathon@2016", query="I am male", conv_id="start")), 
             content_type("Application/json"))
ressb$headers
ressb$all_headers
# ressb$request
cat(content(ressb, "text"))



## POST with a json file (interesting)
ressb <- POST("https://nlc-rr-handler.eu-gb.mybluemix.net/custom/summarize", encode = "multipart", 
              body = upload_file("samplerequest.json"), 
              content_type("Application/json"))
ressb$headers
ressb$all_headers
# ressb$request
cat(content(ressb, "text"))

