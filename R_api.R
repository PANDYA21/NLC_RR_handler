require('Rook')
load_or_install("rjson")
classifier <<- "2374f9x69-nlc-6914" # the nlc id
source("udfs.R")
source("udfs_watson.R")
source("udfsB.R")

# rook$stop()
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

rook$start("summarize", port = "1234")


## post with upload file
ress <- POST("http://127.0.0.1:1234/custom/summarize",encode = "multipart", body = upload_file("bb_history.csv"))
ress
cat(content(ress, "text"))
## post with string
ress <- POST("http://127.0.0.1:1234/custom/summarize", encode = "multipart", body = "I play tennis often", content_type("text"))
ress
cat(content(ress, "text"))
## get
ress <- GET("http://127.0.0.1:1234/custom/summarize")
cat(content(ress, "text"))
