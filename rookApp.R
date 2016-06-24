### the rook based web´-service app taht accepts JSON via POST and 
### responds with JSON in the following formats: 
### 
### the POST request
### '{
### "user_pass":"nlc_rr_handler:hackathon@2016",
### "query":"I am good",
### "conv_id":["existing_id_returned_from_first_response" OR "NULL"],
### }'
### "NULL" conv_id will start a new chat
###
### the POST response
### '{
### "conv_id":"conversation_id",
### "next_question":"question_text",
### "rr_data":"response_from_RR_Watson"
### }'

uname.api <- "nlc_rr_handler"
pswd.api <- "hackathon@2016"
user_pass.api <- paste0(uname.api, ":", pswd.api, collapse = "")

rookApp <- function(env) {
  req <- Request$new(env)
  # res <- Response$new(headers=list("Content-Type"="application/json" , 
  #                                  "Access-Control-Allow-Origin"="*", # "http://nlc-rr-handler.eu-gb.mybluemix.net/", #"*",
  #                                  "access-control-allow-credentials"="TRUE"))
  res <- Response$new()
  if(req$post()){
    
    post <- req$POST() # the input from user
    post <- names(post)
    post <- fromJSON(post)
    
    if(post$user_pass == user_pass.api){
      
      # authentication successful
      # if conv_id is null start a new chat
      if(post$conv_id == "NULL"){
        
        # start a new chat
        chat_id <- sample(c(10000:50000), 1)
        post.ans <- list()
        post.ans$conv_id <- chat_id
        rm(chat_id)
        
        # send the user input to NLC
        nlc.res <- watson.nlc.processtextreturnclass(classifier, as.character(post$query))
        class.ans <- nlc.res$class[nlc.res$confidence > 0.75]
        
        # save chat history for this conversation
        chatt <- paste0(as.character(post$query), ":", class.ans)
        writeLines(text = chatt, 
                   con = paste0(as.character(post.ans$conv_id), ".chat"))
        
        # post.ans$next_question <- class.ans # "next question will go here ..."
        # look for existing data and send next question
        asked <- unlist(lapply(strsplit(chatt, ":"), function(x) x[2]))
        if(length(asked) < 3){
          
          # ask the remaining questions
          all.classes <- c("user_gender", "user_age", "query_cloth")
          rem.classes <- all.classes[!(all.classes %in% asked)]
          rem.classes <- rem.classes[1]
          switch (rem.classes,
                  "user_gender" = {next.que <- "What is your gender?"},
                  "user_age" = {next.que <- "What is your age?"},
                  "query_cloth" = {next.que <- "What are you looking for?"})
        }
        post.ans$next_question <- next.que # next question will go here ...
        
        # send request to rr
        post.ans$rr_data <- "NULL"
        
      } else {
        
        # continue older chat
        if(file.exists(paste0(post$conv_id, ".chat"))){
          
          # read the chat history for the given chat id
          chatt <- readLines(con = paste0(post$conv_id, ".chat"))
        } else {
          
          # user history not found
          chatt <- character(0)
        }
        
        # continue the chat
        post.ans <- list()
        post.ans$conv_id <- post$conv_id
        
        # send the user input to NLC
        nlc.res <- watson.nlc.processtextreturnclass(classifier, as.character(post$query))
        class.ans <- nlc.res$class[nlc.res$confidence > 0.75]
        
        # save chat history for this conversation
        chatt <- c(chatt, paste0(as.character(post$query), ":", class.ans))
        writeLines(text = chatt, con = paste0(as.character(post$conv_id), ".chat"))
        
        # look for existing data and send next question
        asked <- unlist(lapply(strsplit(chatt, ":"), function(x) x[2]))
        if(length(asked) < 3){
          
          # ask the remaining questions
          all.classes <- c("user_gender", "user_age", "query_cloth")
          rem.classes <- all.classes[!(all.classes %in% asked)]
          
          if(length(rem.classes) != 0){
            rem.classes <- rem.classes[1]
            switch (rem.classes,
                    "user_gender" = {next.que <- "What is your gender?"},
                    "user_age" = {next.que <- "What is your age?"},
                    "query_cloth" = {next.que <- "What are you looking for?"})
          } 
        } else {
          next.que <- "Out of questions now... will send the query to RR..."
        }
        post.ans$next_question <- next.que # next question will go here ...
        
        # send request to rr
        post.ans$rr_data <- "NULL"
      }
    } else {
      
      # authentication failed
      post.ans <- "Authentication failure :("
    }
    
    res$write(toJSON(post.ans)) # response for the POST 
  }
  if(req$get()){
    
    res$write(toJSON("get requested")) # response for the GET
  }
  
  # delete the user history older than 5 mins
  chat.files <- list.files(pattern = ".chat")
  file.del <- lapply(chat.files, 
                     function(i.file){
                       dur <- (as.numeric(difftime(as.POSIXct(Sys.time()), 
                                                   as.POSIXct(file.mtime(i.file)),
                                                   units = "secs")))
                       if(dur > 5*60){
                         # delete the file ifits older th  5 mins
                         file.remove(i.file)
                       }
                     })
  
  ### finish it up
  res$finish()
}