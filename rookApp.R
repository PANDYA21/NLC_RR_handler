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
source("udfs.R")

rookApp <- function(env) {
  req <- Request$new(env)
  res <- Response$new(headers=list("Content-Type"="application/json" ,
                                   "Access-Control-Allow-Origin"="*", # "http://nlc-rr-handler.eu-gb.mybluemix.net/", #"*",
                                   "access-control-allow-credentials"="TRUE",
                                   "Access-Control-Allow-Methods"="GET, POST, HEAD, PUT",
                                   "Access-Control-Allow-Headers"="X-Requested-With, Content-Type"
                                   ))
  # res <- Response$new()
  if(req$post()){
    
    post <- req$POST() # the input from user
    post <- names(post)
    # cat(post)
    post <- fromJSON(post)
    
    if(post$user_pass == user_pass.api){
      
      # authentication successful
      # if conv_id is null start a new chat
      if(tolower(post$conv_id) == "start" | tolower(post$conv_id) == "null" ){
        
        # start a new chat
        chat_id <- sample(c(10000:50000), 1)
        post.ans <- list()
        post.ans$conv_id <- chat_id
        rm(chat_id)
        
        # send the user input to NLC
        nlc.res <- watson.nlc.processtextreturnclass(classifier, as.character(post$query))
        class.ans <- nlc.res$class[nlc.res$confidence > 0.75]
        # if NLC couldnt understand...
        if(length(class.ans) == 0){
          didnotget.txt <- sample(c("Could not understand that... ", 
                                    "Did not get that... ",
                                    "I did not understand that... ",
                                    "I am afraid I did not understand... ",
                                    "I am sorry that i could not understand... "), 1)
        } else {
          didnotget.txt <- character(0)
        }
        
        # save chat history for this conversation
        chatt <- paste0(as.character(post$query), ":", class.ans)
        writeLines(text = chatt, 
                   con = paste0(as.character(post.ans$conv_id), ".chat"))
        
        # # look for existing data and send next question
        post.ans$next_question <- paste0(didnotget.txt, getNextQue(chatt))
        
        # send request to rr (in fact, dont, since first question)
        post.ans$rr_data <- "NULL"
        
      } else {
        
        # continue older chat
        if(file.exists(paste0(post$conv_id, ".chat"))){
          
          # read the chat history for the given chat id
          chatt <- readLines(con = paste0(post$conv_id, ".chat"))
        } else {
          
          # user history not found, create a new file
          chatt <- character(0)
        }
        
        # continue the chat
        post.ans <- list()
        post.ans$conv_id <- post$conv_id
        
        # send the user input to NLC
        nlc.res <- watson.nlc.processtextreturnclass(classifier, as.character(post$query))
        class.ans <- nlc.res$class[nlc.res$confidence > 0.75]
        # if NLC couldnt understand...
        if(length(class.ans) == 0){
          didnotget.txt <- sample(c("Could not understand that... ", 
                                    "Did not get that... ",
                                    "I did not understand that... ",
                                    "I am afraid I did not understand... ",
                                    "I am sorry that i could not understand... "), 1)
        } else {
          didnotget.txt <- character(0)
        }
        
        # save chat history for this conversation
        chatt <- c(chatt, paste0(as.character(post$query), ":", class.ans))
        writeLines(text = chatt, con = paste0(as.character(post$conv_id), ".chat"))
        
        # # look for existing data and send next question
        post.ans$next_question <- paste0(didnotget.txt, getNextQue(chatt))
        
        ## send request to rr (send after three accepted answers)
        asked <- unlist(lapply(strsplit(chatt, ":"), function(x) x[2]))
        # accept the answers that NLC was able to classify
        answers <- chatt[!is.na(asked)]
        asked <- asked[!is.na(asked)]
        answers <- unlist(lapply(strsplit(answers, ":"), function(x) x[1]))
        if(length(answers) >= 3){
          query_to_rr <- paste(getKeywords(answers, asked), collapse = "%20")
          # query_to_rr <- paste(answers, collapse = "%20")
          query_to_rr <- gsub("\ ", "%20", query_to_rr) # the proper solution after 'shortDescription' is solved
          # query_to_rr <- gsub("\ ", "_", query_to_rr)
          print(query_to_rr)
          post.ans$rr_data <- getRRresp(query_to_rr)
        } else {
          post.ans$rr_data <- "NULL"
        }
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