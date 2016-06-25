### PreReqs, UDFs and settings for the app

## the very first function to install or load namespaces
load_or_install <- function(func = "", ...){
  if(require(package =  func, character.only = T) == FALSE){
    install.packages(func, clean = T, ...)
    require(package =  func, character.only = T)
  } else {
    require(package =  func, character.only = T)
  }
}
##


## Load libs
load_or_install("httr", repos='http://cran.rstudio.com/')
##


## get the next question based on current chat
getNextQue <- function(chatt) {
  # # look for existing data and send next question
  # asked <- unlist(lapply(strsplit(chatt, ":"), function(x) x[2]))
  # asked <- asked[!is.na(asked)]
  # if(length(asked) < 3 | length(asked) == 0){
  #   
  #   ## ask the remaining questions
  #   # all.classes <- c("user_gender", "user_age", "query_cloth")
  #   all.classes <- as.character(read.csv2("train_data_nlc_new.csv")[,4]) # or [,3]
  #   rem.classes <- all.classes[!(all.classes %in% asked)]
  #   
  #   if(length(rem.classes) != 0){
  #     rem.classes <- rem.classes[1]
  #     switch (rem.classes,
  #             "user_gender" = {next.que <- "What is your gender?"},
  #             "user_age" = {next.que <- "What is your age?"},
  #             "query_cloth" = {next.que <- "What are you looking for?"})
  #   } 
  # } else {
  #   next.que <- "Out of questions now... will send the query to RR..."
  # }
  # return(next.que)
  
  # new classes
  # look for existing data and send next question
  asked <- unlist(lapply(strsplit(chatt, ":"), function(x) x[2]))
  asked <- asked[!is.na(asked)]
  asked <- unique(asked)
  if(length(asked) < 9 | length(asked) == 0){
    ## ask the remaining questions
    all.classes <- c("query_cloth", "user_gender", "user_age", "style",
                     "brand", "color", "price", "material", "climate")
    rem.classes <- all.classes[-c(1:length(asked))]
    if(length(rem.classes) != 0){
      rem.classes <- rem.classes[1]
      switch (rem.classes,
              "user_gender" = {next.que <- "What is your gender?"},
              "user_age" = {next.que <- "What is your age?"},
              "query_cloth" = {next.que <- "What are you looking for?"}, 
              "style" = {next.que <- "Are you looking for any specific styles?"}, 
              "brand" = {next.que <- "Do you have any preferences for brands?"}, 
              "color" = {next.que <- "Are you looking for a specific color?"}, 
              "price" = {next.que <- "Do you have any price criteria?"}, 
              "material" = {next.que <- "Any additional material details?"}, 
              "climate" = {next.que <- "What season are you going to wear it in?"})
    } else {
      next.que <- ""
    }
  } else {
    next.que <- ""
  }
  return(next.que)
}

## send a query to RR and get catentry.ids in response
getRRresp <- function(query_to_rr){
  uname_rr <- "8c676d2c-38a7-4604-b4a3-3849f51e84a6 : Ph4ZKUhXxsAo"
  ranker_id <- "3b140ax15-rank-3562"
  # example query url
  # url_rr <- "https://gateway.watsonplatform.net/retrieve-and-rank/api/v1/solr_clusters/scb7c0999c_d827_4d15_b305_2ddbf02fcd44/solr/example_collection/select?q=shortDescription:male_jackets_black_leather&wt=json"
  query_url_rr <- paste0("https://gateway.watsonplatform.net/retrieve-and-rank/api/v1/solr_clusters/scb7c0999c_d827_4d15_b305_2ddbf02fcd44/solr/example_collection/select?q=", query_to_rr, "&wt=json")
  anss <- fromJSON(getURL(query_url_rr, userpwd = uname_rr))
  if(anss$responseHeader$status == 400){
    catentry.ids <- list("error_code_400")
  } else if(anss$responseHeader$status == 0) {
    if(length(anss$response$docs) != 0){
      catentry.ids <- lapply(1:length(anss$response$docs), function(ii) anss$response$docs[[ii]]$catentry_id)
    } else {
      catentry.ids <- list()
    }
  } else {
    catentry.ids <- "Something went wrong..."
  }
  return(catentry.ids)
}


## get gender
getGender <- function(user.txt){
  if(grepl("female|woman", tolower(user.txt))){
    return("female")
  } else if (grepl("male|man", tolower(user.txt))) {
    return("male")
  }
}


## get one of the four categories: boy, girl, man, woman
getBGMW <- function(user_age, user_gender){
  user_gender <- tolower(user_gender)
  switch(user_gender,
         "male" = {
           if(user_age <= 15){
             return("boy")
           } else {
             return("man")
           }
         },
         "female" = {
           if(user_age <= 15){
             return("girl")
           } else {
             return("woman")
           }
         },
         "man" = {
           if(user_age <= 15){
             return("boy")
           } else {
             return("man")
           }
         },
         "woman" = {
           if(user_age <= 15){
             return("girl")
           } else {
             return("woman")
           }
         },
         "boy" = {
           if(user_age <= 15){
             return("boy")
           } else {
             return("man")
           }
         },
         "girl" = {
           if(user_age <= 15){
             return("girl")
           } else {
             return("woman")
           }
         })
}


# correct the chat with extracted keywords
corChatt <- function(user.txt, class.ans){
  if(length(class.ans) != 0){
    if(class.ans %in% c("user_gender", "user_age")){
      switch (class.ans,
              "user_age" = {return(getDigits(user.txt))},
              "user_gender" = {return(getGender(user.txt))}
      )
    } else {
      return(user.txt)
    }
  }
}

getKeywords <- function(answers, asked){
  key.words <- unlist(lapply(1:length(answers), function(ii) corChatt(answers[ii], asked[ii])))
  index.gen <- tail(which(asked == "user_gender"), 1)
  index.age <- tail(which(asked == "user_age"), 1)
  new.cat <- getBGMW(key.words[index.age], key.words[index.gen])
  key.words <- c(new.cat, key.words[-c(index.age, index.gen)])
  return(key.words)
}


