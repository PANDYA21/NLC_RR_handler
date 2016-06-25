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
  # look for existing data and send next question
  asked <- unlist(lapply(strsplit(chatt, ":"), function(x) x[2]))
  asked <- asked[!is.na(asked)]
  if(length(asked) < 3 | length(asked) == 0){
    
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
  return(next.que)
}

## send a query to RR and get catentry.ids in response
getRRresp <- function(query_to_rr){
  uname_rr <- "8c676d2c-38a7-4604-b4a3-3849f51e84a6 : Ph4ZKUhXxsAo"
  ranker_id <- "3b140ax15-rank-3562"
  # example query url
  # url_rr <- "https://gateway.watsonplatform.net/retrieve-and-rank/api/v1/solr_clusters/scb7c0999c_d827_4d15_b305_2ddbf02fcd44/solr/example_collection/select?q=shortDescription:male_jackets_black_leather&wt=json"
  query_url_rr <- paste0("https://gateway.watsonplatform.net/retrieve-and-rank/api/v1/solr_clusters/scb7c0999c_d827_4d15_b305_2ddbf02fcd44/solr/example_collection/select?q=", "shortDescription:", query_to_rr, "&wt=json")
  anss <- fromJSON(getURL(query_url_rr, userpwd = uname_rr))
  if(anss$responseHeader$status == 400){
    catentry.ids <- list("error_code_400")
  } else if(anss$responseHeader$status == 0) {
    catentry.ids <- lapply(1:length(anss$response$docs), function(ii) anss$response$docs[[ii]]$catentry_id)
  } else {
    catentry.ids <- "Something went wrong..."
  }
  return(catentry.ids)
}

