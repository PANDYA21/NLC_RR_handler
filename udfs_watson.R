######################################################
### Natural Language Classifier - R Programming Language Interface
### Adapting the NLC APIs https://www.ibm.com/smarterplanet/us/en/ibmwatson/developercloud/apis/ to RCURL
######################################################

load_or_install("RCurl", repos='http://cran.rstudio.com/') # install.packages("RCurl") # if the package is not already installed
# load_or_install("httr", repos='http://cran.rstudio.com/')
# load_or_install("XML", repos='http://cran.rstudio.com/')
load_or_install("data.table", repos='http://cran.rstudio.com/')
# load_or_install("reshape2", repos='http://cran.rstudio.com/')
# load_or_install("tidyr", repos='http://cran.rstudio.com/')
# load_or_install("dplyr", repos='http://cran.rstudio.com/')
load_or_install("stringr", repos='http://cran.rstudio.com/')
load_or_install("splitstackshape", repos='http://cran.rstudio.com/')

# PROBLEM > If you get this > Error in function (type, msg, asError = TRUE)  :  SSL certificate problem: self signed certificate in certificate chain 
# SOLUTION then you need to do this > YOU MUST KEEP THIS IN FOR below to work To begin: this line sets CERT Global to make a CA Cert go away - http://stackoverflow.com/questions/15347233/ssl-certificate-failed-for-twitter-in-r
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"),httpauth=AUTH_BASIC)) # NOTE - the "httpauth=AUTH_BASIC" piece gets rid of the "Error: UNAUTHORIZED" message 


######### Housekeeping And Authentication 
######### Housekeeping And Authentication 
base_url = "https://gateway.watsonplatform.net/natural-language-classifier/api/v1/classifiers/"
# username = "1f8006fe-d57a-4201-93b6-0563b1573a67" # "abc123-####-####-YOUR-CREDS-HERE" #### BE VERY CAREFUL TO understand "Instantiating Credentials" from bound service vs "Service Credentials"
# password = "EhscvrMhWnzg" # "123456789ABC"  # you need your own ## if you are having authentication issues , may need the other creds.
# username_password = paste(username,":",password)
# 
# base_url <- "https://gateway.watsonplatform.net/natural-language-classifier/api"
password <- "byIZc0TJS1Bq"
username <- "8166a940-90d3-4f88-8a93-b76e20aa7152"
username_password = paste(username,":",password)

## Next - let's create all the functions (but not actually execute them just yet)

###### FUNCTION: LIST ALL CLASSIFIERS AND RETURN NEAT LIST
watson.nlc.listallclassifiers <- function(){ 
  data <- getURL(base_url,userpwd = username_password )
  data <- as.data.frame(strsplit(as.character(data),"classifier_id"))
  data <- data[-c(1), ] # remove dud first row
  data <- data.frame(matrix(data))
  colnames(data) <- "V1"
  data$V1 <- gsub("[{}]","", data$V1)
  data$V1 <- gsub("]","", data$V1)
  data$V1 <- gsub("\"","", data$V1)
  data$V1 <- gsub("name:","", data$V1)
  data$V1 <- gsub(":","", data$V1)
  data <- cSplit(data, 'V1', sep=",", type.convert=FALSE)
  data[,c(2,4)] <- NULL
  data <- as.data.table(data)
  setnames(data,c("classifier","name","date_created"))
  data <- data[order(date_created),] 
  return(data)
}


###### FUNCTION CREATE NEW CLASSIFIER - post /v1/classifiers - Creates a classifier with CSV data ## URL below no "/" after base url
watson.nlc.createnewclassifier <- function(file,classifiername) {
  return(POST(url="https://gateway.watsonplatform.net/natural-language-classifier/api/v1/classifiers",
              authenticate(username,password),
              body = list(training_data = upload_file(file),
                          training_metadata = paste("{\"language\":\"en\",\"name\":",classifiername,"}",sep="") 
              )))}
###### end of function


###### FUNCTION - CHECK CLASSIFIER STATUS
watson.nlc.checkclassifierstatus <- function(classifier_id) {
  return(
    getURL(paste(base_url,classifier_id,sep=""),userpwd = username_password)
  )
}
### end of function


###### FUNCTION - DELETE CLASSIFIER - Receives name of Classifier to Kill; May not be able to do this until training complete
watson.nlc.deleteclassifier <- function(kill_classifier) {
  return(DELETE(paste(base_url,kill_classifier,sep=""),
                userpwd = username_password)) }
### end of function


###### FUNCTION: ACCEPT QUERY & RETURN RESULT: CLASSIFIER and % FROM TEXT INPUT AND PROCESS TO LOOK GOOD
watson.nlc.processtextreturnclass <- function(classifier_id,query_text){
  query_text <- URLencode(query_text)
  data <- getURL(paste(base_url,classifier_id,"/classify","?text=", query_text,sep=""),userpwd = username_password)
  data <- as.data.frame(strsplit(as.character(data),"class_name"))
  data <- data[-c(1), ] # remove dud first row
  data <- gsub("[{}]","", data)
  data <- gsub("confidence","", data)
  data <- data.frame(matrix(data))
  setnames(data,("V1"))
  data$V1 <- gsub("\"","", data$V1)
  data$V1 <- gsub(":","", data$V1)
  data$V1 <- gsub("]","", data$V1)
  data <- cSplit(data, 'V1', sep=",", type.convert=FALSE)
  setnames(data,c("class","confidence"))
  return(data) }
### end of function


######################################################### END OF FUNCTION DECLARATIONS