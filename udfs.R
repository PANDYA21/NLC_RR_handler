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

