sink(stderr())
print("Running init.R Bbb...")

install.packages("Rcpp", type="source", dependencies=TRUE, repos='http://cran.rstudio.com/')

# shiny and dashboard
install.packages("Rook",clean=T, dependencies=TRUE, repos='http://cran.rstudio.com/')

# other packages
## the very first function to install or load namespaces
load_or_install <- function(func = "", ...){
  if(require(package =  func, character.only = T) == FALSE){
    install.packages(func, clean = T, ...)
    require(package =  func, character.only = T)
  } else {
    require(package =  func, character.only = T)
  }
}
###

## Load libs
load_or_install("RCurl", repos='http://cran.rstudio.com/')
load_or_install("reshape2", repos='http://cran.rstudio.com/')
load_or_install("stringr", repos='http://cran.rstudio.com/')
load_or_install("rjson", repos='http://cran.rstudio.com/')
load_or_install("httr", repos='http://cran.rstudio.com/')
load_or_install("XML", repos='http://cran.rstudio.com/')
load_or_install("data.table", repos='http://cran.rstudio.com/')
load_or_install("tidyr", repos='http://cran.rstudio.com/')
load_or_install("dplyr", repos='http://cran.rstudio.com/')
load_or_install("stringr", repos='http://cran.rstudio.com/')
load_or_install("splitstackshape", repos='http://cran.rstudio.com/')

print("All packages installed...")
