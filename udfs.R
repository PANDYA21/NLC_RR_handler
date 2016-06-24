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
load_or_install("shiny", repos='http://cran.rstudio.com/')
load_or_install("shinydashboard", repos='http://cran.rstudio.com/')
load_or_install("ggplot2", repos='http://cran.rstudio.com/') 
# load_or_install("knitr", repos='http://cran.rstudio.com/')
load_or_install("data.table", repos='http://cran.rstudio.com/')
load_or_install("httr", repos='http://cran.rstudio.com/')
load_or_install("xts", repos='http://cran.rstudio.com/')

##

## global variables
sub.val1 <<- 0
Logged <<- FALSE
tex <<- "tex"
##

## algorithm parameter values
# read the values and set them to default
oparams.df <- read.csv("Algorithm_parameters/Oparams.csv")
Cscale.default <<- oparams.df$cscale # 10
Cwght.default <<- oparams.df$cwght # 0.1
Cwght2.default <<- oparams.df$cwght2 # 0.07
##

## source UIs
source("dialog_page.R")
source("table_plot_pages.R")
source("admin_page.R")
source("root_page.R")
options(bitmapType='cairo')
##

# read the user names and password .csv file
credentials <<- read.csv(file = "users.csv", stringsAsFactors = F)
my_username <<- tolower(credentials$username)
my_password <<- credentials$password
#

# read the current boundry values from DB
bvalues.df <<- read.csv(file = "boundry_values.csv", stringsAsFactors = F)
#

# load the racket database
load("prereqs.RData", envir = .GlobalEnv)

# # load the new parameter tables from csv files
# heads <- read.csv2("heads.csv", row.names = F)
# wghts <- read.csv2("heads.csv", row.names = F)
# string <- read.csv2("heads.csv", row.names = F)
# Rdb <- read.csv2("racketDB.csv", row.names = F)


# translate headsize data "points" variable to "player-type" variable values
heads$points[which(heads$points == "0-5 points")] <- "beginner"
heads$points[which(heads$points == "6-10 points")] <- "hobby"
heads$points[which(heads$points == "11-15 points")] <- "LK 1-8"
heads$points[which(heads$points == "16-20 points")] <- "LK 9-16"
heads$points[which(heads$points == "> 20 points")] <- "LK 17-23"

## to update the variables for user input dialog chat globally
updateDialogVars <- function(){
  # the variables to be used during user profiling
  age1 <<- as.numeric(as.character(bvalues.df[1,1]))
  age2 <<- as.numeric(as.character(bvalues.df[1,2]))
  age3 <<- as.numeric(as.character(bvalues.df[1,3]))
  plevel1 <<- as.character(bvalues.df[2,1])
  plevel2 <<- as.character(bvalues.df[2,2])
  plevel3 <<- as.character(bvalues.df[2,3])
  plevel4 <<- as.character(bvalues.df[2,4])
  plevel5 <<- as.character(bvalues.df[2,5])
  ptype1 <<- as.character(bvalues.df[3,1])
  ptype2 <<- as.character(bvalues.df[3,2])
  ptype3 <<- as.character(bvalues.df[3,3])
  stype1 <<- as.character(bvalues.df[4,1])
  stype2 <<- as.character(bvalues.df[4,2])
  stype3 <<- as.character(bvalues.df[4,3])
  wght1 <<- as.numeric(as.character(bvalues.df[5,1]))
  wght2 <<- as.numeric(as.character(bvalues.df[5,2]))
  head1 <<- as.numeric(as.character(bvalues.df[6,1]))
  head2 <<- as.numeric(as.character(bvalues.df[6,2]))
}
updateDialogVars()
##

## to update the variables for algorithm parameters globally
updateOparamVars <- function(){
  oparams.df <- read.csv("Algorithm_parameters/Oparams.csv")
  Cscale.default <<- oparams.df$cscale # 10
  Cwght.default <<- oparams.df$cwght # 0.1
  Cwght2.default <<- oparams.df$cwght2 # 0.07
}
##

## the match function
getFinDf <- function(In.age, In.sex, In.weight, In.lev, In.ptype, In.err, In.head, In.stroke,
                     Cscale = Cscale.default, Cwght = Cwght.default, Cwght2 = Cwght2.default){
  
  ################
  if(In.sex == ""){
    In.sex <- "male"
  } else {
    In.sex <- tolower(In.sex)
  }
  
  # print(In.age)
  if(is.na(as.numeric(In.age))){
    In.age <- "13-18"
  } else {
    if(as.numeric(In.age) <= 18){
      In.age <- "13-18"
    } else if(as.numeric(In.age) > 18 & as.numeric(In.age) <= 35){
      In.age <- "19-35"
    } else if(as.numeric(In.age) > 36 & as.numeric(In.age) <= 60){
      In.age <- "36-60"
    } else if(as.numeric(In.age) > 60){
      In.age <- "over 60"
    }
  }
  
  if (In.lev == ""){
    In.lev <- "beginner"
  } else if(grepl("hobby", In.lev, ignore.case = T)){
    In.lev <- tolower("hobby")
  } else if(grepl("lk[ ]*1-8", In.lev, ignore.case = T)){
    In.lev <- "LK 1-8"
  } else if(grepl("lk[ ]*9-16", In.lev, ignore.case = T)){
    In.lev <- "LK 9-16"
  } else if(grepl("lk[ ]*17-23", In.lev, ignore.case = T)){
    In.lev <- "LK 17-23"
  } 
  
  if(In.ptype == ""){
    In.ptype <- "aggressive"
  } else if(grepl("know", In.ptype, ignore.case = T)){
    In.ptype <- "aggressive"
  } else{
    In.ptype <- tolower(In.ptype)
  }
  
  if(In.stroke == ""){
    In.stroke <- "topspin"
  } else if(grepl("know", In.stroke, ignore.case = T)){
    In.stroke <- "topspin"
  } else{
    In.stroke <- tolower(In.stroke)
  }
  
  if(In.err == ""){
    In.err <- "too low accuracy"
  } else if(grepl("know", In.err, ignore.case = T)){
    In.err <- "too low accuracy"
  } else if(grepl("out[ ]*of[ ]*bounds", In.err, ignore.case = T)) {
    In.err <- tolower("too many balls out of bounds")
  } else if(grepl("low[ ]*swing", In.err, ignore.case = T)){
    In.err <- tolower("too low swing speed")
  } else if(grepl("many[ ]*frame[ ]*hits", In.err, ignore.case = T)){
    In.err <- tolower("too many frame hits")
  } else if(grepl("low[ ]*accuracy", In.err, ignore.case = T)){
    In.err <- tolower("too low accuracy")
  }
  
  if (grepl("no|No|NO|nO", In.weight, ignore.case = T)){
    In.weight <- "middle (260-300g)"
  } else{
    if(is.na(as.numeric(In.weight))){
      In.weight <- "middle (260-300g)"
    } else{
      if (as.numeric(In.weight) <= 260){
        In.weight <- "light (under 260g)"#,	"middle (260-300g)", "heavy (over 300g)"
      } else if (as.numeric(In.weight) > 260 & as.numeric(In.weight) <= 300){
        In.weight <- "middle (260-300g)"
      } else if (as.numeric(In.weight) > 300){
        In.weight <- "heavy (over 300g)"
      } 
    }
  }
  
  if(In.head == ""){
    In.head <- "middle (630-660 cm²)"
  } else if (is.na(as.numeric(In.head))){
    if (grepl("no|No|NO|nO", In.head)){
      In.head <- "middle (630-660 cm²)"
    } 
  } else if (as.numeric(In.head) <= 630){
    In.head <- "small (under 630 cm²)"# , "middle (630-660 cm²)", "large (over 660 cm²)"
  } else if (as.numeric(In.head) > 630 & as.numeric(In.head) <= 660){
    In.head <- "middle (630-660 cm²)"
  } else if (as.numeric(In.head) > 660){
    In.head <- "large (over 660 cm²)"
  } 
  ################
  
  
  # weight recommendation
  wghts.op <<- wghts$Output[which((wghts$Age == In.age) & (wghts$Gender == In.sex) & 
                                    (wghts$Weight == unlist(strsplit(In.weight, " "))[1]) & (wghts$Play.level == In.lev))]
  if(wghts.op == "light"){
    wghts.val <<- mean(Rdb$Weight)-sd(Rdb$Weight)
  } else if(wghts.op == "heavy"){
    wghts.val <<- mean(Rdb$Weight)+sd(Rdb$Weight)
  } else {
    wghts.val <<- mean(Rdb$Weight)
  }
  
  # headsize recommendation
  heads.op <<- heads$Output[which((heads$Play.level == In.lev) & (heads$Common.error == In.err) & 
                                    (heads$Prefered.head.size == unlist(strsplit(In.head, " "))[1]) & (heads$points == In.lev))]
  if(heads.op == "small"){
    heads.val <<- mean(Rdb$Headsize)-sd(Rdb$Headsize)
  } else if(heads.op == "large"){
    heads.val <<- mean(Rdb$Headsize)+sd(Rdb$Headsize)
  } else {
    heads.val <<- mean(Rdb$Headsize)
  }
  
  # stringsize rec ommendation
  string.op <<- string$Output[which((string$Play.style == In.ptype) & (string$Stroke.style == In.stroke) &
                                      (string$Common.error == In.err))]
  
  # ### calculating average match for the inputs
  # ## headsize
  # norm.head <<- 1-((abs(Rdb$Headsize-heads.val)/10)^1.5)/10
  # ## weight
  # norm.weight <<- 1-((abs(Rdb$Weight-wghts.val)/10)^1.5)*0.07
  # ## string
  # norm.string <<- 1-(abs(Rdb$Mains-string.op))*0.07
  # ## average of the three
  # mean.match <<- (norm.head+norm.weight+norm.string)/3
  
  ### calculating average match for the inputs
  ## headsize
  norm.head <<- 1-((abs(Rdb$Headsize-heads.val)/Cscale)^1.5)*Cwght
  ## weight
  norm.weight <<- 1-((abs(Rdb$Weight-wghts.val)/Cscale)^1.5)*Cwght2
  ## string
  norm.string <<- 1-(abs(Rdb$Mains-string.op))*0.07
  ## average of the three
  mean.match <<- (norm.head+norm.weight+norm.string)/3
  
  # fin.df <<- data.frame("Value" = c(wghts.op, wghts.val, heads.op, heads.val, string.op, sort(mean.match, decreasing = T)[1:5]), 
  #                       row.names = c("Weight category", "Recommended weight", "Headsize category", "Recommended headsize",
  #                                     "Recommended string", Rdb$Model[which(mean.match %in% sort(mean.match, decreasing = T)[1:5])]), 
  #                       stringsAsFactors = F)
  fin.df <<- data.frame("Value" = c(wghts.op, wghts.val, heads.op, heads.val, string.op, sort(mean.match, decreasing = T)), 
                        row.names = c("Weight category", "Recommended weight", "Headsize category", "Recommended headsize",
                                      "Recommended string", Rdb$Model[order(mean.match, decreasing = T)]), 
                        stringsAsFactors = F)
  return(fin.df)
}


### the plot function
getGgTradeOff <- function(s2 = s2Func(), DF2 = getMatch(), zoom.in = TRUE){
  #### sort norm.head and the others by rank of match
  norm.head <- norm.head[order(mean.match, decreasing = T)]
  norm.weight <- norm.weight[order(mean.match, decreasing = T)]
  norm.string <- norm.string[order(mean.match, decreasing = T)]
  
  #### gg section
  theDF <- data.frame(DF2[s2,], norm.head[as.numeric(s2)]*100,
                      norm.string[as.numeric(s2)]*100, norm.weight[as.numeric(s2)]*100)
  # theDF[5,] <- c(NA, 1:4)
  names(theDF)[2:5] <- c("match", "head", "string", "weight")
  the.DF <- data.frame("Racket" = rep(NA, nrow(theDF)*3),
                       "Match" = rep(NA, nrow(theDF)*3),
                       "yAxis" = rep(NA, nrow(theDF)*3),
                       "fac" = rep(NA, nrow(theDF)*3))
  the.DF$Racket <- rep(theDF$Racket, 3)
  the.DF$Match <- rep(theDF$match, 3)
  the.DF$yAxis <- c(theDF$head, theDF$string, theDF$weight)
  the.DF$fac <- rep(names(theDF)[3:5], each = nrow(theDF))
  
  if(nrow(the.DF) != 0){
    the.DF$fac <- names(theDF)[3:5]
    the.DF$Match <- c(0,50,100)
    
    ## correction of data points places
    data.points <- data.frame(matrix(unlist(lapply(as.numeric(s2), 
                                                    function(ii){
                                                      ax1 <- norm.head[ii]
                                                      ax2 <- norm.weight[ii]
                                                      ax3 <- norm.string[ii]
                                                      
                                                      x.dat <- c(ax1*0.5, 1-(ax2*0.5), 0.5)
                                                      # x.axis <- c(x.axis, x.dat)
                                                      y.dat <- c(ax1*0.5, ax2*0.5, 1-(ax3*0.5))
                                                      # y.axis <- c(y.axis, y.dat)
                                                      
                                                      return(c(sum(x.dat)/3, sum(y.dat)/3))
                                                    })), ncol = 2, byrow = T))
    x.ax <- data.points[,1]*100
    y.ax <- 100/3 + 2*((data.points[,2]/(0.5*3))*100 - 100/3) # (data.points[,2]/(0.5*3))*100
    ##
    
    # add the perfact match point to the plot
    x.ax <- c(x.ax, 50)
    y.ax <- c(y.ax, 100/3)
    
    # zoom the plot according to the poits' scatteredness
    if(zoom.in == TRUE){
      max.y <- max(abs(y.ax-33))
      max.x <- max(abs(x.ax-50))
      # print(max(max.x, max.y))
      zoom.fac <- 2.5 # (max(max.x, max.y))*2 # 5
      zoom.crit <- roundMe(max(max.x, max.y)*zoom.fac)
      if(zoom.crit > zoom.fac*10){
        # difference is more than 10 points
        # so dont zoom in 
      } else {
        # difference is less than 10 data points, so zoom in
        x.ax <- (x.ax - 50)*zoom.crit + 50
        y.ax <- (y.ax - 100/3)*zoom.crit + 100/3
      }
    }
    #
    
    # for triangle
    tri.df <- data.frame("xx" = c(0,50,100), "yy" = c(0,100,0)) 
    tri.df2 <- data.frame("xx" = c(0,50,50,100), "yy" = c(0,100,33.33,0))
    tribase.df <- data.frame("x2" = c(0:100), "y2" = rep(0,101))
    tritext.df <- data.frame("xx" = c(-13,50,113), "yy" = c(0,113,0))
    #
    
    Selected_Rackets <- c(the.DF$Racket[1:(nrow(the.DF)/3)], "Ideal Match")
    gg3 <- ggplot() + 
      geom_line(data = tri.df, mapping = aes(x = xx, y = yy), color = "lightblue", size = 2) +
      geom_line(data = tribase.df, mapping = aes(x = x2, y = y2), color = "lightblue", size = 2) +
      geom_point(data = tri.df2, mapping = aes(x = xx, y = yy), colour = "blue", shape=21,
                 size = 7, fill = "white", stroke = 2) +
      geom_text(data = tritext.df, mapping = aes(x = xx, y = yy, label = c("Head", "String", "Weight" )),
                colour = "black", size = 5) +
      geom_point(data = data.frame(x.ax, y.ax), 
                 aes(x = x.ax, y = y.ax, colour = Selected_Rackets), 
                 size = 5) +
      xlim(-15,115) + ylim(0,115) + 
      theme_bw() + theme(axis.line = element_blank(), #element_line(colour = "black"),
                         axis.ticks = element_blank(),
                         axis.text = element_blank(),
                         panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(),
                         panel.border = element_blank() )+ #, panel.background = element_blank()) +
      xlab("") + ylab("") +
      theme(
        #           panel.background = element_rect(fill = "#ecf0f5",colour = "#ecf0f5"), # or theme_blank() for transperency
        #           panel.grid.minor = element_blank(), 
        #           panel.grid.major = element_blank(),
        #           plot.background = element_rect(fill = "#ecf0f5",colour = "#ecf0f5"),
        #           legend.background = element_rect(fill = "#ecf0f5",colour = "#ecf0f5")
        # panel.background = element_rect(fill = "#ffffff",colour = "#ffffff"), # or theme_blank() for transperency
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        # plot.background = element_rect(fill = "#ffffff",colour = "#ffffff"),
        panel.background = element_rect(fill = "#f3f3f3",colour = "#f3f3f3"), # or theme_blank() for transperency
        plot.background = element_rect(fill = "#f3f3f3",colour = "#f3f3f3"),
        legend.background = element_rect(fill = "#f3f3f3",colour = "#f3f3f3")
      ) 
    return(gg3)
  }
}


### the user plot functions
getUserDetails <- function(){
  usrs.csv <- list.files(pattern = "*.csv")
  usrs.csv <- usrs.csv[grep("history", usrs.csv)] # usrs.csv[usrs.csv != "users.csv"]
  return(usrs.csv)
}

roundMe <- function(x){
  return(round((x*10+4)/10))
}

getGgUser <- function(fi.les = getUserDetails()){
  library(scales)
  # test.track <- rbindlist(lapply(fi.les, read.csv), fill = T)
  tp <- lapply(fi.les, read.csv)
  test.track <- lapply(tp, 
                       function(x){
                         anss <- data.frame(apply.daily(xts(rep(1, nrow(x)), 
                                                    order.by = as.Date(x$time_stamp)), 
                                                FUN = sum))
                         names(anss) <- "Count"
                         return(anss)
                         })
  test.track <- lapply(1:length(test.track), 
                       function(ii){
                         test.track[[ii]]$Usr <- rep(gsub("_history.csv", "", fi.les[ii]), nrow(test.track[[ii]]))
                         return(test.track[[ii]])
                       })
  test.track <- do.call(rbind, test.track)
    
  # gg4 <- ggplot(data = test.track) + geom_bar(mapping = aes(x = as.POSIXct(test.track$time_stamp)), 
  #              stat = "bin", binwidth = 10000, fill = "white") + 
  #   theme_bw() + scale_x_datetime(breaks = date_breaks("2 day"), labels = date_format("%d/%m")) + 
  #   xlab("Time") + ggtitle("") + facet_wrap(~Usr, ncol = roundMe(length(unique(test.track$Usr))/2), scales = "free_y") + 
  #   theme(axis.text.x=element_text(angle=90, vjust = 0.5), panel.grid.minor = element_blank(),
  #         panel.background = element_rect(fill = "turquoise"))
  gg4 <- ggplot(data = test.track) + geom_bar(mapping = aes(x = as.Date(rownames(test.track)), 
                                                            y = Count), 
                                              stat = "identity", width = 0.5, fill = "white") + 
    theme_bw() + scale_x_date(breaks = date_breaks("2 day"), labels = date_format("%d/%m")) + 
    xlab("Time") + ggtitle("") + facet_wrap(~Usr, ncol = roundMe(length(unique(test.track$Usr))/2), scales = "free") + 
    theme(axis.text.x=element_text(angle=90, vjust = 0.5), panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(), panel.background = element_rect(fill = "grey"),
          strip.background = element_rect(fill="black", colour = "black"), 
          strip.text = element_text(colour = "white")) + 
  geom_line(aes(x = as.Date(rownames(test.track)), y = mean(Count)),
             color = "white", linetype = 2, size = 1.2)
  return(gg4)
}



### additional funcitons
## to fill the DF with unequal vector sizes
fill.nas <- function(x, maxlen = length(x)){
  return(c(x, rep(NA, maxlen-length(x))))
}
fill.df <- function(...){
  max.len <- max(unlist(lapply(list(...), length)))
  return(data.frame(sapply(list(...), fill.nas, max.len, USE.NAMES = T) ))# , stringsAsFactors = F))
}
