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

getWatsonClass <- function(classifier, query){
  ans <- watson.nlc.processtextreturnclass(classifier, query)
  return(list("query" = query, ans[1,1, with=F], ans[1,2, with=F]))
}

getWatsonProfile <- function(classifier,user.txt){
  in.sentences <- unlist(strsplit(user.txt, "\\."))
  in.sentences <- in.sentences[!(in.sentences %in% " ")]
  in.sentences <- in.sentences[!(in.sentences %in% "")]
  ans.df <- lapply(in.sentences, function(xx){
      return(getWatsonClass(classifier, xx))
    })
  ans.df <- ldply(ans.df, data.frame)
  return(ans.df)
}


getUsername <- function(txt){
  remove.txt <- c("my", "surname", "name", "is", "i ", "am", "called", "full", "\\.", " ")
  txt.ans <- tolower(txt)
  for(xx in remove.txt){
    txt.ans <- gsub(xx, "", txt.ans)
  }
  return(txt.ans)
}


getDigits <- function(txt){
  return(as.numeric(unlist(str_extract_all(txt, "[:digit:]+"))))
}


getProfile <- function(classifier, in.txt){
  in.txt <- gsub("\\n", "", in.txt)
  ans.df <- getWatsonProfile(classifier, user.txt = in.txt)
  ans.name <- tail(as.character(ans.df[which(ans.df$class == "Username"),1]), 1)
  ans.name <- getUsername(ans.name)
  ans.age <- tail(as.character(ans.df[which(ans.df$class == "Age"),1]), 1)
  ans.age <- getDigits(ans.age)
  ans.df2 <- data.frame(matrix(nrow = 6, ncol = 2))
  names(ans.df2) <- c("class", "value")
  ans.df2$value <- rep("", nrow(ans.df2))
  ans.df2$class <- c("Username", "Age", "Player_level", "Player_type", "Stroke_type", "Common_error")
  # if(length(ans.df$confidence[grep("Username", ans.df$class)]) != 0){
  #   if(ans.df$confidence[grep("Username", ans.df$class)] > 0.5){
  #     ans.df2$value[ans.df2$class == "Username"] <- ans.name
  #   }
  # }
  # if(length(ans.df$confidence[grep("Age", ans.df$class)]) != 0){
  #   if(ans.df$confidence[grep("Age", ans.df$class)] > 0.5){
  #     ans.df2$value[ans.df2$class == "Age"] <- ans.age
  #   }
  # }
  # if(length(ans.df$confidence[grep("Ptype", ans.df$class)]) != 0){
  #   if(ans.df$confidence[grep("Ptype", ans.df$class)] > 0.5){
  #     ans.df2$value[ans.df2$class == "Player_type"] <- ans.df$class[grep("Ptype", ans.df$class)]
  #   }
  # }
  # if(length(ans.df$confidence[grep("Player_type", ans.df$class)]) != 0){
  #   if(ans.df$confidence[grep("Player_type", ans.df$class)] > 0.5){
  #     ans.df2$value[ans.df2$class == "Player_level"] <- ans.df$class[grep("Player_type", ans.df$class)]
  #   }
  # }
  # if(length(ans.df$confidence[grep("Stype", ans.df$class)]) != 0){
  #   if(ans.df$confidence[grep("Stype", ans.df$class)] > 0.5){
  #     ans.df2$value[ans.df2$class == "Stroke_type"] <- ans.df$class[grep("Stype", ans.df$class)]
  #   }
  # }
  # if(length(ans.df$confidence[grep("Err_type", ans.df$class)]) != 0){
  #   if(ans.df$confidence[grep("Err_type", ans.df$class)] > 0.5){
  #     ans.df2$value[ans.df2$class == "Common_error"] <- ans.df$class[grep("Err_type", ans.df$class)]
  #   }
  # }
  if(length(tail(ans.df$confidence[grep("Username", ans.df$class)], 1)) != 0){
    if(tail(ans.df$confidence[grep("Username", ans.df$class)], 1) > 0.5){
      ans.df2$value[ans.df2$class == "Username"] <- ans.name
    }
  }
  if(length(tail(ans.df$confidence[grep("Age", ans.df$class)], 1)) != 0){
    if(tail(ans.df$confidence[grep("Age", ans.df$class)], 1) > 0.5){
      ans.df2$value[ans.df2$class == "Age"] <- ans.age
    }
  }
  if(length(tail(ans.df$confidence[grep("Ptype", ans.df$class)], 1)) != 0){
    if(tail(ans.df$confidence[grep("Ptype", ans.df$class)], 1) > 0.5){
      ans.df2$value[ans.df2$class == "Player_type"] <- tail(ans.df$class[grep("Ptype", ans.df$class)], 1)
    }
  }
  if(length(tail(ans.df$confidence[grep("Player_type", ans.df$class)], 1)) != 0){
    if(tail(ans.df$confidence[grep("Player_type", ans.df$class)], 1) > 0.5){
      ans.df2$value[ans.df2$class == "Player_level"] <- tail(ans.df$class[grep("Player_type", ans.df$class)], 1)
    }
  }
  if(length(tail(ans.df$confidence[grep("Stype", ans.df$class)], 1)) != 0){
    if(tail(ans.df$confidence[grep("Stype", ans.df$class)], 1) > 0.5){
      ans.df2$value[ans.df2$class == "Stroke_type"] <- tail(ans.df$class[grep("Stype", ans.df$class)], 1)
    }
  }
  if(length(tail(ans.df$confidence[grep("Err_type", ans.df$class)], 1)) != 0){
    if(tail(ans.df$confidence[grep("Err_type", ans.df$class)], 1) > 0.5){
      ans.df2$value[ans.df2$class == "Common_error"] <- tail(ans.df$class[grep("Err_type", ans.df$class)], 1)
    }
  }
  return(ans.df2)
}


convertUserProfile <- function(user.df){
  anss <- lapply(1:nrow(user.df), 
                 function(ii){
                   switch(as.character(user.df$x2[ii]),
                          Username = {data.table("Username", getUsername(as.character(user.df$x1[ii])))},
                          Age = {data.table("Age", getDigits(as.character(user.df$x1[ii])))},
                          Player_type_hobby = {data.table("Player", "Hobby")},
                          Player_type_advanced = {data.table("Player", "Advanced")},
                          Player_type_beginner = {data.table("Player", "Beginner")},
                          Ptype_aggressive = {data.table("Type", "Aggressive")},
                          Ptype_defensive = {data.table("Type", "Defensive")},
                          Ptype_allround = {data.table("Type", "Allround")},
                          Stype_flat = {data.table("Stroke", "Flat")},
                          Stype_topspin = {data.table("Stroke", "Topspin")},
                          Stype_slice = {data.table("Stroke", "Slice")}, 
                          Err_type_hits = {data.table("Error", "Many frame hits")},
                          Err_type_balls = {data.table("Error", "Many balls out of bounds")},
                          Err_type_acc = {data.table("Error", "Low accuracy")})
                 })
  anss <- rbindlist(anss)
  return(anss)
}

getReply <- function(watson.class, user.prof, in.txt = in.txt1){
  # all.fields <-c("Username", "Age", "Player_type_advanced", "Player_type_hobby", 
  #                "Player_type_beginner", "Ptype_aggressive", "Ptype_defensive", 
  #                "Ptype_allround", "Stype_topspin", "Stype_slice", "Stype_flat", 
  #                "Err_type_ball", "Err_type_hits", "Err_type_acc")

  # switch(watson.class,
  #        Username = {getUsername(as.character(in.txt))},
  #        Age = {getDigits(as.character(in.txt))},
  #        Player_type_hobby = {"Hobby"},
  #        Player_type_advanced = {"Advanced"},
  #        Player_type_beginner = {"Beginner"},
  #        Ptype_aggressive = {"Aggressive"},
  #        Ptype_defensive = {"Defensive"},
  #        Ptype_allround = {"Allround"},
  #        Stype_flat = {"Flat"},
  #        Stype_topspin = {"Topspin"},
  #        Stype_slice = {"Slice"}, 
  #        Err_type_hits = {"Many frame hits"},
  #        Err_type_balls = {"Many balls out of bounds"},
  #        Err_type_acc = {"Low accuracy"})
  
  # all fields of user profile
  req.fields <- c("Username", "Age", "Player", "Type", "Stroke", "Error")
  rem.fields <- c(req.fields[!req.fields %in% user.prof$V1])
  rem.fields <- rem.fields[!rem.fields %in% as.character(convertUserProfile(data.frame(x1 = in.txt, x2 = watson.class))[,1,with=F])]
  if(length(rem.fields) == 0){
    return("Cool! Move to next step...")
  }
  rpl.txt <- HTML(paste0("You can tell me about your ", paste0(rem.fields, collapse = ", "),
                    ", if you know.", as.character(br()), "You can type '@Ex' for example."))
  return(rpl.txt)
  
}


separateSentences <- function(user.txt){
  in.sentences <- unlist(strsplit(user.txt, "\\."))
  in.sentences <- in.sentences[!(in.sentences %in% " ")]
  in.sentences <- in.sentences[!(in.sentences %in% "")]
  return(in.sentences)
}

cdf <- function(vect){
  return(unlist(lapply(1:length(vect), 
                       function(ii){
                         return(sum(vect[1:ii]))
                       })))
}

keepLastRowInDf <- function(usr.df){
  usr.df <- data.frame(usr.df)
  # names(usr.df) <- c("x1", "x2")
  rec <- rle(as.character(usr.df$V1))
  usr.df <- usr.df[cdf(rec$lengths),]
  rownames(usr.df) <- NULL
  return(usr.df)
}


