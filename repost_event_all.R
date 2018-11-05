library(tidyverse) # load libraries
library(dplyr)
library(quantmod)
library(ggpubr)
library(cowplot)
library(lubridate)
library(igraph)
library(network)
library(sna)
library(stringr)
library(reshape)
library(gridExtra) # when grid.arrange does not work, add the library to fix it
library(data.table) # when as.data.table does not work
rm(list=ls()) # clear variables and functions

list_id <- c("5781292544", "5781311106", "6290114447", "5908064369", "6292617784",
             "5780592127", "6015189526", "6224699669", "5902696506", "6177367279") # user ids of the trainees
folder <- "/Users/xingruchen/Dropbox/Sina_weibo/data/" # path to folder that holds multiple .csv files
folder_f <- "/Users/xingruchen/Dropbox/Sina_weibo/manuscript/figure/" # path to folder that holds multiple figures
folder_d <- "/Users/xingruchen/Dropbox/Sina_weibo/data/R/diff/" # path to folder that holds multiple .csv files


interval <- c(days(1), days(3), days(7), days(14), days(21), days(28)) # cutoff time interval
interval_tag <- c("1d/", "3d/", "7d/", "14d/", "21d/", "28d/")
interval_name <- c("1d", "3d", "7d", "14d", "21d", "28d")

title_size <- c(18, 28, 24); axis_size <- c(16, 26, 22)

############create a function to for the pre-process of data############
repost_pre <- function(trainee){
  trainee$user_id <- as.character(trainee$user_id); trainee$father_weibo_user_id <- as.character(trainee$father_weibo_user_id) # change data type from factor to character
  trainee$pub_time <- as.POSIXct(trainee$pub_time, tz = "HongKong") # change data type to datetime with the corresponding timezone
  trainee <- trainee[order(trainee$pub_time),] # arrange in the time order
  return(trainee)
}

############create a function to get the parameters of the reposts############
repost_measure <- function(trainee, name, date, k){
  trainee <- subset(trainee, pub_time <= trainee$pub_time[1] + interval[k]) # only consider reposts published within the interval
  start_datetime <- trainee[1,]$pub_time # the publish time of the post
  n <- nrow(trainee) - 1 # repost number (do not count the original tweet) ###
  m <- length(unique(trainee$user_id)) - 1 # fan number (do not count the celebrity) ###
  d = 0 # repost depth ###
  l = NULL
  # (repost number, fan number, fractions of reposts at each level, depth and average level)
  parent <- trainee[1, ]
  kids <- trainee[trainee$father_weibo_id %in% parent$weibo_id, ]
  while(nrow(kids)){ # consider the direct parent-child relation
    l <- c(l, round(nrow(kids)/n, digits = 4))
    parent <- kids
    kids <- trainee[trainee$father_weibo_id %in% parent$weibo_id, ]
    d <- d + 1
  }
  radius <- sum(sapply(1:d, function(i) i*l[i])) # average distance ###
  # colnames: thumb_num, pub_time, user_id, comment_num, father_weibo_id (5), repost_num, content, weibo_id, client_type, 
  # father_weibo_user_id (10), father_weibo_user_name, user_name, repost_reason
  df_0 <- trainee[c(2,3,10,12,11)] # remove unused columns
  df <- df_0[-1, ] # remove the original post
  
  edges <- df[, c("father_weibo_user_id","user_id")] # extract edge list
  g_trainee <- graph_from_data_frame(edges, directed = TRUE, vertices = NULL) # create graph
  E(g_trainee)$weight <- 1 # remove mulitple edges and do not keep multiplicity
  g_trainee <- simplify(g_trainee, remove.loops = FALSE, edge.attr.comb=list(weight="ignore"))
  distance <- mean_distance(g_trainee, directed = FALSE, unconnected = FALSE) # consider the network as undirected and unwighted!!!
  
  # find all the special ids: boosters, sock puppets as well as dusts
  outdegree <- as.data.frame(table(df$father_weibo_user_id))
  colnames(outdegree) <- c("user_id", "out_degree")
  indegree<-as.data.frame(table(df$user_id))
  colnames(indegree) <- c("user_id", "in_degree")
  # merge the two datasets
  degree <- merge(indegree, outdegree, by = "user_id", all = TRUE)
  degree[is.na(degree)] <- 0
  degree <- degree[order(-degree$out_degree, -degree$in_degree),]
  # find the boosters
  booster <- setdiff(as.vector(degree$user_id[degree$out_degree >= sqrt(n)]), list_id) # the criterion 
  nb <- length(booster)
  # find the sock puppets
  sp <- NULL
  if(nb > 0){ # suppose that there is any booster
    for (i in 1:nb){
      desced <- subcomponent(g_trainee, booster[i], mode = "out")$name
      sp <- c(sp, desced[-1]) # remove the booster himself/herself
    }
  }
  
  dust <- names(which(table(df$user_id) == 1))
  
  
  ############################################################function to decide whether there is a detour surrounding the user
  detour <- function(id, df_0){
    flag <- 0 # count the number detours
    A_id <- setdiff(unique(df_0[df_0$user_id == id, ]$father_weibo_user_id), id) # find the parents (do not include self reposts)
    C_id <- setdiff(unique(df_0[df_0$father_weibo_user_id == id, ]$user_id), id) # find the children (do not include self reposts)
    df_AC <- df_0[df_0$user_id %in% C_id & df_0$father_weibo_user_id %in% A_id, ] # get the shortcut reposts
    df_AC <- df_AC[!duplicated(df_AC[c("user_id", "father_weibo_user_id")]), ] # only keep the first reposts
    if(nrow(df_AC)>0){ # when there do exist shortcuts
      C_id <- df_AC$user_id # include A to C and D to C (corresponding to A to B and D to B)
      get_flag <- function(i){
        df_BC <- df_0[df_0$user_id == C_id[i] & df_0$father_weibo_user_id == id, ] # get the detour reposts
        df_BC <- df_BC[!duplicated(df_BC[c("user_id", "father_weibo_user_id")]), ] # only keep the first repost
        flag_test <- df_BC[1,]$pub_time < df_AC[i,]$pub_time # if the repost BC is before the repost AC
        return(flag_test)
      }
      flag <- sum(sapply(1:length(C_id), get_flag)) # apply to every child
    }
    return(c(nrow(df_AC),flag))
  }
  
  ############################################################task A: analyse the triangle structures in the network############################################################
  # A to B, B to C vs A to C
  inter_id <- intersect(df_0$user_id, df_0$father_weibo_user_id) # find users that are both a parent and a child
  inter_count <- rowSums(sapply(inter_id, detour, df_0))
  m_tri <- inter_count[1]  # number of triangle structures in the repost network ###
  m_detour <- inter_count[2] # number of detours in the repost network ###
  ############################################################task A: analyse the circluar structures in the network############################################################
  # A to B versus B to A
  AB <- unique(df[c("user_id","father_weibo_user_id")])
  AB <- AB[!AB$user_id == AB$father_weibo_user_id, ] # remove self reposts
  BA <- AB[,c(2,1)] # interchange the first and second columns
  colnames(BA) <- c("user_id","father_weibo_user_id")
  m_mutual <- nrow(inner_join(AB, BA)) # number of pairs who reposted each other ###
  
  ############################################################task C: analyse the repost frequencies############################################################
  ####################################consider all users (include users who only reposted once)
  df_datetime_0 <- as.data.frame(df_0$user_id)
  colnames(df_datetime_0) <- "user_id"
  df_datetime_0$min <- c(NA, as.numeric(difftime(df_0$pub_time[-1], start_datetime, units="mins")))
  df_datetime_0$interval <- c(NA, as.numeric(difftime(df_0$pub_time[-1], df_0$pub_time[-nrow(df_0)], units="mins"))) # whether to add an NA or a zero?
  df_datetime_0$type <- "others"
  df_datetime_0[df_datetime_0$user_id %in% dust,]$type <- "dust"
  df_datetime_0$type[df_datetime_0$user_id %in% sp] <- "sock puppet"
  df_datetime_0$type[df_datetime_0$user_id %in% booster] <- "booster"
  df_datetime_0[df_datetime_0$user_id %in% list_id,]$type <- "celebrity"
  fraction <- round(sum(df_datetime_0$type=="sock puppet" | df_datetime_0$type=="booster")/n, digits = 4)
  write.csv(df_datetime_0,
            file = paste(folder_d, "general/", interval_tag[k], name, "_", date, "_general.csv", sep = ""), row.names = FALSE)
  #ggplot(df_datetime_0) + geom_histogram(aes(mins))
  
  ####################################consider self reposts in particular
  df_self <- df[df$user_id == df$father_weibo_user_id, ]
  n_self_repost <- nrow(df_self) # number of self reposts ###
  self_id <- unique(df_self$user_id)
  m_self <- length(self_id) # number of fans perform self reposts ###
  frequency_self <- function(i){ # calculate the time differences of self reposts
    df_self_single <- df_self[df_self$user_id == self_id[i], ]
    start_datetime_self <- df_self_single$pub_time[1]
    df_datetime_self <- as.data.frame(df_self_single$user_id)
    colnames(df_datetime_self) <- "user_id"
    df_datetime_self$min <- c(NA, as.numeric(difftime(df_self_single$pub_time[-1], start_datetime_self, units = "mins")))
    df_datetime_self$interval <- c(NA, as.numeric(difftime(df_self_single$pub_time[-1], df_self_single$pub_time[-nrow(df_self_single)], units="mins")))
    return(df_datetime_self)
  }
  if(nrow(df_self)>0){
    df_datetime_self <- do.call("rbind", lapply(1:m_self, frequency_self))
    df_datetime_self$type <- "others"
    df_datetime_self$type[df_datetime_self$user_id %in% sp] <- "sock puppet"
    df_datetime_self$type[df_datetime_self$user_id %in% booster] <- "booster"
  }else{df_datetime_self <- data.frame(user_id=character(), min=integer(), interval=integer(), type=character(), stringsAsFactors=FALSE) }
  
  
  write.csv(df_datetime_self,
            file = paste(folder_d, "self/", interval_tag[k], name, "_", date, "_self.csv", sep = ""), row.names = FALSE)
  #ggplot(df_datetime_self) + geom_histogram(aes(mins))
  
  
  ####################################consider reposts by user who reposted more than once
  ############consider reposts by dusts
  df_dust <- df[df$user_id %in% names(which(table(df$user_id) == 1)), ]
  df_datetime_dust <- as.data.frame(df_dust$user_id)
  colnames(df_datetime_dust) <- "user_id"
  df_datetime_dust$min <- as.numeric(difftime(df_dust$pub_time, start_datetime, units = "mins"))
  df_datetime_dust$interval <- c(NA, diff(df_datetime_dust$min))
  df_datetime_dust$type <- "dust"
  write.csv(df_datetime_dust,
            file = paste(folder_d, "dust/", interval_tag[k], name, "_", date, "_dust.csv", sep = ""), row.names = FALSE)
  m_dust <- length(unique(df_dust$user_id)) # number of fans who only reposted once ###
  
  #df <- df[df$user_id %in% names(which(table(df$user_id) > 1)), ] # remove reposts by users who only reposted once
  
  
  ############consider reposts by pairs of users
  # The formula is NOT(duplicated OR duplicated). 
  # The first duplicated does not identify the first occurrences of duplicated values and the second duplicated does not identify the last occurrences of duplicated values. 
  # Together, they identify all duplicated values.
  #df_pair <- df[duplicated(df[c("user_id","father_weibo_user_id")]) | duplicated(df[c("user_id","father_weibo_user_id")], fromLast = TRUE), ]
  df_pair <- df
  pair_id <- unique(cbind(df_pair$user_id, df_pair$father_weibo_user_id)) # include self reposts
  frequency_pair <- function(i){ # calculate the frequencies of pair reposts (AB, AB, AB, ...) (AA, AA, AA, ...) (self repost) (AC) (no repeat, just once) is included as well
    df_pair_single <- df_pair[df_pair$user_id == pair_id[i,1] & df_pair$father_weibo_user_id == pair_id[i,2], ]
    start_datetime_pair <- df_pair_single$pub_time[1]
    df_datetime_pair <- as.data.frame(df_pair_single$user_id)
    colnames(df_datetime_pair) <- "user_id"
    df_datetime_pair$min <- c(NA, as.numeric(difftime(df_pair_single$pub_time[-1], start_datetime_pair, units = "mins")))
    df_datetime_pair$interval <- c(NA, as.numeric(difftime(df_pair_single$pub_time[-1], df_pair_single$pub_time[-nrow(df_pair_single)], units="mins")))
    df_datetime_pair$mark <- ifelse( df_pair_single$user_id== df_pair_single$father_weibo_user_id, 1, 0) # if it is a self repost, mark 1, else 0 
    return(df_datetime_pair)
  }
  df_datetime_pair <- do.call("rbind", lapply(1:nrow(pair_id), frequency_pair))
  df_datetime_pair$type <- "others"
  df_datetime_pair[df_datetime_pair$user_id %in% dust,]$type <- "dust"
  df_datetime_pair$type[df_datetime_pair$user_id %in% sp] <- "sock puppet"
  df_datetime_pair$type[df_datetime_pair$user_id %in% booster] <- "booster"
  df_datetime_pair <- df_datetime_pair[,c(1,2,3,5,4)]
  
  write.csv(df_datetime_pair,
            file = paste(folder_d, "pair/", interval_tag[k], name, "_", date, "_pair.csv", sep = ""), row.names = FALSE)
  #ggplot(df_datetime_pair) + geom_histogram(aes(mins))
  
  
  ############consider reposts by different children
  son_id <- unique(df$user_id)
  ans <- as.data.table(df)[, count := uniqueN(father_weibo_user_id), by = user_id] # count the number of different fathers for sons
  ans <- ans[!duplicated(ans$user_id), ]
  m_more_users <- sum(ans$count > 1) # number of fans who reposted more than one user (a subset of fans who reposted more than once) ###
  frequency_son <- function(i){ # calculate the frequencies of reposts with fixed son (AB, CB, BB, DB, ...)
    df_single <- df[df$user_id == son_id[i], ]
    start_datetime <- df_single$pub_time[1]
    df_datetime <- as.data.frame(df_single$user_id)
    colnames(df_datetime) <- "user_id"
    df_datetime$min <- c(NA, as.numeric(difftime(df_single$pub_time[-1], start_datetime, units = "mins")))
    df_datetime$interval <- c(NA, as.numeric(difftime(df_single$pub_time[-1], df_single$pub_time[-nrow(df_single)], units="mins")))
    df_datetime$mark <- ifelse(df_single$user_id==df_single$father_weibo_user_id, 1, 0) # if it is a self repost, mark 1, else 0 
    return(df_datetime)
  }
  df_datetime <- do.call("rbind", lapply(1:length(son_id), frequency_son))
  df_datetime$type <- "others"
  df_datetime[df_datetime$user_id %in% dust,]$type <- "dust"
  df_datetime$type[df_datetime$user_id %in% sp] <- "sock puppet"
  df_datetime$type[df_datetime$user_id %in% booster] <- "booster"
  df_datetime <- df_datetime[,c(1,2,3,5,4)]
  
  
  write.csv(df_datetime,
            file = paste(folder_d, "child/", interval_tag[k], name, "_", date, "_child.csv", sep = ""), row.names = FALSE)
  #ggplot(df_datetime) + geom_histogram(aes(mins))

  # repost number, fan number, depth, average level, distance, fraction of reposts made by boosters and sps, self repost number, self repost fan number, dust fan number,
  # fan number who reposted more than one users, fan pairs who reposted each other, triangle structure number, detour triangle structure number
  l <- c(n, m, d, radius, distance, fraction, n_self_repost, m_self, m_dust, m_more_users, m_mutual, m_tri, m_detour, l)
  return(l)
}

############create a function to count the daily number of reposts############
repost_count <- function(trainee, timeStamps){
  Dates <- as.Date(trainee$pub_time, tz = "HongKong")
  allDates <- seq(from = min(Dates), to = max(Dates), by = "day")
  reposts <- sapply(allDates, FUN = function(X) sum(Dates == X))
  cumreposts <- cumsum(reposts)
  result <- data.frame(day = allDates, reposts = reposts, cumreposts = cumreposts)
  return(result)
}

############create a function to count the daily number of reposts (daily)############
repost_count_d <- function(trainee, timeStamps){
  Datetimes <- as.POSIXct(trainee$pub_time, tz = "HongKong")
  cutDatetimes <- seq(from = min(Datetimes), to = max(Datetimes), by = "day")
  reposts <- sapply(cutDatetimes, FUN = function(X) sum(Datetimes < X + days(1) & Datetimes >= X))
  cumreposts <- cumsum(reposts)
  result <- data.frame(day = as.Date(cutDatetimes, tz = "HongKong"), reposts = reposts, cumreposts = cumreposts)
  return(result)
}

############create a function to count the hourly number of reposts (hourly)############
repost_count_h <- function(trainee, timeStamps){
  Datetimes <- as.POSIXct(trainee$pub_time, tz = "HongKong")
  cutDatetimes <- seq(from = min(Datetimes), to = max(Datetimes), by = "hour")
  reposts <- sapply(cutDatetimes, FUN = function(X) sum(Datetimes < X + hours(1) & Datetimes >= X))
  cumreposts <- cumsum(reposts)
  result <- data.frame(day = as.Date(cutDatetimes, tz = "HongKong"), reposts = reposts, cumreposts = cumreposts)
  return(result)
}

############create a function to get parameters of logistic growth############
logistic_para <- function(trainee){
  ttrainee <- repost_count_d(trainee)
  n = min(7, nrow(ttrainee)) # only consider the first 7 days
  ttrainee1 <- filter(ttrainee, day <= day[1] + n - 1)
  ttrainee1$index <- 1:nrow(ttrainee1)
  #calculate initial parameters
  para <- as.vector(getInitial(cumreposts ~ SSlogis(index, alpha, xmid, scale), data = ttrainee1))
  para0 <- c(alpha=para[1], beta=para[2]/para[3], gamma=1/para[3])
  fit0 <- coef(nls(cumreposts~alpha/(1+exp(beta-gamma*index)), ttrainee1, start=para0, trace=T))
  return(fit0)
}

############define a function to get the number of boosters, minimal and maximal number of sock puppets############
booster_sp_number <- function(trainee, k){
  trainee <- subset(trainee, pub_time <= trainee$pub_time[1] + interval[k]) # only consider reposts published within the interval
  trainee <- trainee[!(trainee$father_weibo_id=="ROOT"),] # remove the original tweet
  edges <- trainee[, c("father_weibo_user_id","user_id")] # extract the edge list
  # create graph
  g_trainee <- graph_from_data_frame(edges, directed = TRUE, vertices = NULL)
  E(g_trainee)$weight <- 1 # remove mulitple edges but keep multiplicity
  g_trainee <- simplify(g_trainee, remove.loops = FALSE, edge.attr.comb=list(weight="sum"))
  # degrees calculation, considering multiple edges
  outdegree <- as.data.frame(table(trainee$father_weibo_user_id))
  colnames(outdegree) <- c("user_id", "out_degree")
  indegree<-as.data.frame(table(trainee$user_id))
  colnames(indegree) <- c("user_id", "in_degree")
  # merge the two datasets
  degree <- merge(indegree, outdegree, by = "user_id", all = TRUE)
  degree[is.na(degree)] <- 0
  degree <- degree[order(-degree$out_degree, -degree$in_degree),]
  # find the boosters
  booster <- as.vector(degree$user_id[degree$out_degree >= sqrt(nrow(trainee))]) # the criterion 
  n <- length(booster)
  ma <- 0 # record the maximal number of sock puppets
  mi <- 10000 # record the minimal number of sock puppets
  flag <- 0 # count the number of trainees
  # put the descendents in the degree dataframe and extract nodes of boosters as well as
  # the corresponding sock puppets
  for (i in 1:n){
    desced <- subcomponent(g_trainee, booster[i], mode = "out")$name
    if (ma < length(desced) & !(degree$user_id[i] %in% list_id)){
      ma <- length(desced) - 1 # exclude the booster itself
    }
    if (mi > length(desced) & !(degree$user_id[i] %in% list_id)){
      mi <- length(desced) - 1 # exclude the booster itself
    }
    if(degree$user_id[i] %in% list_id){flag <- flag + 1} # we do not consider any trainee as a booster
  }
  if(n-flag==0){mi = 0} # in case there is no boosters
  return(c(n - flag, mi, ma))
}

############define a function to get boosters and maximal sock puppets numbers############
booster_stats <- function(trainee){
  trainee <- trainee[!(trainee$father_weibo_id=="ROOT"),]
  edges <- trainee[, c("father_weibo_user_id","user_id")] # extract edge list
  # create graph
  g_trainee <- graph_from_data_frame(edges, directed = TRUE, vertices = NULL)
  E(g_trainee)$weight <- 1 # remove mulitple edges but keep multiplicity
  g_trainee <- simplify(g_trainee, remove.loops = FALSE, edge.attr.comb=list(weight="sum"))
  # degrees calculation, considering multiple edges
  outdegree<- as.data.frame(table(trainee$father_weibo_user_id))
  colnames(outdegree)<- c("user_id", "out_degree")
  indegree<-as.data.frame(table(trainee$user_id))
  colnames(indegree)<- c("user_id", "in_degree")
  # merge the two datasets
  degree <- merge(indegree, outdegree, by = "user_id", all = TRUE)
  degree[is.na(degree)] <- 0
  degree <- degree[order(-degree$out_degree, -degree$in_degree),]
  # find the boosters
  booster <- as.vector(degree$user_id[degree$out_degree >= sqrt(nrow(trainee))])
  n <- length(booster)
  number_of_sp <- rep(0,n)
  sp <- rep("sp",n)
  booster_statistics <- data.frame(booster, number_of_sp, sp)
  flag <- 0 # count the number of trainees
   #put the descendents in the degree dataframe and extract nodes of boosters as well as
  # the corresponding sock puppets
  for (i in 1:n){
    desced <- subcomponent(g_trainee, booster[i], mode = "out")$name
    if (!(degree$user_id[i] %in% list_id)){
      ma <- length(desced) - 1 # exclude the booster itself
    }
    if (mi > length(desced) & !(degree$user_id[i] %in% list_id)){
      mi <- length(desced) - 1 # exclude the booster itself
    }
    if(degree$user_id[i] %in% list_id){flag <- flag + 1}
  }
  return(c(n - flag, mi, ma))
}

########################create a function to return booster indices###########################
booster_index <- function(trainee, k){
  trainee <- trainee[order(trainee$pub_time),] # time order
  trainee <- subset(trainee, pub_time <= trainee$pub_time[1] + interval[k]) # only consider reposts published within the interval
  colnames(trainee)[10] <- "source" # change the column names for gephi to recognize
  colnames(trainee)[3] <- "target" # 5 & 8 event based; 11 & 12 (name)/3 & 10 (id) user based
  trainee_index <- as.data.frame(unique(trainee$target))
  trainee_index$index<- seq.int(nrow(trainee_index))
  colnames(trainee_index)[1] <- "user_id"
  flag <- which(unique(trainee$target) %in% list_id)
  # match user_id with index 1,2,3,4,5...
  trainee$index <- trainee_index$index[match(trainee$target, trainee_index$user_id)]
  trainee$father_index <- trainee_index$index[match(trainee$source, trainee_index$user_id)]
  trainee_new <- subset(trainee, select=c("pub_time", "index","father_index"))
  in_degree <- as.data.frame(table(trainee_new$index))
  out_degree <- as.data.frame(table(trainee_new$father_index))
  colnames(in_degree) <- c("index", "in_degree")
  colnames(out_degree) <- c("index", "out_degree")
  degree <- merge(in_degree, out_degree, by = c("index"), all = TRUE)
  degree[is.na(degree)] <- 0
  index_sub0 <- degree[which(degree$out_degree >= sqrt(nrow(trainee) - 1)),]$index
  index_sub0 <- index_sub0[!index_sub0 %in% flag] # remove the indices of the trainees
  index_sub <- paste(index_sub0,collapse=" ") # turn the index vector into a single string
  # number of reposts, number of fans, index of the first booster, index of the last booster, indices of the boosters
  return(c(nrow(trainee), nrow(degree), index_sub0[1], rev(index_sub0)[1], index_sub)) 
}

########################binding########################

############create a function to bind all the parameters############
repost_event <- function(filelist, name, date_csv, k){ # e.g. repost_event(filelist1, "dcx", dcx_date_csv, k)
  l <- length(filelist)   # extract the number of data frames
  post <- vector("list", l) # create a vector to store the lists
  for (i in 1:l) {post[[i]] <- repost_measure(filelist[[i]], name, date_csv[i], k)}
  n <- max(sapply(post, length)) # compute the maximum of vector lengths (decided by the depth)
  post <- lapply(post, function(v) {c(v, rep(NA, n-length(v)))})
  do.call(rbind, post) # convert the list to a data frame
}

############create a function to bind all the logistic parameters############ 
repost_para <- function(filelist){
  l <- length(filelist)  #extract the number of data frames
  logpara <- vector("list", l) # create a vector to store the lists
  post_date <- vector("character", l) # create a vector to store the dates
  class(post_date) <- "Date"
  for (i in 1:l){
    testpara <- try(logistic_para(filelist[[i]]), TRUE) # sometimes we cannot get ideal logistic regression
    if(isTRUE(class(testpara)=="try-error")) { logpara[[i]] <- NA } 
    else {logpara[[i]] <- testpara}
    post_date[i] <- as.Date(filelist[[i]][1, ]$pub_time)
  }
  logpara <- do.call(rbind, logpara)
  post_date <- data.frame(post_date)
  #post_date <- post_date[!apply(is.na(post_date) | is.null(post_date)),] # remove empty rows
  cbind(post_date, logpara)
}

########################create a function to bind all data returned from booster_sp_number() ########################
repost_booster_sp <- function(filelist, k){
  l <- length(filelist) # extract the number of data frames
  boo_num <- vector("list", l) # create a list to store the vectors
  for (i in 1:l) {boo_num[[i]] <- booster_sp_number(filelist[[i]], k)}
  do.call(rbind, boo_num) # convert the list to a data frame
}

########################create a function to bind all data returned from booster_index()########################
repost_booster_index <- function(filelist, k){
  l <- length(filelist) # extract the number of data frames
  boo_ind <- vector("list", l) # create a list to store the vectors
  for (i in 1:l) {boo_ind[[i]] <- booster_index(filelist[[i]], k)}
  do.call(rbind, boo_ind) # convert the list to a data frame
}

########################create a function to extract all the dates###########################
repost_date <- function(filelist){
  l <- length(filelist) # extract the number of data frames
  post_date <- vector("character", l)   # create a vector to store the date
  class(post_date) <- "Date"
  for (i in 1:l) {post_date[i] <- as.Date(filelist[[i]][filelist[[i]]$father_weibo_id=="ROOT", ]$pub_time, tz = "HongKong")}
  post_date <- data.frame(post_date)
  return(post_date)
}

########################plotting########################
#plot_level("dcx", dcx_event, dcx_date, c(1,16,24,52,108), "Blues", 1)
########################create a function to plot level distribution###########################
plot_level <- function(name, trainee_event, trainee_date, subindex, color_palette, tag){
  trainee_date_sub <- trainee_date[subindex,]
  trainee_event_sub <- trainee_event[which(as.Date(trainee_event[,1]) %in% trainee_date_sub), -c(2:14,ncol(trainee_event))]
  us_date <- sapply(format(as.Date(trainee_event_sub[,1]), "%m/%d/%y"), as.character)
  #trainee_event_sub[,1] <- sapply(trainee_event_sub[,1], as.character)
  trainee_event_sub[,1] <- us_date
  trainee_event_sub <- melt(trainee_event_sub,id = "post_date") # each row is a unique id-variable combination
  p = ggplot(trainee_event_sub, aes(x = trainee_event_sub[,1], y = value, fill = variable)) +
    geom_bar(stat='identity', position = position_fill(reverse = TRUE)) +
    scale_fill_brewer(palette=color_palette) + 
    labs(x="date", y="level fraction") +
    scale_x_discrete(limits = us_date) +
    theme_bw() +
    theme(text = element_text(size=title_size[3]), axis.text.x = element_text(size = axis_size[3]), axis.text.y = element_text(size = axis_size[3])) +
    theme(legend.title = element_blank())
  #annotate_figure(p,
                  #top = text_grob("timeline", color = "black", face = "bold", size = 14),
                  #bottom = text_grob(paste("Data source: trainee ", tag, sep=''), color = "black",
                                     #just = "centre", hjust = 1.5, x = 1, face = "plain", size = 15))
  ggsave(height = 10, width = 10, file=paste0(folder_f, "level_", name, ".pdf"))
}

########################create a function to prepare date for print###########################
date_print <- function(date){
  date <- format(as.Date(date, format = "%Y-%m-%d"), "%m/%d/%Y") # convert 2017-09-06 to 09/06/2017
  date <- as.character(date)
}

########################create a function to plot data for both trainees###########################
plot_bi <- function(trainee1, trainee2, n, variable_name, plot_name){
  date_1_f <- date_print(trainee1[1,1]) # extract the date of the first post for trainee 1
  date_1_l <- date_print(trainee1[nrow(trainee1),1]) # extract the date of the last post for trainee 1
  date_2_f <- date_print(trainee2[1,1]) # extract the date of the first post for trainee 2
  date_2_l <- date_print(trainee2[nrow(trainee2),1]) # extract the date of the last post for trainee 2
  options(scipen=999) # remove scientific notation in printing
  p = ggplot()+
    geom_line(data = trainee1,aes(x=index,y=trainee1[,n], color = "trainee1"), size = 1)+
    geom_line(data = trainee2,aes(x=index,y=trainee2[,n], color = "trainee2"), size = 1)+
    scale_color_manual(values = c("cornflowerblue", "pink")) +
    xlab("post") +
    ylab(variable_name) +
    theme_bw() +
    theme(text = element_text(size=15), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
  title1 <- paste("trainee 1 post date: ", date_1_f, " to ", date_1_l)
  title2 <- paste("trainee 2 post date: ", date_2_f, " to ", date_2_l)
  annotate_figure(p,
                  #top = text_grob("timeline", color = "black", face = "bold", size = 14),
                  bottom = text_grob(paste(title1, title2, sep = "\n"), color = "black",
                                     just = "centre", hjust = 1, x = 1, face = "plain", size = 15))
  ggsave(file=paste0(folder_f, plot_name, ".eps"))
}

########################create a function to plot data for one trainee with linear regression###########################
plot_mono <- function(trainee_event, n, variable_name, plot_name, tag){
  last_date = format(as.Date(trainee_event[nrow(trainee_event),1], format = "%Y-%m-%d"), "%m/%d/%Y")
  mid = mean(trainee_event[,n])
  shape = c(16, 16) #16, 18
  name = c("dcx", "azy")
  if(tag==1){color = c("#eff3ff","#bdd7e7","#6baed6", "#2171b5")}
  else{color = c("#fee5d9","#fcae91","#fb6a4a","#cb181d")}
  options(scipen = 0) # restore scientific notation in printing
  p = ggplot(data = trainee_event,aes(x=index,y=trainee_event[,n], color = trainee_event[,n]))+
    scale_shape_discrete(solid=F) +
    geom_point(shape = shape[tag], size = 1.5) +
    scale_color_gradient2(midpoint=mid, low=color[1], mid=color[2],
                         high=color[4], space ="Lab" ) +
    geom_smooth(method = "lm", color = color[3], se = FALSE) + # add linear regression
    xlab("day") +
    ylab(variable_name) +
    theme_bw() +
    theme(legend.title=element_blank()) +
    theme(text = element_text(size=15), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
  annotate_figure(p,
                  #top = text_grob("timeline", color = "black", face = "bold", size = 14),
                  bottom = text_grob(paste("Data source: trainee ", tag, " Post date: 12/12/2015 to ", as.character(last_date), sep=''), 
                                     color = "black", just = "centre", hjust = 1, x = 1, face = "plain", size = 15))
  ggsave(file=paste0(folder_f, plot_name, "_", name[tag], ".eps"))
  return(p)
}

########################create a function to get and save side-by-side plot###########################
plot_mono_grid <- function(trainee1, trainee2, p1, p2, plot_name){
  date_1_f <- date_print(trainee1[1,1]) # extract the date of the first post for trainee 1
  date_1_l <- date_print(trainee1[nrow(trainee1),1]) # extract the date of the last post for trainee 1
  date_2_f <- date_print(trainee2[1,1]) # extract the date of the first post for trainee 2
  date_2_l <- date_print(trainee2[nrow(trainee2),1]) # extract the date of the last post for trainee 2
  title1 <- paste("trainee 1 post date: ", date_1_f, " to ", date_1_l)
  title2 <- paste("trainee 2 post date: ", date_2_f, " to ", date_2_l)
  options(scipen = 0) # restore scientific notation in printing
  p <- grid.arrange(p1, p2, ncol=1)
  annotate_figure(p,
                  #top = text_grob("timeline", color = "black", face = "bold", size = 14),
                  bottom = text_grob(paste(title1, title2, sep = "\n"), color = "black",
                                     just = "centre", hjust = 1, x = 1, face = "plain", size = 15))
  ggsave(file=paste0(folder_f, plot_name, "_lm.eps"))
}

########################create a function to save data frames###########################
repost_save <- function(df, name){
  write.csv(df,
            file = paste(folder, "R/statistics/", name, sep = ""), row.names = FALSE)
}

filenames1 <- list.files(path=folder, pattern="dcx_1+.*csv") #*.csv #.*csv # get the file names "dcx_151212.csv" etc.
dcx_date_csv <- str_sub(filenames1,5,10) # get post pates '151212' etc.
filelist1 <- lapply(filenames1, read.csv)
filelist1 <- lapply(filelist1, repost_pre)
dcx_date <- repost_date(filelist1) # get post pates '2015-12-12' etc.

filenames2 <- list.files(path=folder, pattern="azy_1+.*csv") # get filenames "azy_151212.csv" etc.
azy_date_csv <- str_sub(filenames2,5,10) # get post pates "151212" etc.
filelist2 <- lapply(filenames2, read.csv)
filelist2 <- lapply(filelist2, repost_pre)
azy_date <- repost_date(filelist2)

dcx_para <- repost_para(filelist1)


azy_para <- repost_para(filelist2)



k = 4 # k = 4, time interval is 14 days/2 weeks
########trainee1########
dcx_event <- repost_event(filelist1, "dcx", dcx_date_csv, k)
dcx_event <- cbind(dcx_date, dcx_event)
# date, repost number, fan number, depth, average level, average distance, fraction of reposts made by boosters and sps, self repost number, self repost fan number, 
# dust fan number, fan number who reposted more than one users, pair number who reposted each other, triangle structure number, detour triangle structure number
# date, n, m, d, radius, n_self_repost, m_self, m_dust, m_more_users, m_mutual, m_tri, m_detour, l
colnames(dcx_event) <- c('post_date','repost_number','fan_number', 'depth', 'average_level', 'distance', 'fraction', 'n_self', 'm_self', 'm_dust', 'm_2', 'm_mutual', 'm_tri', 'm_detour',
                         sprintf("level%d", 1:(ncol(dcx_event)-14)))

start_date <- dcx_event$post_date[1]

dcx_event$index <- as.integer(as.Date(as.character(dcx_event$post_date)) - as.Date(as.character(start_date))) + 1
View(dcx_event)
repost_save(dcx_event, paste0("dcx_event_", interval_name[k], ".csv")) # save the dataframe

dcx_para$index <- dcx_event$index
View(dcx_para)
repost_save(dcx_para, "dcx_para.csv")

dcx_booster <- as.data.frame(repost_booster_sp(filelist1, k))
colnames(dcx_booster) <- c('booster_number', 'minimal_number_of_sp','maximal_number_of_sp')
dcx_booster$index <- dcx_event$index
dcx_booster <- cbind(dcx_date, dcx_booster)
View(dcx_booster)
repost_save(dcx_booster, paste0("dcx_booster_", interval_name[k], ".csv"))

dcx_booster_index <- as.data.frame(repost_booster_index(filelist1, k))
colnames(dcx_booster_index) <- c('repost_number', 'fan_number', 'first', 'last', 'index')
dcx_booster_index <- cbind(dcx_date, dcx_booster_index)
View(dcx_booster_index)
repost_save(dcx_booster_index, paste0("dcx_booster_index_", interval_name[k], ".csv"))

########trainee2########

azy_event <- repost_event(filelist2, "azy", azy_date_csv, k)
azy_event <- cbind(azy_date, azy_event)
colnames(azy_event) <- c('post_date','repost_number','fan_number', 'depth', 'average_level', 'distance', 'fraction', 'n_self', 'm_self', 'm_dust', 'm_2', 'm_mutual', 'm_tri', 'm_detour',
                         sprintf("level%d", 1:(ncol(azy_event)-14)))
azy_event$index <- as.integer(as.Date(as.character(azy_event$post_date)) - as.Date(as.character(start_date))) + 1
View(azy_event)
repost_save(azy_event, paste0("azy_event_", interval_name[k], ".csv"))

azy_para$index <- azy_event$index
View(azy_para)
repost_save(azy_para, "azy_para.csv")

azy_booster <- as.data.frame(repost_booster_sp(filelist2, k))
colnames(azy_booster) <- c('booster_number', 'minimal_number_of_sp', 'maximal_number_of_sp')
azy_booster$index <- azy_event$index 
azy_booster <- cbind(azy_date, azy_booster)
View(azy_booster)
repost_save(azy_booster, paste0("azy_booster_", interval_name[k], ".csv"))

azy_booster_index <- as.data.frame(repost_booster_index(filelist2, k))
colnames(azy_booster_index) <- c('repost_number', 'fan_number', 'first', 'last', 'index')
azy_booster_index <- cbind(azy_date, azy_booster_index)
repost_save(azy_booster_index, paste0("azy_booster_index_", interval_name[k], ".csv"))

########################main function########################
repost_event_all <-
  function(job=0){
    if(job==0){ # calculate the data size
      dcx_m = length(filelist1)
      azy_m = length(filelist2)
      dcx_n = sum(sapply(filelist1, function(x) nrow(x)))
      azy_n = sum(sapply(filelist2, function(x) nrow(x)))
      print(paste("trainee 1: post number ", dcx_m, ", repost number ", dcx_n, sep=''))
      print(paste("trainee 2: post number ", azy_m, ", repost number ", azy_n, sep=''))
      print(paste("total: post number ", dcx_m + azy_m, ", repost number ", dcx_n + azy_n, sep=''))
      print(paste("minimal: repost number ", min(c(sapply(filelist1, function(x) nrow(x)), sapply(filelist2, function(x) nrow(x)))), 
                  ", depth ", min(c(dcx_event$depth, azy_event$depth)), sep=''))
      print(paste("maximal: repost number ", max(c(sapply(filelist1, function(x) nrow(x)), sapply(filelist2, function(x) nrow(x)))), 
                  ", depth ", max(c(dcx_event$depth, azy_event$depth)), sep=''))
    }
    # date, n, m, d, radius, n_self_repost, m_self, m_dust, m_more_users, m_mutual, m_tri, m_detour, l
    if(job==1){ # select a subset of event and plot level distributions
      #name, trainee_event, trainee_date, subindex, color_palette, tag
      plot_level("dcx", dcx_event, dcx_date, c(1,40,82,177,203), "Blues", 1)
      plot_level("azy", azy_event, azy_date, c(1,40,92,101,162), "Reds", 2)
    }
    if(job==2){ # plot repost numbers
      # trainee1, trainee2, n, variable_name, plot_name
      plot_bi(dcx_event, azy_event, 2, "number of reposts", "repost_number")
    }
    if(job==2.1){ # plot repost numbers with linear regression
      # trainee_event, n, variable_name, tag
      p1 <- plot_mono(dcx_event, 2, "number of reposts", "repost_number",  1)
      p2 <- plot_mono(azy_event, 2, "number of reposts", "repost_number",  2)
      plot_mono_grid(dcx_event, azy_event, p1, p2, "repost_number")
      
    }
    if(job==3){ # plot fan numbers
      plot_bi(dcx_event, azy_event, 3, "number of fans", "fan_number")
      }
    if(job==3.1){ # plot fan numbers with linear regression
      p1 <- plot_mono(dcx_event, 3, "number of fans", "fan_number",  1) #1 for trainee 1 and 2 for trainee 2
      p2 <- plot_mono(azy_event, 3, "number of fans", "fan_number", 2)
      plot_mono_grid(dcx_event, azy_event, p1, p2, "fan_number")
    }
    if(job==4){ # plot depths
      plot_bi(dcx_event, azy_event, 4, "depth", "depth")
    }
    if(job==4.1){ # plot depths with linear regression
      p1 <- plot_mono(dcx_event, 4, "depth", "depth",  1) #1 for trainee 1 and 2 for trainee 2
      p2 <- plot_mono(azy_event, 4, "depth", "depth", 2)
      plot_mono_grid(dcx_event, azy_event, p1, p2, "depth")
    }
    if(job==5){ # plot average repost levels
      plot_bi(dcx_event, azy_event, 5, "average level", "average_level")
    }
    if(job==5.1){ # plot average repost levels with linear regression
      p1 <- plot_mono(dcx_event, 5, "average level", "average_level",  1) #1 for trainee 1 and 2 for trainee 2
      p2 <- plot_mono(azy_event, 5, "average level", "average_level", 2)
      plot_mono_grid(dcx_event, azy_event, p1, p2, "average_level")
    }
    if(job==6){ # plot logistic parameters
      dcx_para_rm <- dcx_para[!is.na(dcx_para$alpha),] #remove NA's
      azy_para_rm <- azy_para[!is.na(azy_para$alpha),]
      plot_bi(dcx_para_rm, azy_para_rm, 2, "alpha", "alpha")
    }
    if(job==6.1){ # plot logistic parameters with linear regression
      dcx_para_rm <- dcx_para[!is.na(dcx_para$alpha),] #remove NA's
      azy_para_rm <- azy_para[!is.na(azy_para$alpha),]
      p1 <- plot_mono(dcx_para_rm, 2, "alpha", "alpha",  1) #1 for trainee 1 and 2 for trainee 2
      p2 <- plot_mono(azy_para_rm, 2, "alpha", "alpha", 2)
      plot_mono_grid(dcx_para_rm, azy_para_rm, p1, p2, "alpha")
    }
    if(job==7){ # plot booster numbers
      plot_bi(dcx_booster, azy_booster, 2, "number of boosters", "booster_number")
    }
    if(job==7.1){ # plot booster numbers with linear regression
      p1 <- plot_mono(dcx_booster, 2, "number of boosters", "booster_number",  1) #1 for trainee 1 and 2 for trainee 2
      p2 <- plot_mono(azy_booster, 2, "number of boosters", "booster_number", 2)
      plot_mono_grid(dcx_booster, azy_booster, p1, p2, "booster_number")
    }
    if(job==8){ # plot maximal sock puppets numbers
      plot_bi(dcx_booster, azy_booster, 4, "maximal number of sps", "sp_number")
    }
    if(job==8.1){ # plot maximal sock puppets numbers with linear regression
      p1 <- plot_mono(dcx_booster, 4, "maximal number of sps", "sp_number",  1) #1 for trainee 1 and 2 for trainee 2
      p2 <- plot_mono(azy_booster, 4, "maximal number of sps", "sp_number", 2)
      plot_mono_grid(dcx_booster, azy_booster, p1, p2, "sp_number")
    }
    
  }

  #lm_fit <- lm(repost_number ~ index, data = azy_event)
  #summary(lm_fit)
  # save predictions of the model in the new data frame 
  # together with variable you want to plot against
  #predicted <- data.frame(index = azy_event$index, repost_number_pred = predict(lm_fit, azy_event))
  # this is the predicted line of multiple linear regression
  #ggplot(data = azy_event,aes(x=index,y=repost_number)) + 
    #geom_point(color='pink', size = 1) +
    #geom_line(color='pink', data = predicted, aes(x=index, y=repost_number_pred))
