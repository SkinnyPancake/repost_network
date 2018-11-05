# load libraries
library(tidyverse)
library(dplyr)
library(quantmod)
library(ggpubr)
library(cowplot)
library(igraph)
library(data.table)
library(plyr)
# clear variables and functions
#rm(list=ls())
folder <- "/Users/xingruchen/Dropbox/Sina_weibo/data/" # path to folder that holds multiple .csv files
list_id <- c("5781292544", "5781311106", "6290114447", "5908064369", "6292617784",
             "5780592127", "6015189526", "6224699669", "5902696506", "6177367279") # user ids of the trainees
booster_id <- c("5359685902", "aoziyifanclub") # user ids of the boosters
folder_f <- "/Users/xingruchen/Dropbox/Sina_weibo/manuscript/" # path to folder that holds multiple figure files
interval <- c(days(1), days(3), days(7), days(14), days(21), days(28)) # cutoff time interval
interval_folder <- c("1d/", "3d/", "7d/", "14d/", "21d/", "28d/")
interval_name <- c("1d", "3d", "7d", "14d", "21d", "28d")
object_folder <- c("child/", "dust/", "general/", "pair/", "self/") # observed objects
object_name <- c("child", "dust", "general", "pair", "self")
interval_length <- c(1440, 4320, 10080, 20160, 30240, 40320)
object_text <- c("repost pattern of individuals", "repost pattern of dusts", "general repost pattern", "repost pattern of pairs", "repost pattern of self reposts")
interval_text <- c("(first day)", "(first three days)", "(first week)", "(first two weeks)", "(first three weeks)", "(first four weeks)")

########################5 & 8 event based; 11 & 12 (name)/10 & 3 (id) user based########################
################################################
########################functions description########################
###1###repost_read######create a function to read csv files######
###2###repost_rename######create a function to rename columns######
###3###repost_degree_count######create a function to count the weighted indegree and outdegree######
###4###repost_index######create a function to get the indices of users in chronological sequence of reposts######
###5###repost_chrome_0######create a function to get the reposts of two posts marked with chromes from two trainees######
###6###repost_chrome######create a function to get the reposts of two posts marked with chromes from two trainees with weighted degree######
########################functions description########################
################################################

########################create a function to read csv files########################
repost_read <- function(name, date){
  trainee<-read.csv(paste(folder, name, "_", date, ".csv", sep=''))
  return(trainee)
}
############create a function to for the pre-process of data############
repost_pre <- function(trainee){
  trainee$user_id <- as.character(trainee$user_id); trainee$father_weibo_user_id <- as.character(trainee$father_weibo_user_id) # change data type from factor to character
  trainee$pub_time <- as.POSIXct(trainee$pub_time, tz = "HongKong") # change data type to datetime with the corresponding timezone
  trainee <- trainee[order(trainee$pub_time),] # arrange in the time order
  return(trainee)
}
########################create a function to rename columns########################
repost_rename <- function(trainee){
  colnames(trainee)[10] <- "source" #change the column names for gephi to recognize
  colnames(trainee)[3] <- "target" #5 & 8 event based; 11 & 12 (name)/3 & 10 (id) user based
  return(trainee)
}
########################create a function to get the reposts of a post########################
repost_single0 <- function(name, post_date){
  trainee <- repost_read(name, post_date) 
  trainee <- repost_rename(trainee)
  write.csv(trainee,
            file = paste(folder, "R/general/", name, "_", post_date, ".csv", sep = ""), #an extra "w" to represent the feature of weighted degree
            row.names = FALSE)
}
########################create a function to get the reposts of a post with weighted degree and identification for boosters, sock puppets and normal fans########################
repost_single <- function(name, post_date, color, k){
  trainee <- repost_read(name, post_date)
  trainee <- repost_pre(trainee)
  trainee <- subset(trainee, pub_time <= trainee$pub_time[1] + interval[k]) # only consider reposts published within the interval
  trainee <- repost_rename(trainee)
  node <- trainee[,c("target","user_name")]
  node <- node[!duplicated(node$target),]
  colnames(node)[1] <- "id"
  degree <- repost_degree_count(trainee)
  node <- merge(node, degree, by="id") #get the node list with weighted degree
  names(node) <- c("id", "label", "in", "out") #in for weighted in degree and out for weighted out degree
  booster_sp <- booster_sockpuppet_00(trainee, degree)
  node$color <- ifelse(node$id %in% booster_sp, color[2], color[3]) #give different types to boosters, sock puppets and normal fans
  node[node$id %in% list_id, ]$color <- color[1]
  node <- na.omit(node)
  trainee <- trainee[!(trainee$father_weibo_id=="ROOT"),] #remove the root
  trainee <- trainee[order(as.POSIXct(trainee$pub_time)),]
  node$first_pub_time <- trainee$pub_time[match(node$id, trainee$target)] # add timestamp
  node$label <- as.character(node$label)
  node[node$out < nrow(trainee)/1000, ]$label <- ""
  write.csv(trainee,
            file = paste(folder, "R/general/", name, "_", post_date, "_w.csv", sep = ""), #an extra "w" to represent the feature of weighted degree
            row.names = FALSE)
  write.csv(node,
            file = paste(folder, "R/general/", name, "_", post_date, "_w_node.csv", sep = ""),
            row.names = FALSE)
}
########################create a function to extract all the dates###########################
repost_date <- function(filelist){
  l <- length(filelist) #extract the number of data frames
  post_date <- vector("character", l)   #create a vector to store the date
  class(post_date) <- "Date"
  for (i in 1:l) {post_date[i] <- as.Date(filelist[[i]][1, ]$pub_time)}
  post_date <- data.frame(post_date)
  return(post_date)
}
########################create a function to count the weighted indegree and outdegree########################
repost_degree_count <- function(trainee){
  #degrees calculation, considering multiple edges
  trainee <- trainee[!(trainee$father_weibo_id=="ROOT"),] #remove the root
  outdegree<- as.data.frame(table(trainee$source))
  colnames(outdegree)<- c("id", "out_degree")
  indegree<-as.data.frame(table(trainee$target))
  colnames(indegree)<- c("id", "in_degree")
  #merge the two datasets
  degree <- merge(indegree, outdegree, by = c("id"), all = TRUE)
  degree[is.na(degree)] <- 0
  degree <- degree[order(-degree$out_degree, -degree$in_degree),]
  return(degree)
}
########################create a function to get the indices of users in chronological sequence of reposts########################
repost_index <- function(trainee){
  trainee_node <- as.data.frame(unique(trainee$target))
  trainee_node$index<- seq.int(nrow(trainee_node))
  return(trainee_node)
}
########################create a function to get the reposts of two posts marked with chromes from two trainees########################
repost_chrome_0 <- function(trainee_1, trainee_2){
  mono_1 <- anti_join(trainee_1,trainee_2, by = "target") #extract monochrome reposts for trainee 1
  mono_2 <- anti_join(trainee_2,trainee_1,by = "target") #extract monochrome reposts for trainee 2
  couple <- rbind(setdiff(trainee_1, mono_1), setdiff(trainee_2, mono_2)) #extract bichrome reposts
  #couple_1 <- subset(trainee_1, !(target %in% mono_1$target)) #another method
  #couple_2 <- subset(trainee_2, !(target %in% mono_2$target))
  mono_1$chrome<- "dcx" #set chrome value
  mono_2$chrome<- "azy"
  couple$chrome<- "da"
  bi <- rbind(mono_1,mono_2,couple)
  return(bi)
}
########################create a function to get the reposts of two posts marked with chromes from two trainees with weighted degree########################
repost_chrome <- function(trainee_1, trainee_2, date){
  mono_1_node <- as.data.frame(unique(anti_join(trainee_1,trainee_2, by = "target")$target))
  colnames(mono_1_node)[1] <- "target" #get monochrome node for trainee 1
  mono_2_node <- as.data.frame(unique(anti_join(trainee_2,trainee_1, by = "target")$target))
  colnames(mono_2_node)[1] <- "target" #get monochrome node for trainee 2
  couple_node <- as.data.frame(unique(subset(trainee_1, !(target %in% mono_1_node$target))$target))
  #couple_node <- setdiff(trainee_1_node, mono_1_node) #another method
  colnames(couple_node)[1] <- "target"
  mono_1_node$chrome<- "dcx" #set chrome value
  mono_1_node$color <- "#6495ED" #set color value # cornflower blue
  mono_2_node$chrome <- "azy"
  mono_2_node$color <- "#FFC0CB" # pink
  couple_node$chrome <- "da"
  couple_node$color <- "#32CD32" # lime green
  bi_node <- rbind(mono_1_node, mono_2_node, couple_node) #get the node list
  bi <- rbind(trainee_1, trainee_2)
  bi <- merge(bi, bi_node, by = c("target"), all = TRUE) #get the edge list
  colnames(bi_node)[1] <- "id"
  bi_degree <- repost_degree_count(bi)
  bi_degree <- merge(bi_degree, bi_node, all = TRUE) #get the node list with weighted degree
  bi_degree <- bi_degree[order(-bi_degree$out_degree, -bi_degree$in_degree),] 
  names(bi_degree) <- c("id","in", "out", "chrome", "color") #in for weighted in degree and out for weighted out degree
  bi_degree <- na.omit(bi_degree)
  bi <- bi[!(bi$father_weibo_id=="ROOT"),] #remove the root
  write.csv(bi,
            file = paste(folder, "R/chrome/da_", date, "_w.csv", sep = ""), #an extra "w" to represent the feature of weighted degree
            row.names = FALSE)
  write.csv(bi_degree,
            file = paste(folder, "R/chrome/da_", date, "_w_node.csv", sep = ""),
            row.names = FALSE)
}
########################create a function to find boosters and sock puppets then return the list of boosters and sock puppets########################
booster_sockpuppet_00 <- function(trainee, degree){
  trainee <- trainee[!(trainee$father_weibo_id=="ROOT"),]
  edges <- trainee[, c("source","target")] #extract edge list
  g_trainee <- graph_from_data_frame(edges, directed = TRUE, vertices = NULL) #create graph
  E(g_trainee)$weight <- count_multiple(g_trainee) #remove mulitple edges but keep multiplicity
  g_trainee <- simplify(g_trainee)
  #include (or exclude) the trainee himself
  booster_sp <- as.vector(degree$id[degree$out_degree >= sqrt(nrow(trainee))]) #the threshold for the out degree of a booster
  n <- length(booster_sp) #find the descendents
  #put the descendents in the degree data frame
  for (i in 1:n){
    desced <- tail(subcomponent(g_trainee, booster_sp[i], mode = "out"),-1)
    if (!(booster_sp[i] %in% list_id) & length(desced) > 0){
      booster_sp <- c(booster_sp, as.vector(desced$name))}
    }
  return(booster_sp)
}
########################create a function to find boosters and sock puppets########################
booster_sockpuppet_0 <- function(trainee, degree){
  trainee <- trainee[!(trainee$father_weibo_id=="ROOT"),]
  edges <- trainee[, c("source","target")] #extract edge list
  g_trainee <- graph_from_data_frame(edges, directed = TRUE, vertices = NULL) #create graph
  E(g_trainee)$weight <- count_multiple(g_trainee) #remove mulitple edges but keep multiplicity
  g_trainee <- simplify(g_trainee)
  #exclude the trainee himself
  booster <- as.vector(degree$id[degree$out_degree >= sqrt(nrow(trainee))]) #the threshold for the out degree of a booster
  n <- length(booster) #find the descendents
  #put the descendents in the degree data frame
  for (i in 1:n){
    desced <- tail(subcomponent(g_trainee, booster[i], mode = "out"),-1)
    if (!(booster[i] %in% list_id)){
      degree[i,4] <- toString(desced$name)
      degree[i,5] <- length(desced$name)}
    else {
      degree[i,4] <- "NULL"
      degree[i,5] <- -1
    }
  }
  colnames(degree)<- c("id", "in_degree", "out_degree", "sock_puppet", "no_of_sp")
  return(degree)
}
########################create a function to get the boosters marked with chromes from two trainees########################
repost_chrome_booster <- function(trainee_1, trainee_2){
  degree_1 <- booster_sockpuppet_0(trainee_1, repost_degree_count(trainee_1))
  booster_1 <- degree_1[degree_1$no_of_sp>=0,]$id
  booster_1 <- booster_1[!is.na(booster_1)] # ?
  degree_2 <- booster_sockpuppet_0(trainee_2, repost_degree_count(trainee_2))
  booster_2 <- degree_2[degree_2$no_of_sp>=0,]$id
  booster_2 <- booster_2[!is.na(booster_2)]
  bi_booster_1 <- sum(booster_1 %in% trainee_2$target)
  bi_booster_2 <- sum(booster_2 %in% trainee_1$target)
  return(c(nrow(degree_1), nrow(degree_2), length(booster_1), length(booster_2), bi_booster_1, bi_booster_2))
}
########################create a function to convert filenames to date########################
filename_to_date <- function(filenames){ # dcx_171127.csv to 2017/11/27
  date <- substring(filenames, 5) # remove prefix dcx_171127.csv to 171127.csv
  date <- gsub('.{4}$', '', date) # reomve suffix 171127.csv to 171127
  date <- as.Date(date, "%y%m%d")
}
k = 4
########################main function########################
gephi <- function(job=0.0){
  name <- c("dcx", "azy"); post_date <- "151212"; usdate <- "12/12/15";
  filenames1 <- list.files(path=folder, pattern="dcx_1+.*csv") #*.csv #.*csv # get the file names "dcx_151212.csv" etc.
  dcx_date_csv <- str_sub(filenames1,5,10) # get post pates '151212' etc.
  filenames2 <- list.files(path=folder, pattern="azy_1+.*csv") # get filenames "azy_151212.csv" etc.
  azy_date_csv <- str_sub(filenames2,5,10) # get post pates "151212" etc.
  color_1 <- c("#3399FF", "#FF6666", "#99CCFF")
  color_2 <- c("#FF3333", "#66B2FF", "#FF9999")
  #color_2 <- c("#FFCCCC", "#CCE5FF", "#FFE5CC")
  if(job==0.0){ #save the original csv file as a new dataframe for gephi plot
    #repost_single0(name[1], post_date)
    #repost_single0(name[2], post_date)
    repost_single("dcx", "151212", color_1, k)
    sapply(dcx_date_csv, repost_single, name="dcx", color=color_1, k=k)
    sapply(azy_date_csv, repost_single, name="azy", color=color_2, k=k)
  }
 if(job==0.1){ #save the orignial csv file as a new dataframe with timestamp for dynamic gephi plot
    trainee <- repost_read(name[1], post_date) #1 for dcx and 2 for azy
    trainee <- repost_rename(trainee)
    trainee <- trainee[order(as.Date(trainee$pub_time)),] #order the reposts in chronological sequence
    degree_trainee <- repost_degree_count(trainee)
    names(degree_trainee) <- c("id","in", "out")
    degree_trainee$first_pub_time <- trainee$pub_time[match(degree_trainee$id, trainee$target)]
    write.csv(trainee,
              file = paste(folder, "R/dynamic/", name[1], "_", post_date, "_d.csv", sep = ""), #a extra "d" to represent the feature of dynamic.
              row.names = FALSE)
    write.csv(degree_trainee,
              file = paste(folder, "R/dynamic/", name[1], "_", post_date, "_d_node.csv", sep = ""), #a extra "d" to represent the feature of dynamic.
              row.names = FALSE)
  }
  if(job==0.2){ #match user name with index
    trainee <- repost_read(name[1], post_date) #1 for dcx and 2 for azy
    trainee <- repost_rename(trainee)
    trainee <- trainee[order(as.Date(trainee$pub_time)),] #order the reposts in chronological sequence
    trainee_index <- repost_index(trainee)
    colnames(trainee_index)[1] <- "user_id"
    #match user_id with index
    trainee$index <- trainee_index$index[match(trainee$target, trainee_index$user_id)]
    trainee$father_index <- trainee_index$index[match(trainee$source, trainee_index$user_id)]
    trainee_new <- subset(trainee, select=c("pub_time", "index","father_index"))
    #save the index oriented file
    #write.csv(trainee_new,
    #file = paste(folder, "R/G", name[1],"_",post_date, "_new.csv", sep = ""), 
    #row.names = FALSE)
    in_degree <- as.data.frame(table(trainee_new$index))
    out_degree <- as.data.frame(table(trainee_new$father_index))
    colnames(in_degree)<- c("index", "in_degree")
    colnames(out_degree)<- c("index", "out_degree")
    degree <- merge(in_degree, out_degree, by = c("index"), all = TRUE)
    degree[is.na(degree)] <- 0
    ggplot(degree, aes(x=index, y=in_degree)) + geom_point()
    ggplot(degree, aes(x=index, y=out_degree)) + geom_point()
    degree <- degree[order(-degree$out_degree, -degree$in_degree),]
    ggplot(degree[which(degree$out_degree >= sqrt(nrow(trainee) - 1)),], aes(x=as.integer(index), y=out_degree)) + geom_point() + 
      scale_x_continuous(name = "index",limits = c(0, nrow(degree))) + scale_y_continuous(name = "weighted out degree") + 
      geom_text(aes(label= index),vjust = 0, nudge_y = 200)
    }
  if(job==1.0){ #save chrome reposts (the reposts of two posts from two trainees) as a new dataframe
    trainee <- lapply(name, repost_read, date = post_date)
    trainee <- lapply(trainee, repost_rename)
    bi <- repost_chrome_0(trainee[[1]], trainee[[2]])
    write.csv(bi,
              file = paste(folder, "R/chrome/da_", post_date, ".csv", sep = ""),
              row.names = FALSE)
  }
  if(job==1.1){ #save chrome reposts with weighted degree
    trainee <- lapply(name, repost_read, date = post_date)
    trainee <- lapply(trainee, repost_rename)
    repost_chrome(trainee[[1]], trainee[[2]], post_date)
  }
  if(job==2.0){ #consider the number of monochrome and bichorme boosters
    filenames1 <- list.files(path=folder, pattern="dcx_1+.*csv") #*.csv #.*csv
    filenames2 <- list.files(path=folder, pattern="azy_1+.*csv")
    dcx_date <- filename_to_date(filenames1)
    azy_date <- filename_to_date(filenames2)
    date <- intersect(dcx_date, azy_date)
    date <- as.Date(as.numeric(date))
    date_file <- gsub("-", "", date)
    date_file <- substring(date_file, 3)
    filelist1_sub <- lapply(paste0("dcx_", date_file, ".csv"), read.csv)
    filelist1_sub <- lapply(filelist1_sub, repost_rename)
    filelist2_sub <- lapply(paste0("azy_", date_file, ".csv"), read.csv)
    filelist2_sub <- lapply(filelist2_sub, repost_rename)
    chrome_booster <- matrix( , nrow = length(date), ncol = 6)
    for(i in 1:length(date)){
      chrome_booster[i,] <- repost_chrome_booster(filelist1_sub[[i]], filelist2_sub[[i]])
      }
    colnames(chrome_booster) <- c('fan_number_1', 'fan_number_2','booster_number_1',
                                  'booster_number_2','bi_booster_number_1', 'bi_booster_number_2')
    chrome_booster <- data.frame(date, chrome_booster)
    View(chrome_booster)
    }
  }