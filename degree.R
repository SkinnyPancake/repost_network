#load libraries
library(tidyverse)
library(plotly)
library(pander)
library(ggpubr)
library(igraph)
library(dplyr)
library(network)
library(sna)
library(ggplot2)
library(GGally)
library(RColorBrewer)
library(viridis)
library(gridExtra)
# clear variables and functions
#rm(list=ls())

folder <- "/Users/xingruchen/Dropbox/Sina_weibo/data/" # path to folder that holds multiple .csv files
folder_f <- "/Users/xingruchen/Dropbox/Sina_weibo/manuscript/" # path to folder that holds multiple figures
folder_sub <- "/Users/xingruchen/Dropbox/Sina_weibo/data/R/sub/" # path to folder that holds multiple .csv files for sub networks
list_id <- c("5781292544", "5781311106", "6290114447", "5908064369", "6292617784",
             "5780592127", "6015189526", "6224699669", "5902696506", "6177367279") # user ids of the trainees
booster_id <- c("5359685902", "aoziyifanclub") # two booster ids for trainee 1 and trainee 2, respectively
# the first id is that of trainee 1 and the second is that of trainee 2

interval <- c(days(1), days(3), days(7), days(14), days(21), days(28)) # cutoff time interval
interval_tag <- c("1d/", "3d/", "7d/", "14d/", "21d/", "28d/")
interval_name <- c("1d", "3d", "7d", "14d", "21d", "28d")

title_size <- c(18, 28); axis_size <- c(16, 26)
################################################
########################functions description########################
###1###repost_read######create a function to read csv files######
###2###repost_degree_count######create a function to count indegree and outdegree######
###3###plot_hist_0######create a function to plot histogram######
###4###plot_hist_1######create a function to plot histogram with sqrt y axis######
###5###booster_sockpuppet_0######create a function to find boosters and sock puppets######
###6###booster_sockpuppet######create a function to find boosters and sock puppets with gephi dataframe exported######
########################functions description########################

################################################
########################create a function to read csv files########################
repost_read <- function(name, date){
  trainee <- read.csv(paste(folder, name, "_", date, ".csv", sep=''))
  return(trainee)
}

############create a function to for the pre-process of data############
repost_pre <- function(trainee){
  trainee$user_id <- as.character(trainee$user_id); trainee$father_weibo_user_id <- as.character(trainee$father_weibo_user_id) # change data type from factor to character
  trainee$pub_time <- as.POSIXct(trainee$pub_time, tz = "HongKong") # change data type to datetime with the corresponding timezone
  trainee <- trainee[order(trainee$pub_time),] # arrange in the time order
  return(trainee)
}

########################create a function to extract all the dates###########################
repost_date <- function(filelist){
  l <- length(filelist) # count the number of data frames
  post_date <- vector("character", l)   # create a vector to store the date
  class(post_date) <- "Date"
  for (i in 1:l) {post_date[i] <- as.Date(filelist[[i]][filelist[[i]]$father_weibo_id=="ROOT", ]$pub_time)}
  post_date <- data.frame(post_date)
  return(post_date)
}

########################create a function to count the number of self-repost###########################
self_repost <- function(trainee){
  trainee <- trainee[!(trainee$father_weibo_id=="ROOT"),] # remove the original tweet
  no_self_repost <- sum(as.character(trainee$user_id)==as.character(trainee$father_weibo_user_id))
  return(no_self_repost/nrow(trainee))
}

########################create a function to count indegree and outdegree (include the number of self reposts)########################
repost_degree_count <- function(trainee){
  trainee <- trainee[!(trainee$father_weibo_id=="ROOT"),]
  indegree<- as.data.frame(table(trainee$user_id)) # degrees calculation, consider multiple edges
  colnames(indegree) <- c("id", "in_degree")
  outdegree<-as.data.frame(table(trainee$father_weibo_user_id))
  colnames(outdegree) <- c("id", "out_degree")
  degree <- merge(indegree, outdegree, by = "id", all = TRUE) #merge the two datasets
  degree[is.na(degree)] <- 0
  degree <- degree[order(-degree$out_degree, -degree$in_degree),]
  degree$self_repost <- 0
  self <- as.character(trainee$user_id)==as.character(trainee$father_weibo_user_id) # find the self reposts
  for (i in 1:nrow(degree)){
    id <- as.character(degree$id[i])
    degree$self_repost[i] <- sum(as.character(trainee$user_id[self])==id) # count the number of self reposts for the ith id
  }
  return(degree)
}

########################create a function to plot histogram########################
plot_hist_0 <- function(indegree, outdegree){
  par(mfrow=c(1,2), mar=c(2,2,1,1)) 
  in_h <- hist(indegree, breaks = 100, col = "pink",
               xlab = "in degrees",
               main = "Histogram for in degree")
  out_h <- hist(outdegree, breaks = 50, col = "cornflowerblue",
                xlab = "out degree",
                main = "Histogram for out degree")
}

########################create a function to plot histogram with sqrt y axis########################
plot_hist_1 <- function(degree, name, usdate){
  p_in <- ggplot(degree, aes(x = in_degree)) + 
    geom_histogram(binwidth = 100, color = "white", fill = "pink") + scale_y_sqrt() +
    labs(x = "in degree", y = "square root of count", title = "Histogram for in degree") +
    theme_bw()
    annotate_figure(p_in,
                    #top = text_grob("couple mpg", color = "black", face = "bold", size = 14),
                    bottom = text_grob(paste("Data source: ", name, "Post date: ", usdate, sep=' '), 
                                       color = "black", just = "centre", hjust = 1.5, x = 1, face = "plain", size = 10))
    p_out <- ggplot(degree, aes(x = out_degree)) + 
      geom_histogram(binwidth = 100, color = "white", fill = "cornflowerblue") + scale_y_sqrt() +
      labs(x = "out degree", y = "square root of count", title = "Histogram for out degree") +
      theme_bw()
    annotate_figure(p_out,
                    #top = text_grob("couple mpg", color = "black", face = "bold", size = 14),
                    bottom = text_grob(paste("Data source: ", name, "Post date: ", usdate, sep=' '), 
                                       color = "black", just = "centre", hjust = 1.5, x = 1, face = "plain", size = 10))
    ggarrange(p_in, p_out, labels = c("A", "B"), ncol = 1, nrow = 2)
}

########################create a function to find boosters and sock puppets########################
booster_sockpuppet_0 <- function(trainee, degree){
  trainee <- trainee[!(trainee$father_weibo_id=="ROOT"),]
  edges <- trainee[, c("father_weibo_user_id","user_id")] # extract edge list
  g_trainee <- graph_from_data_frame(edges, directed = TRUE, vertices = NULL) # create graph
  E(g_trainee)$weight <- count_multiple(g_trainee) # remove mulitple edges but keep multiplicity
  g_trainee <- simplify(g_trainee)
  # exclude the trainee himself
  booster <- as.vector(degree$id[degree$out_degree >= sqrt(nrow(trainee))]) # the threshold for the out degree of a booster
  n <- length(booster) # find the descendents
  # put the descendents in the degree data frame
  for (i in 1:n){
    desced <- tail(subcomponent(g_trainee, booster[i], mode = "out"),-1)
    if (length((desced$name)) & !(booster[i] %in% list_id)){
      degree[i,5] <- toString(desced$name)
      degree[i,6] <- length(desced$name)}
    else {
      degree[i,5] <- NULL
      degree[i,6] <- 0}
  }
  colnames(degree)<- c("id", "in", "out", "self_repost", "sock_puppet", "no_of_sp")
  return(degree)
}

########################create a function to find boosters and sock puppets with gephi dataframe exported########################
booster_sockpuppet <- function(trainee, degree, name, date, tag, k){ 
  # trainee <- read.csv("azy_171127.csv"); degree <- repost_degree_count(trainee); name <- "azy"; date <- "171127"; tag <- 2
  #trainee_0 <- trainee[order(as.POSIXct(trainee$pub_time, tz="HongKong")),] # save the original dataframe
  trainee_0 <- subset(trainee, pub_time <= trainee$pub_time[1] + interval[k]) # only consider reposts published within the interval
  trainee <- trainee_0
  trainee <- trainee[!(trainee$father_weibo_id=="ROOT"),]
  node <- trainee_0[,c("user_id","user_name")] # create a dataframe for the nodes
  node <- node[!duplicated(node$user_id),]
  colnames(node) <- c("id", "label")
  edges <- trainee[, c("father_weibo_user_id","user_id")] # extract edge list
  g_trainee <- graph_from_data_frame(edges, directed = TRUE, vertices = NULL) # create graph
  E(g_trainee)$weight <- 1 # remove mulitple edges but keep multiplicity
  g_trainee <- simplify(g_trainee, remove.loops = FALSE, edge.attr.comb=list(weight="sum"))
  degree <- merge(node, degree, by="id") # get the node list with weighted degree
  degree <- degree[order(-degree$out_degree, -degree$in_degree),]
  degree$booster <- 0 # add booster tag
  degree$color <- "#000000" # add color tag
  booster <- as.vector(degree$id[degree$out_degree >= sqrt(nrow(trainee))]) # find the boosters
  n <- length(booster)
  if(tag==1){color <- colorRampPalette(c("#2171b5", "#FFFFFF", "#cb181d"))(n + 1)
  }else{color <- colorRampPalette(c("#cb181d", "#FFFFFF", "#2171b5"))(n + 1)}
  if("#FFFFFF" %in% color){color <- color[-which(color == "#FFFFFF")] # remove the color 'white'
  }else{color <- color[-length(color)]}
  subnode <- list_id[tag] # create the vector of subnodes, first include the trainee himself
  degree$booster[which(degree$id %in% list_id)] <- 1
  degree$color[which(degree$id %in% list_id)] <- color[which(degree$id %in% list_id)]
  # put the descendents in the degree dataframe and extract nodes of boosters as well as
  # the corresponding sock puppets 
  # include the trainee himself
  for (i in 1:n){
    desced <- subcomponent(g_trainee, booster[i], mode = "out")$name
    #desced <- tail(desced, -1) # exculde the booster himself/herself
    if (length((desced)) & !(booster[i] %in% list_id)){
      subnode <- append(subnode, desced)
      V(g_trainee)$booster[which(V(g_trainee)$name %in% desced)] <- i # with updated booster tag
      degree$booster[which(degree$id %in% desced)] <- i # with updated booster tag
      degree$color[which(degree$id %in% desced)] <- color[i] # with updated color tag
      #V(g_trainee)$booster[which(V(g_trainee)$name %in% desced & V(g_trainee)$booster == NA)] <- i
      #degree$booster[which(degree$id %in% desced & !degree$booster)] <- i # do not allow the latter one to cover the former one
      degree[i,8] <- toString(desced)
      degree[i,9] <- length(desced)-1
    }
    else {
      degree[i,8] <- NA
      degree[i,9] <- 0
      }
  }
  colnames(degree)<- c("id", "label", "in", "out", "self_repost", "booster", "color", "sock_puppet", "no_of_sp")
  # plot the subgraph consisting of the trainee, his boosters and their sock puppets
  #pal <- rainbow(n, alpha=.5) 
  #g_s <- induced_subgraph(g_trainee, subnode)
  #V(g_s)$type <- 2; V(g_s)$type[which(V(g_s)$name %in% booster)] <- 1
  #g_s <- simplify(g_s, remove.loops = T)
  #V(g_s)$size <- 3; V(g_s)$frame.color <- "white"; V(g_s)$color <- pal[V(g_s)$booster]; V(g_s)$color[1] <- "black"
  #V(g_s)$shape <- c("square", "circle")[V(g_s)$type]; V(g_s)$label <- ""
  #E(g_s)$width <- 1+E(g_s)$weight/250; E(g_s)$arrow.size <- .05
  #plot(g_s, layout = layout_with_graphopt)
  # export gephi dataframe
  trainee_sub <- filter(trainee_0, trainee_0$user_id %in% subnode)
  trainee_sub <- filter(trainee_sub, trainee_sub$father_weibo_user_id %in% subnode) # delete it or not?
  colnames(trainee_sub)[10] <- "source"
  colnames(trainee_sub)[3] <- "target"
  degree_sub <- filter(degree, degree$id %in% subnode)
  trainee_sub <- trainee_sub[order(as.POSIXct(trainee_sub$pub_time, tz = "Hongkong")),] # order from old reposts to new reposts
  degree_sub$first_repost_time <- trainee_0$pub_time[match(degree_sub$id, trainee_0$user_id)] # add timestamp for nodes in the sub network
  degree_sub$first_repost_time[degree_sub$id %in% list_id] <- trainee_0$pub_time[trainee_0$user_id %in% list_id] # add timestamp for the post
  write.csv(trainee_sub,
  file = paste(folder, "R/sub/",name,"_",date,"_sub.csv", sep = ""),
  row.names = FALSE)
  write.csv(degree_sub,
  file = paste(folder, "R/sub/",name,"_",date,"_sub_node.csv", sep = ""),
  row.names = FALSE)
  return(degree)
}

########################create a function to perform batch process to booster_sockpuppet()########################
booster_sockpuppet_batch <- function(name, tag, k){
  filenames <- list.files(path=folder, pattern=paste0(name, "_1+.*csv")) #*.csv #.*csv
  filelist <- lapply(filenames, read.csv)
  filelist <- lapply(filelist, repost_pre) # newly added
  date <- repost_date(filelist)
  post_date <- substring(str_replace_all(as.vector(date)[,1], "[^[:alnum:]]", ""), 3)
  degree <- lapply(filelist, repost_degree_count) # weighted degree
  for(i in 1:length(filenames)){
    degree_bsp <- booster_sockpuppet(filelist[[i]], degree[[i]], name, post_date[i], tag, k)
  }
}
########################create a function to prepare date for print: 15-12-12 to 12/12/15###########################
date_print <- function(date)
{
  date <- format(as.Date(date, format = "%Y-%m-%d"), "%m/%d/%Y")
  date <- as.character(date)
}

########################create a function to record the statistics for a single post###########################
repost_record <- function(trainee, filelist, date){
  trainee_r <- trainee # r for record
  l <- length(filelist) # the total number of posts
  weights = 1/2^(0:(l-1)) # 1, 1/2, 1/4, ...
  if(nrow(trainee) > 0){ # if the dataframe trainee is non empty
    trainee_r$post_date <- date
    if(nrow(trainee) > 1){
      record <- as.data.frame(sapply(seq_along(filelist), # record whether an account reposted the ith post
                                     function(i,x) {trainee$id %in% x[[i]]$id}, x=filelist))
      colnames(record) <- 1:ncol(record)
      indices <- sapply(1:nrow(record), function(i) which(record[i,]==TRUE)) 
      trainee_r$indices <- indices # the indices of posts that the booster reposts (the column is a list with every element a vector)
    }else{ # in case we only have 1 booster
      record <- as.data.frame(sapply(seq_along(filelist), # record whether an account reposted the ith post
                                     function(i,x) {trainee$id %in% x[[i]]$id}, x=filelist))
      record <- t(record) # dataframe transpose
      colnames(record) <- 1:ncol(record)
      rownames(record) <- 1
      indices <- which(record[1,]==TRUE)
      trainee_r$indices <- vector(mode = "list",length=1) # creat empty column
      trainee_r$indices[[1]] <- indices
    }
    trainee_r[sprintf("in%d", 1:l)] <- record
    trainee_r <- trainee_r %>%
      mutate(n = rowSums(record)) %>% # the number of reposts
      mutate(earliest = max.col(record, "first")) %>% # the first repost
      mutate(latest = max.col(record,"last")) %>% # the last repost
      mutate(t = latest - earliest + 1) %>% ############ calculate the length betwwen the first and last reposts (customer's lifespan) ############
    mutate(clv = n*t/l^2) %>% # calculate the customer lifetime value
      mutate(sumd = rowSums(as.matrix(record) %*% weights)/sum(weights)) %>% # binomial weighted sum
      #mutate(suma = rowSums(as.matrix(record) %*% rev(weights))/sum(weights)) %>% # binomial weighted sum in reverse order
      #mutate(sum_average = 1/2*(sumd + suma)) %>% # average of the previous two binomial sum
      #mutate(sum_min = pmin(sumd, suma)) %>% # the smaller one of the previous two binomial sum
      mutate_if(is.numeric, funs(round(., 4))) # round up
    trainee_r <- trainee_r[,c(8,7,1:4,6,(ncol(trainee_r)-5):ncol(trainee_r),9,5,10:(ncol(trainee_r)-6))] # remove unused columns
  }else{ # the dataframe trainee_r is empty
    trainee_r[1,] <- NA
    trainee_r$post_date <- date
    record <- as.data.frame(rep(NA, l))
    record <- t(record) # dataframe transpose
    colnames(record) <- 1:ncol(record)
    rownames(record) <- 1
    trainee_r$indices <- vector(mode = "list",length=1) # creat empty column
    trainee_r$indices[[1]] <- c(NA)
    trainee_r[sprintf("in%d", 1:l)] <- record 
    trainee_r <- trainee_r %>%
      mutate(n = NA) %>% # the number of reposts
      mutate(earliest = NA) %>% # the first repost
      mutate(latest = NA) %>% # the last repost
      mutate(t = NA) %>% ############ calculate the length betwwen the first and last reposts (customer's lifespan) ############
      mutate(clv = NA) %>% # calculate the customer lifetime value
      mutate(sumd = NA) # binomial weighted sum
    trainee_r <- trainee_r[,c(8,7,1:4,6,(ncol(trainee_r)-5):ncol(trainee_r),9,5,10:(ncol(trainee_r)-6))] # remove unused columns
  }
  return(trainee_r)
}

########################create a function to record the evolution of boosters of a trainee###########################
booster_evo <- function(name, k){
  filenames <- list.files(path=folder_sub, pattern=paste0(name, "_1+.*node.csv"), full.names = TRUE) #*.csv #.*csv # read node dataframes from the folder_sub for sub networks
  post_date <- gsub('.{4}$', '', str_sub(filenames,-19,-10)) # get post pates '151212' etc.
  st_date <- as.Date(post_date, "%y%m%d") # get standard date format '2015-12-12' etc.
  us_date <- format(st_date, "%m/%d/%Y") # get Us date format '12/12/2015' etc.
  filelist <- lapply(filenames, read.csv, check.names=FALSE) # check.names=FALSE 
  # colnames = id, label, in, out, self_repost, booster, color, sock_puppet, no_of_sp, first_repost_time
  filelist <- lapply(filelist, function(x) x[,c(1,3:5,8:ncol(x))]) # remove unused columns
  filelist <- lapply(filelist, function(x) x[!(is.na(x$sock_puppet)),]) # remove the celebrity
  filelist_record <- lapply(seq_along(filelist), function(i) {repost_record(filelist[[i]], filelist, st_date[i])})
  for(i in 1:length(filelist)){repost_record(filelist[[i]], filelist, st_date[i])}
  filelist_record <- do.call("rbind", filelist_record)
  filelist_record_tidy <- filelist_record[,1:15] %>% 
    arrange(earliest, id, post_date)
  filelist_record_tidy_x <- filelist_record_tidy
  filelist_record_tidy_x$indices <- as.character(filelist_record_tidy$indices) # save the indices as characters instead of list
  write.csv(filelist_record_tidy_x,
            file = paste(folder, "R/statistics/", name, "_booster_evo_", interval_name[k], ".csv", sep = ""), row.names = FALSE)
  return(filelist_record_tidy)
}

########################create a function to plot data for one trainee with linear regression###########################
plot_mono <- function(trainee_event, n, variable_name, plot_name, tag){
  first_date = format(as.Date(trainee_event$first_repost_date[1], format = "%Y-%m-%d"), "%m/%d/%Y")
  last_date = format(as.Date(trainee_event$first_repost_date[nrow(trainee_event)], format = "%Y-%m-%d"), "%m/%d/%Y")
  mid = mean(trainee_event[,n])
  shape = c(16, 16) # 16, 18 # the shapes of points
  name = c("dcx", "azy")
  if(tag==1){color = c("#eff3ff","#bdd7e7","#6baed6", "#2171b5")
  }else{color = c("#fee5d9","#fcae91","#fb6a4a","#cb181d")}
  p = ggplot(data = trainee_event,aes(x=index,y=no_of_sp, color = no_of_sp)) +
    scale_shape_discrete(solid=F) +
    geom_point(shape = shape[tag], size = 1.5) +
    scale_color_gradient2(midpoint=mid, low=color[1], mid=color[2],
                          high=color[4], space ="Lab" ) +
    geom_smooth(method = "lm", color = color[3], se = FALSE) + # linear regression
    xlab("post") +
    ylab(variable_name) +
    theme_bw() +
    theme(legend.title=element_blank()) +
    theme(text = element_text(size=15), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
  annotate_figure(p,
                  #top = text_grob("timeline", color = "black", face = "bold", size = 14),
                  bottom = text_grob(paste("Data source: booster ", tag, " repost date: ", as.character(first_date), " to ", as.character(last_date), sep=''), 
                                     color = "black", just = "centre", hjust = 1, x = 1, face = "plain", size = 15))
  ggsave(file=paste0(folder_f, plot_name, "_", name[tag], ".eps"))
  return(p)
}

########################create a function to further process the results of booster_evo() and consider the fanclubs for two trainees as examples###########################
booster_evo_wrap <- function(name, tag, k){
  record_tidy <- booster_evo(name, k) # get the dataframe from booster_evo()
  record_split <- split(record_tidy, with(record_tidy, id), drop = TRUE)
  record_lifespan <- sapply(record_split, nrow) # the lifespans of boosters
  record_length_df <- as.data.frame(record_lifespan) %>% 
    mutate(first_repost_time = sapply(record_split, function(df) df$first_repost_time[1])) %>% 
    mutate(first_repost_year = format(as.POSIXlt(sapply(record_split, function(df) df$first_repost_time[1]), tz = "Etc"), "%Y")) %>% 
    arrange(first_repost_time)
  ggplot(record_length_df, aes(x=record_lifespan, fill=first_repost_year, color=first_repost_year)) + # color=first_repost_year
    geom_histogram(alpha=0.5, position="identity")
  #booster_longest <- record_split[[names(which.max(record_lifespan))]]
  booster_fanclub <- record_split[[booster_id[tag]]]
  booster_fanclub$first_repost_date <- format(as.POSIXlt(booster_fanclub$first_repost_time, tz = "Etc"), "%Y-%m-%d")
  booster_fanclub$index = as.integer(as.Date(as.character(booster_fanclub$post_date)) - as.Date(booster_fanclub$post_date[1])) + 1
  booster_fanclub$actual_index = booster_fanclub$indices[[1]]
  booster_fanclub$n <- NULL
  #booster_fanclub <- booster_fanclub[-which.max(booster_fanclub$no_of_sp),] # remove the row with maixmal number of sock puppets to reduce randomness
  booster_fanclub_x <- booster_fanclub
  booster_fanclub_x$indices <- as.character(booster_fanclub$indices) # save the indices as characters instead of list
  write.csv(booster_fanclub_x,
            file = paste(folder, "R/statistics/", name, "_booster_evo_fanclub_", interval_name[k], ".csv",sep = ""), row.names = FALSE)
  return(booster_fanclub)
}

########################create a function to get and save side-by-side plot###########################
plot_mono_grid <- function(trainee1, trainee2, p1, p2, plot_name){
  date_1_f <- date_print(trainee1$first_repost_date[1]) # extract the date of the first post for trainee 1
  date_1_l <- date_print(trainee1$first_repost_date[nrow(trainee1)]) # extract the date of the last post for trainee 1
  date_2_f <- date_print(trainee2$first_repost_date[1]) # extract the date of the first post for trainee 2
  date_2_l <- date_print(trainee2$first_repost_date[nrow(trainee2)]) # extract the date of the last post for trainee 2
  title1 <- paste("booster 1 repost date: ", date_1_f, " to ", date_1_l)
  title2 <- paste("booster 2 repost date: ", date_2_f, " to ", date_2_l)
  p <- grid.arrange(p1, p2, ncol=1)
  annotate_figure(p,
                  #top = text_grob("timeline", color = "black", face = "bold", size = 14),
                  bottom = text_grob(paste(title1, title2, sep = "\n"), color = "black",
                                     just = "centre", hjust = 1, x = 1, face = "plain", size = 15))
  ggsave(file=paste0(folder_f, plot_name, "_lm.eps"))
}

########################create a function to find one particular booster (the fanclub) and its sock puppets with gephi dataframe exported########################
booster_sockpuppet_single <- function(trainee, degree, name, date, tag){
  trainee_0 <- trainee[order(as.Date(trainee$pub_time)),] # save the original dataframe
  trainee <- trainee[!(trainee$father_weibo_id=="ROOT"),]
  edges <- trainee[, c("father_weibo_user_id","user_id")] # extract edge list
  g_trainee <- graph_from_data_frame(edges, directed = TRUE, vertices = NULL) # create graph
  E(g_trainee)$weight <- 1 # remove mulitple edges but keep multiplicity
  g_trainee <- simplify(g_trainee, remove.loops = FALSE, edge.attr.comb=list(weight="sum"))
  subnode <- booster_id[tag] # create the vector of subnodes, first include the booster himself
  desced <- subcomponent(g_trainee, subnode, mode = "out")$name
  subnode <- append(subnode, desced)
  trainee_sub <- filter(trainee_0, trainee_0$father_weibo_user_id %in% subnode)
  colnames(trainee_sub)[10] <- "source"
  colnames(trainee_sub)[3] <- "target"
  degree_sub <- filter(degree, degree$id %in% trainee_sub$target | degree$id %in% trainee_sub$source)
  trainee_sub <- trainee_sub[order(as.Date(trainee_sub$pub_time)),]
  edges_sub <- trainee_sub[,c("source", "target")]
  g_trainee_sub <- graph_from_data_frame(edges_sub, directed = TRUE, vertices = NULL) #create graph
  E(g_trainee_sub)$weight <- 1 # remove mulitple edges but keep multiplicity
  g_trainee_sub <- simplify(g_trainee_sub, remove.loops = FALSE, edge.attr.comb=list(weight="sum"))
  disc <- as.data.frame(dfs(g_trainee_sub, booster_id[tag], neimode = "out",
               unreachable = TRUE, order = TRUE, order.out = FALSE, father = FALSE,
               dist = TRUE, in.callback = NULL, out.callback = NULL, extra = NULL,
               rho = parent.frame())$dist) # create a dataframe to record the levels of nodes
  colnames(disc) <- "level"
  disc$id <- rownames(disc)
  colnames(degree_sub) <- c("id", "in", "out", "self_repost") 
  degree_sub <- merge(degree_sub, disc, by = "id")
  n <- max(degree_sub$level) + 1
  #if(tag==1){color <- colorRampPalette(c("#2171b5", "#FFFFFF", "#cb181d"))(n + 1)
  #}else{color <- colorRampPalette(c("#cb181d", "#FFFFFF", "#2171b5"))(n + 1)}
  #if("#FFFFFF" %in% color){color <- color[-which(color == "#FFFFFF")] # remove the color 'white' if exists
  #}else{color <- color[-length(color)]}
  color <- c("#fee0d2", colorRampPalette(c("#c7e9c0", "#238b45"))(n-1))
  degree_sub$color <- color[degree_sub$level+1]
  degree_sub$first_repost_time <- trainee_0$pub_time[match(degree_sub$id, trainee_0$user_id)] # add timestamp
  write.csv(trainee_sub,
            file = paste(folder, "R/booster/",name,"_",date,"_boo.csv", sep = ""),
            row.names = FALSE)
  write.csv(degree_sub,
            file = paste(folder, "R/booster/",name,"_",date,"_boo_node.csv", sep = ""),
            row.names = FALSE)
  return(degree_sub)
}


#how to retrive the descendents from a string (two methods)
#s <- toString(desced$name)
#method 1
# t <- strsplit(s, ",")
# t[[1]][2]
#method 2
#t <- unlist(strsplit(s, ","))
#t[2]

k = 4
########################main function########################
repost_degree <- function(job=0.0){
  name <- c("dcx", "azy"); post_date <- "180708"; usdate <- "07/08/18";
  trainee_1 <- repost_read(name[1], post_date)
  degree_1 <- repost_degree_count(trainee_1) # weighted degree
  freq_self_repost_1 <- self_repost(trainee_1)
  trainee_2 <- repost_read(name[2], post_date)
  degree_2 <- repost_degree_count(trainee_2) # weighted degree
  freq_self_repost_2 <- self_repost(trainee_2)
  if(job==0.0){ # plot histograms of in degree and out degree
    plot_hist_0(degree_1$in_degree, degree_1$out_degree)
  }
  if(job==0.1){ # plot histograms of in degree and out degree with sqrt y axis
    plot_hist_1(degree_1, name[1], usdate)
  }
  if(job==1.0){ # find boosters and sock puppets
    degree_bsp_0 <- booster_sockpuppet_0(trainee_1, degree_1)
    return(degree_bsp_0)
  }
  if(job==1.1){ # find boosters and sock puppets and export gephi dataframe
    degree_bsp_1 <- booster_sockpuppet(trainee_1,degree_1,name[1],post_date, 1)
    degree_bsp_2 <- booster_sockpuppet(trainee_2,degree_2,name[2],post_date, 2)
    return(degree_bsp_1)
  }
  if(job==1.2){ # batch process
    booster_sockpuppet_batch("dcx", 1, k)
    booster_sockpuppet_batch("azy", 2, k)
  }
  if(job==2.0){ # consider the evolution of boosters over time
    booster_longest_dcx <- booster_evo_wrap("dcx", 1, k)
    p1 <- plot_mono(booster_longest_dcx, 7, "number of sps", "booster_sp_number_lm", 1)
    booster_longest_azy <- booster_evo_wrap("azy", 2, k)
    p2 <- plot_mono(booster_longest_azy, 7, "number of sps", "booster_sp_number_lm", 2)
    plot_mono_grid(booster_longest_dcx, booster_longest_azy, p1, p2, "booster_sp_number")
  }
  if(job==2.1){ # export gephi dataframes of a particular booster and the corresponding subgraphs
    post_date_1 <- c("160114", "170216", "180224")
    for(i in 1:3){
      trainee_1 <- repost_read(name[1], post_date_1[i])
      degree_1 <- repost_degree_count(trainee_1) #weighted degree
      degree_sub_1 <- booster_sockpuppet_single(trainee_1, degree_1, "dcx", post_date_1[i], 1)
    }
  }
    
  }
########################igraph code added########################
#g_s <- induced_subgraph(g_trainee, subcomponent(g_trainee, "Aaaoooz_", mode = "out"))
#V(g_s)$size <- degree(g_s)*2
#V(g_s)$frame.color <- "white"
#V(g_s)$color <- "gold"
#V(g_s)$label <- ""
#E(g_s)$width <- 1+E(g_s)$weight/50
#E(g_s)$arrow.size <- .2
#plot(g_s, layout = layout_with_dh)
#not ideal#
#plot(g_s, layout = layout_as_star) 
#plot(g_s, layout = layout_with_gem)
#plot(g_s, layout = layout_with_graphopt)
#plot(g_s, layout = layout_with_lgl)
########################igraph code added########################
######summary######
#summary(g_trainee)
#V(g)
#E(g)
######cliques, k-core, hubs, authorities######
#the k-core has the same result as hubs
#largest_cliques(g_trainee)
#clique_num(g_trainee) azy 3,6,4,4 dcx 4,4,4,5
###
#c=coreness(g_trainee)
#c[which.max(c)]
### azy_151212, 170618, 171016, 171214
#g_hub <- hub_score(g_trainee, scale = TRUE, options = arpack_defaults)
#g_hub_vector <- g_hub$vector
#g_hub_vector[which.max(g_hub_vector)]
#length(which(g_hub_vector > 0.001)) 2, 1, 12(the largest hub with its sockpuppets)
###
#g_auth <- authority_score(g_trainee, scale = TRUE, options = arpack_defaults)
#g_auth_vector <- g_auth$vector
#g_auth_vector[which.max(g_auth_vector)]
#length(which(g_auth_vector > 0.001)) 1,7,20,8

######components######
#?????no strong connected components
#components(g_trainee, mode="strong")

######the loop edges######
#l=which_loop(g_trainee)
#length(which(l))

######repost levels######
#?????the results of dfs and bfs are different
#g_dfs <- dfs(g_trainee, root = "TF家族新生-敖子逸", neimode = "out",
#unreachable = TRUE, order = TRUE, order.out = FALSE, father = FALSE,
#dist = TRUE, in.callback = NULL, out.callback = NULL, extra = NULL,
#rho = parent.frame())
#g_dist <- g_dfs$dist
#g_dist_freq <- as.data.frame(table(g_dist))
###
#g_bfs <- bfs(g_trainee, root = "TF家族新生-敖子逸", neimode = "out",
#unreachable = TRUE, restricted = NULL, order = TRUE, rank = FALSE,
#father = FALSE, pred = FALSE, succ = FALSE, dist = TRUE,
#callback = NULL, extra = NULL, rho = parent.frame())
#g_dist <- g_bfs$dist
#g_dist_freq <- as.data.frame(table(g_dist))

######distances######
###distances###
#g_dist <- distances(g_trainee, v = "TF家族新生-敖子逸",to = V(g_trainee), 
#mode = "out", algorithm = "unweighted")
#g_dist[which.max(g_dist)]
#g_dist_freq <- as.data.frame(table(g_dist))
###neighbors###
#g_ego_1 <- ego(g_trainee, 1, nodes = "TF家族新生-敖子逸", mode = "out", mindist = 1)
#g_ego_1 <- t(as.data.frame(as.list(g_ego_1[[1]]))) transpost is needed
#g_ego_size[1] <- ego_size(g_trainee, 1, nodes = "TF家族新生-敖子逸", mode = "out", mindist = 1)
#g_ego_size <- c(g_ego_size, ego_size(g_trainee, 2, nodes = "TF家族新生-敖子逸", mode = "out", mindist = 2))
#make_ego_graph(g_trainee, 1, nodes = "TF家族新生-敖子逸", mode = "out", mindist = 0)
#neighbors(g_trainee, "TF家族新生-敖子逸", mode = "out")

######communities######azy_171214
###1 undirected
#clp <- cluster_label_prop(g_trainee, weights = NULL, initial = NULL, fixed = NULL)
###19 undirected information map
#g_imc <- cluster_infomap(graph, e.weights = NULL, v.weights = NULL, nb.trials = 10,
#modularity = TRUE)
#membership(g_imc)
#communities(g_imc)
###25 undirected fast greedy
#g_fc <- cluster_fast_greedy(as.undirected(g_trainee), merges = TRUE, modularity = TRUE, 
#membership = TRUE, weights = E(as.undirected(g_trainee))$weight)
#sizes(g_fc) return all the communities as well as their sizes
### directed edge betweenness SSR slow
#g_eb <- cluster_edge_betweenness(g_trainee, weights = E(g_trainee)$weight, directed = TRUE, 
#edge.betweenness = TRUE, merges = TRUE, bridges = TRUE, modularity = TRUE, membership = TRUE)

######layout#######
###star
#g_star <- layout_as_star(g_trainee, center = "TF家族新生-敖子逸", order = NULL)
#plot(g_trainee, layout = g_star)
###large graph layout
#g_lgl <- layout_with_lgl(graph, maxiter = 150, maxdelta = vcount(graph),
#area = vcount(graph)^2, coolexp = 1.5, repulserad = area *
#vcount(graph), cellsize = sqrt(sqrt(area)), root = NULL)

######mutual edges###### 0
#g_mu <- which_mutual(g_trainee)
#sum(g_mu)/2

######maximum flow######
#max_flow(g_trainee, source=V(g_trainee)["1"], target=V(g_trainee)["2"], capacity = NULL)

######centrality######
###Page rank###0.989
#g_pr <- page_rank(g_trainee)
#range(g_pr$vector)
#breaks <- seq(0.0002, 0.008, by = 0.0005)
#cut <- cut(g_pr$vector, breaks, right=FALSE)
#g_pr.freq <- transform(table(cut))
#g_pr.freq <- transform(g_pr.freq, Real_freq = prop.table(Freq), Cum_freq = cumsum(Freq))
###Bonacich power centrality score###0.99976 
#g_pc <- power_centrality(g_trainee, nodes = V(g_trainee), loops = TRUE, exponent = 1,
#rescale = FALSE, tol = 1e-07, sparse = TRUE)
#range(g_pc)
#g_pc.freq <- transform(table(cut))
#g_pc.freq <- transform(g_pc.freq, Real_freq = prop.table(Freq), Cum_freq = cumsum(Freq))
#which.max(g_pc) = TF家族新生-敖子逸, which(g.pc > 0) size = 96

######motifs######
#sample_motifs(g_trainee, 3) 3466
#sample_motifs(g_trainee, 4) 9037

######topological sorting of vertices######
#topo_sort(g_trainee)

######multiple edges######
#E(g_trainee)[weight == 1]
#3253,401,122,74,43,25,19,22,17,31
#9,5,6,5,3,5,3,2,3,9
#(21:10)129,32,28,34,18,32,17,7
#(101:20)11,9,11,6,7,3,4,2,0,2
#388,400,395