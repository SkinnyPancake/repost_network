#load libraries
library(tidyverse)
library(dplyr)
library(ggpubr)
library(cowplot)
library(igraph)
library(data.table)
library(plyr)
rm(list=ls()) #clear variables and functions
# global functions
########################create a function to count the weighted indegree and outdegree########################
repost_degree_count <- function(df){ # degrees calculation, considering multiple edges
  outdegree<- as.data.frame(table(df$source))
  colnames(outdegree)<- c("id", "out_degree") # weighted out degree
  indegree<-as.data.frame(table(df$target))
  colnames(indegree)<- c("id", "in_degree") # weighted in degree
  degree <- merge(indegree, outdegree, by = c("id"), all = TRUE) # merge the two datasets
  degree[is.na(degree)] <- 0
  degree <- degree[order(-degree$out_degree, -degree$in_degree),]
  return(degree)
}
########################create a function to find boosters and sock puppets then return the list of boosters and sock puppets########################
booster_sockpuppet_00 <- function(df, degree){
  edges <- df[, c("source","target")] # extract edge list
  g_df <- graph_from_data_frame(edges, directed = TRUE, vertices = NULL) # create graph
  E(g_df)$weight <- count_multiple(g_df) # remove mulitple edges but keep multiplicity
  g_df <- simplify(g_df)
  # include (or exclude) the trainee himself/herself
  booster_sp <- as.vector(degree$id[degree$out >= sqrt(nrow(df))]) # the threshold for the out degree of a booster
  n <- length(booster_sp) # find the descendents
  for (i in 1:n){ # put the descendents in the degree data frame
    desced <- tail(subcomponent(g_df, booster_sp[i], mode = "out"),-1)
    if (!(booster_sp[i] %in% list_id) & length(desced) > 0){
      booster_sp <- c(booster_sp, as.vector(desced$name))}
  }
  booster_sp <- unique(booster_sp)
  return(booster_sp)
}
########################create a function to get the reposts of a post with weighted degree and identification for boosters, sock puppets and normal fans########################
repost_single <- function(df, degree, booster_sp, color, version, date, index){
  node <- degree # get the node list with weighted degree
  node$color <- ifelse(node$id %in% booster_sp, color[2], color[3]) # give different types to boosters, sock puppets and normal fans
  node[node$id %in% list_id, ]$color <- color[1]
  node <- na.omit(node)
  # df <- df[order(as.Date(df$pub_time)),]
  # node$first_pub_time <- df$pub_time[match(node$id, df$target)] # add timestamp
  write.csv(df, file = paste(folder, "R/simulation/general/", version, "_", date, "_", index,  "_w.csv", sep = ""), # an extra "w" to represent the feature of weighted degree
            row.names = FALSE)
  write.csv(node, file = paste(folder, "R/simulation/general/", version, "_", date, "_", index, "_w_node.csv", sep = ""),
            row.names = FALSE)
}

# global variables

folder <- "/Users/xingruchen/Dropbox/Sina_weibo/data/" # path to folder that holds multiple .csv files
color_1 <- c("#3399FF", "#FF6666", "#99CCFF")
color_2 <- c("#FF3333", "#66B2FF", "#FF9999")

N <- 4000 # total number of unique retweeters N (total number of users is N+1, the "1" comes from the original celebrity)

M <- 60000 # total number of retweets M

booster_ratio <- c(800,400)

sp_ratio <- c(4, 1, 2)

spp_ratio <- c(1, 1/2, 1/2)

k <- ceiling(N/200) # initial number of retweeters some of whom are likely to grow into boosters # N/200

nb <- ceiling(N/booster_ratio[1]) # total number of boosters # 20

ndust <- ceiling(2*N/3) # total number of dusts

# number of sock puppets per booster: mean, standard deviation
Mean_nsp <- ceiling(sqrt(N)/sp_ratio[2]); SD_nsp <- ceiling(sqrt(N)/sp_ratio[2]) 

# the retweet power of sock puppets: mean, standard deviation
Mean_psp <- ceiling(sqrt(M)/spp_ratio[2]); SD_psp <- ceiling(sqrt(M)/spp_ratio[2])

# probabilities
p_a <- 49/50; p_b <- 499/500

list_id <- c("1") # the original celebrity

simulation_v4 <- function(job=0){
  
  # generate a indice set of boosters (do not include the first k retweeters)
  # the index of a booster is between k + 1 and 4*N/5
  # introduce the exponential distribution with mean 10/N
  booster_list <- intersect((k+1):ceiling(4*N/5), unique(ceiling(rexp(5*nb, 10/N))))
  booster <- as.data.frame(sort(sample(booster_list, nb, replace = F))) # generate the indices of boosters
  colnames(booster) <- "index"
  booster$no_of_sp <- floor(rexp(nb, 1/SD_nsp)) # generate the number of sockpuppets for boosters
  nsp <- sum(booster$no_of_sp) # get the total number of sockpuppets in the network
  
  # generate a indice set of sockpuppets
  sockpuppet <- as.data.frame(rep(NA, nsp)) # initialize the indices of sockpuppets
  colnames(sockpuppet) <- "index"
  sockpuppet$booster_no <- NA
  sockpuppet$booster_index <- NA
  sockpuppet$size <- NA
  
  booster$sp_begin <- 0
  booster$sp_end <- 0
  
  sp <- NULL # initialize the indices of sockpuppets
  
  for(i in 1:nb){
    isize <- booster$no_of_sp[i]
    ibooster <- booster$index[i]
    if(isize > 0){
      booster$sp_end[i] <- sum(booster$no_of_sp[1:i])
      booster$sp_begin[i] <- booster$sp_end[i] - isize + 1
      
      sp_list <- unique(intersect(setdiff((ibooster+1):(ceiling(9*N/10)), c(booster$index,sp)), 
                           c(1:(50*isize) + ibooster, sample(ibooster:(ceiling(9*N/10)), 5*isize, replace = F))))
      
      
      sptemp <- sort(sample(sp_list, isize, replace = F)) # the indices of sockpuppets for the ith booster
      
      sp <- c(sp, sptemp)
      
      sockpuppet$index[booster$sp_begin[i]:booster$sp_end[i]] <- sptemp
      sockpuppet$booster_no[booster$sp_begin[i]:booster$sp_end[i]] <- i
      sockpuppet$booster_index[booster$sp_begin[i]:booster$sp_end[i]] <- ibooster
      sockpuppet$size[booster$sp_begin[i]:booster$sp_end[i]] <- isize
    }else{
      booster$sp_end[i] <- NA
      booster$sp_begin[i] <- NA
    }
  }
  
  # generate a indice set of parents of sockpuppets
  for (i in 1:nsp) {
    isize <- sockpuppet$size[i]
    ibooster <- sockpuppet$booster_index[i]
    iparent <- sample(c(rep(ibooster, 5*isize), sockpuppet[1:i,]$index[sockpuppet[1:i,]$booster_index==ibooster]), 1)
    sockpuppet$parent_index[i] <- iparent
  }
  
  
  # generate a rewteet power set of sockpuppets
  for(i in 1:nsp){sockpuppet$power[i] <- ceiling(rexp(1, sockpuppet$size[i]^(1/3)/SD_psp))}
  
  Ret_power <- aggregate(sockpuppet$power, by=list(Category=sockpuppet$booster_no), FUN=sum)
  
  colnames(Ret_power) <- c("no", "power")
  
  Ret_power <- merge(data.frame(no = seq(nrow(booster))) , Ret_power, all.x = TRUE)
  booster$power <- Ret_power$power
  booster$power[is.na(booster$power)] <- max(ceiling(rexp(sum(is.na(booster$power)), 1/SD_psp)), SD_psp)
  
  # generate a indice set of dusts
  dust <- sort(sample(setdiff(1:N, c(booster$index, 1:k, sockpuppet$index)), ndust, replace = F))
  
  
  # create an ordered edge list of retweet event
  # Ret_event[r, ] = [i, j] means that the rth retweet is i retweets j
  Ret_event <- matrix(rep(0, len=M*2), nrow = M)
  
  # initial stage for the first k retweets
  Ret_index <- 1 # retweet individual (index) labeling ordered by their retweeting time
  Ret_event[1, ] <- c(2, 1) # the celebrity is labeled by 1 and the first retweeting individual is labeled by 2
  Ret_ct <- 1 # total count of retweets so far
  
  
  while(Ret_index < k){
    Ret_ct <- Ret_ct + 1
    if(runif(1) < p_a){ # brand new retweet by a new individual
      Ret_index <- Ret_index + 1
      if(runif(1) < p_a){ # the new individual retweet the original tweet by the celebrity,
        Ret_event[Ret_ct, ] <- c(Ret_index + 1, 1)
      }else{ # or tweet one of the previous retweets at the moment
        Ret_event[Ret_ct, ] <- c(Ret_index + 1, sample(Ret_event[1:(Ret_ct - 1), 1], 1))
      }
    }else{ 
      if(runif(1) < p_a){ # repeat one of the previous retweets
        Ret_event[Ret_ct, ] <- Ret_event[sample(1:(Ret_ct - 1), 1), ]
      }else{ # randomly pick two indivuduals (not necessarily the former one retweets the latter one)
        Ret_event[Ret_ct, ] <- sample(1:(Ret_index + 1), 2) 
      }
    }
  }
  
  
  # second stage of retweeting
  
  while(0 < M - Ret_ct & (N - Ret_index < M - Ret_ct) & -1 < N - Ret_index){
    Ret_ct <- Ret_ct + 1
    if(runif(1) < Ret_index/N){ # repeat previous retweets
      itemp <- sample(1:Ret_index, 1) + 1 # pick up a random individual
      if(itemp %in% (dust + 1)){ # decide whether the chosen one is a dust
        Ret_ct <- Ret_ct - 1
      }else if(itemp %in% (booster$index + 1)){ # decide whether the chosen one is a booster
        Ret_event[Ret_ct, ] = c(itemp, 1) # retweet the original tweet
      }else if(itemp %in% (sockpuppet$index + 1)){ # decide whether the chosen one is a sockpuppet
        iflag <- which(sockpuppet$index + 1==itemp)
        ibooster <- sockpuppet$booster_index[iflag] # find the index of the corresponding booster
        iparent <- sockpuppet$parent_index[iflag] # find the index of the corresponding parent
        isize <- sockpuppet$size[iflag] # find the size of the siblings
        ipower <- sockpuppet$power[iflag] # find the repost power of the sockpuppet
        # select a parent from the booster, the celebrity or the parent
        jtemp <- ifelse(ibooster==iparent, sample(c(0, rep(ibooster, ipower*isize)), 1), sample(c(ibooster, iparent))) + 1
        Ret_event[Ret_ct, ] = c(itemp, jtemp)
      }else{ 
        if(runif(1) < p_b){ # find the individual's parent in all previous retweets
          if(runif(1) < p_a){
            Ret_event[Ret_ct, ] = c(itemp, 1) # retweet the original tweet
          }else{
            iflag <- which(Ret_event[1:(Ret_ct-1), 1]==itemp) # to whom he has retweeted multiple times
            Ret_event[Ret_ct, ] = Ret_event[max(iflag), ] # add the new multiple retweet to the list (repeating the latested retweet)
          }
        }else{
          if(runif(1) < p_a){ # tweet one of the most retweeted three retweets 
            x <- as.data.frame(table(Ret_event[1:(Ret_ct-1), 2])) %>% 
              arrange(desc(Freq)) %>% 
              filter(!Var1=="1")
            jtemp <- sample(x$Var1[1:min(3,nrow(x))], 1) 
            Ret_event[Ret_ct, ] <- c(Ret_index+1, ifelse(is.na(jtemp), 1, jtemp)) # in case x is empty
          }else{ # or tweet one of the most recent five retweets at the moment (will not retweet any sockpuppet)
            # Ret_event[Ret_ct, ] <- c(itemp, Ret_event[Ret_ct+sample(-5:-1, 1), 1])
            Ret_index_sub <- Ret_event[1:(Ret_ct-1), 1] # get the current retweet accounts
            Ret_index_sub <- Ret_index_sub[!(Ret_index_sub %in% (sockpuppet$index + 1))] # remove the sockpuppets
            Ret_event[Ret_ct, ] <- c(Ret_index+1, sample(tail(Ret_index_sub,5), 1))
          }
        }
      } 
    }else{ # brand new retweets by new individuals
      Ret_index <- Ret_index + 1
      if(Ret_index %in% booster$index){# decide whether the new individual is a booster 
        if(booster$no_of_sp[booster$index==Ret_index] > 0){ # the booster has any sockpuppet
          Ret_event[Ret_ct, ] <- c(Ret_index+1, 1)
        }else{ # the booster has no sockpuppet
          Ret_event[Ret_ct, ] <- c(Ret_index+1, 1)
          ipower <- booster$power[booster$index==Ret_index]
          Ret_ct <- Ret_ct + ipower # retweet the booster himself/herself
          Ret_event[(Ret_ct+1-ipower):Ret_ct, ] <- rep(c(Ret_index+1, Ret_index+1), each=ipower)
        }
        }else if(Ret_index %in% sockpuppet$index){ # decide whether the new individual is a sockpuppet
          iflag <- which(sockpuppet$index==Ret_index)
          ipower <- sockpuppet$power[iflag] # the size of siblings
          ibooster <- sockpuppet$booster_index[iflag] # find the index of the corresponding booster
          iparent <- sockpuppet$parent_index[iflag] # find the index of the corresponding parent
          jtemp <- ifelse(iparent==Ret_index, ibooster, iparent) + 1
          Ret_event[Ret_ct, ] <- c(Ret_index+1, jtemp)
          Ret_ct <- Ret_ct + ipower
          Ret_event[(Ret_ct+1-ipower):Ret_ct, ] <- rep(c(Ret_index+1, iparent + 1),each=ipower)
          }else{ # the new individual retweet the original tweet by the celebrity
            # or tweet one of the most retweeted three retweets 
            # or tweet one of the most recent five retweets at the moment (will not retweet any sockpuppet)
            if(runif(1) < p_a){
              Ret_event[Ret_ct, ] <- c(Ret_index+1, 1)
              }else if(runif(1) < p_a){
                x <- as.data.frame(table(Ret_event[1:(Ret_ct-1), 2])) %>% 
                  arrange(desc(Freq)) %>% 
                  filter(!Var1=="1")
                jtemp <- sample(x$Var1[1:min(3,nrow(x))], 1) 
                Ret_event[Ret_ct, ] <- c(Ret_index+1, ifelse(is.na(jtemp), 1, jtemp)) # in case x is empty
                }else{
                  Ret_index_sub <- Ret_event[1:(Ret_ct-1), 1] # get the current retweet accounts
                  Ret_index_sub <- Ret_index_sub[!(Ret_index_sub %in% (sockpuppet$index + 1))] # remove the sockpuppets
                  Ret_event[Ret_ct, ] <- c(Ret_index+1, sample(tail(Ret_index_sub,5), 1))}
          }
      }
    }
  # finish the remaining retweets
  
  itemp <- Ret_index
  
  if(itemp < N){
    for(Ret_index in (itemp+1):N){
      Ret_ct <- Ret_ct+1
      # the new individual retweet the original tweet by the celebrity,
      # or retweet one of the most retweeted three retweets 
      # or retweet one of the most revent five retweets at the moment
      if(runif(1) < p_a){
        Ret_event[Ret_ct, ] <- c(Ret_index+1, 1)
      }else if(runif(1) < p_a){
        x <- as.data.frame(table(Ret_event[1:(Ret_ct-1), 2])) %>% 
          arrange(desc(Freq))
        Ret_event[Ret_ct, ] <- c(Ret_index+1, sample(x$Var1[1:min(3,nrow(x))], 1))
      }else{
        Ret_index_sub <- Ret_event[1:(Ret_ct-1), 1] # get the current retweet accounts
        Ret_index_sub <- Ret_index_sub[!(Ret_index_sub %in% (sockpuppet$index + 1))] # remove the sockpuppets
        Ret_event[Ret_ct, ] <- c(Ret_index+1, sample(tail(Ret_index_sub,5), 1))
      }
    }
  }
  df <- as.data.frame(Ret_event) %>% 
    `colnames<-`(c("target", "source"))
  degree <- repost_degree_count(df) %>% 
    `colnames<-`(c("id", "in", "out"))
  booster_sp <- booster_sockpuppet_00(df, degree)
  sum(degree$out>=sqrt(M))
  date <- "181028"
  index <- "4"
  color <- color_1
  repost_single(df, degree, booster_sp, color, "v4", date, index) # "180806", "1"
}



