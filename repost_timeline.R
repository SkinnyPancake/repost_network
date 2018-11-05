library(tidyverse) #load libraries
library(dplyr)
library(quantmod)
library(ggpubr)
library(cowplot)
library(plotly) #for 3d plot
rm(list=ls()) #clear variables and functions
folder <- "/Users/xingruchen/Dropbox/Sina_weibo/data/" #path to folder that holds multiple .csv files
folder_d <- "/Users/xingruchen/Dropbox/Sina_weibo/data/R/diff/" # path to folder that holds multiple .csv files on repost time
folder_f <- "/Users/xingruchen/Dropbox/Sina_weibo/manuscript/" #path to folder that holds multiple figure files
folder_ft <- "/Users/xingruchen/Dropbox/Sina_weibo/manuscript/figure_timeline/" #path to folder that holds multiple timeline figure files
#job==0: #plot reposts for a single trainee
#job==0.1: #plot reposts for a single trainee with logistic growth regression
#job==1: #plot monochrome and bichrome reposts
#job==1.1: #plot stacked monochrome and bichrome reposts
#create a function to count the daily number of reposts
title_size <- c(16, 28); axis_size <- c(14, 26)
repost_count_day <- function(trainee, timeStamps){
  #Dates<-as.Date(strftime(trainee$pub_time,"%Y-%m-%d"))
  Dates <- as.Date(trainee$pub_time, tz = "HongKong")
  allDates<-seq(from = min(Dates), to = max(Dates), by = "day")
  reposts<-sapply(allDates, FUN = function(X) sum(Dates == X))
  cumreposts<-cumsum(reposts)
  result <- data.frame(day = allDates, reposts = reposts, cumreposts = cumreposts)
  return(result)
}

############create a function to count the daily number of reposts (new)############
repost_count_day_x <- function(trainee, timeStamps){
  Datetimes <- as.POSIXct(trainee$pub_time, tz = "HongKong")
  cutDatetimes <- seq(from = min(Datetimes), to = max(Datetimes), by = "day")
  reposts <- sapply(cutDatetimes, FUN = function(X) sum(Datetimes < X + days(1) & Datetimes >= X))
  cumreposts <- cumsum(reposts)
  result <- data.frame(day = as.Date(cutDatetimes, tz = "HongKong"), reposts = reposts, cumreposts = cumreposts)
  return(result)
}


############create a function to count the hourly number of reposts (hourly)############
repost_count_hour <- function(trainee, timeStamps){
  Datetimes <- as.POSIXct(trainee$pub_time, tz = "HongKong")
  cutDatetimes <- seq(from = min(Datetimes), to = max(Datetimes), by = "hour")
  reposts <- sapply(cutDatetimes, FUN = function(X) sum(Datetimes < X + hours(1) & Datetimes >= X))
  cumreposts <- cumsum(reposts)
  result <- data.frame(datetime = cutDatetimes, reposts = reposts, cumreposts = cumreposts)
  return(result)
}

#create a function to count the minutely number of reposts
repost_count_minute <- function(trainee, timeStamps){
  Datetimes<-as.POSIXct(trainee$pub_time)
  allDatetimes<-seq(from = min(Datetimes), to = max(Datetimes), by = "min")
  reposts<-sapply(allDatetimes, FUN = function(X) sum(Datetimes == X))
  cumreposts<-cumsum(reposts)
  result <- data.frame(datetime = allDatetimes, reposts = reposts, cumreposts = cumreposts)
  return(result)
}
#create a function to count the hourly number of reposts

########################plotting########################
########################create a function to plot reposts for a single trainee########################
plot_mono_day_0 <- function(name, date, usdate, tcolor, tag){
  trainee<-read.csv(paste(folder, name, "_", date, ".csv", sep='')) #read files
  #View(trainee)
  ttrainee<-repost_count_day_x(trainee) #count the daily number of reposts
  #View(ttrainee)
  n = 28 #set time range(how many days)
  ttrainee_lg <- filter(ttrainee, day <= day[1] + n - 1)
  ttrainee_lg$index <- 1:nrow(ttrainee_lg) #add indices
  
  p = ggplot(data = ttrainee_lg) + #plot partial timeline
    geom_point(aes(day, cumreposts), color = "#0066CC", size = 1.5) +
    geom_segment(aes(day, 0, xend = day, yend = reposts), color = "#0066CC", size = 2, data = ttrainee_lg) + 
    scale_y_continuous(name="number of reposts", breaks=c(0,10,100, 1000,10000,100000,1000000), trans="log1p", expand=c(0,0), labels = function(x) format(x, scientific = TRUE)) +
    xlab("time (day)") +
    ylab("number of reposts") +
    theme_bw() +
    theme(text = element_text(size=title_size[2]), axis.text.x = element_text(size = axis_size[2]), axis.text.y = element_text(size = axis_size[2])) +
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
    theme(plot.margin=unit(c(0.5,1,0.5,0.5),"cm")) # t, r, b, l
  #add daily reposts line segments
  
  #annotate_figure(p,
  #top = text_grob("timeline", color = "black", face = "bold", size = 14),
  #bottom = text_grob(paste("Data source: singer", tag, " Post date: ", usdate, sep=' '), 
  #color = "black", just = "centre", hjust = 1.5, x = 1, face = "plain", size = title_size[2]))
  ggsave(height = 10, width = 10, file=paste0(folder_ft, "dgrowth_", name, "_", date, ".pdf"))
}
########################create a function to plot reposts for a single trainee with logistic growth regression (daily)########################
plot_mono_day <- function(name, date, usdate, tag){ #"dcx", "171127", "11/27/17", "1" name <- "dcx"; date <- "171127"; usdate <- "11/27/17"; tag <- "1"
  trainee<-read.csv(paste(folder, name, "_", date, ".csv", sep='')) #read files
  #View(trainee)
  ttrainee<-repost_count_day_x(trainee) #count the daily number of reposts
  #View(ttrainee)
  n = 7 #set time range(how many days)
  ttrainee_lg <- filter(ttrainee, day <= day[1] + n - 1)
  ttrainee_lg$index <- 1:nrow(ttrainee_lg) #add indices
  #calculate initial parameters
  para <- as.vector(getInitial(cumreposts ~ SSlogis(index, alpha, xmid, scale), data = ttrainee_lg))
  para0 <- c(alpha=para[1], beta=para[2]/para[3], gamma=1/para[3])
  fit0 <- nls(cumreposts~alpha/(1+exp(beta-gamma*index)), ttrainee_lg, start=para0, trace=T)
  #summary(fit0)
  abc <- as.vector(coef(fit0)) #attain parameter alpha, beta and gamma
  ttrainee_lg <- mutate(ttrainee_lg,logis = abc[1]/(1+exp(abc[2]-abc[3]*index)))

  p = ggplot(data = ttrainee_lg) + #plot partial timeline
    geom_point(aes(x=day, y=cumreposts, color = "actual"), size = 1.5) + 
    geom_smooth(aes(x=day, y=logis, color = "fitting"),size = 1.5, se=F) +
    scale_color_manual(values = c("#0066CC", "#FF9999"), name = "Type") + # "#2171b5", "#fcae91"
    xlab("time (day)") +
    ylab("number of reposts") +
    scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
    theme_bw() +
    theme(text = element_text(size=title_size[2]), axis.text.x = element_text(size = axis_size[2]), axis.text.y = element_text(size = axis_size[2])) +
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
    theme(plot.margin=unit(c(0.5,1,0.5,0.5),"cm")) # t, r, b, l
  #add daily reposts line segments
  p = p + geom_segment(aes(day, 0, xend = day, yend = reposts), color = "#0066CC",
                   size = 2, data = ttrainee_lg) 
  #annotate_figure(p,
                  #top = text_grob("timeline", color = "black", face = "bold", size = 14),
                  #bottom = text_grob(paste("Data source: singer", tag, " Post date: ", usdate, sep=' '), 
                                     #color = "black", just = "centre", hjust = 1.5, x = 1, face = "plain", size = title_size[2]))
  ggsave(height = 10, width = 10, file=paste0(folder_ft, "lgrowth_", name, "_", date, ".pdf"))
  print(summary(fit0))
}
########################create a function to plot reposts for a single trainee (hourly)########################
plot_mono_hour <- function(name, date, usdate, tag){ #"dcx", "171127", "11/27/17", "1" name <- "dcx"; date <- "171127"; usdate <- "11/27/17"; tag <- "1"
  trainee<-read.csv(paste(folder, name, "_", date, ".csv", sep='')) #read files
  #View(trainee)
  ttrainee<-repost_count_hour(trainee) #count the daily number of reposts
  #View(ttrainee)
  n = 48 #set time range(how many days)
  ttrainee_lg <- filter(ttrainee, datetime <= datetime[1] + hours(n)-1)
  ttrainee_lg$index <- 1:nrow(ttrainee_lg) #add indices
  
  p = ggplot(data = ttrainee_lg) + #plot partial timeline
    geom_point(aes(x=index,y=cumreposts), color = "#0066CC", size = 1.5) + 
    geom_segment(aes(index, 0, xend = index, yend = reposts), color = "#0066CC",
                 size = 0.5) +
    xlab("time (hour)") +
    scale_y_continuous(name="number of reposts", breaks=c(0,10,100,1000, 10000, 100000), trans="log1p", expand=c(0,0), labels = function(x) format(x, scientific = TRUE)) +
    theme_bw() +
    theme(text = element_text(size=title_size[2]), axis.text.x = element_text(size = axis_size[2]), axis.text.y = element_text(size = axis_size[2])) +
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
    theme(plot.margin=unit(c(0.5,1,0.5,0.5),"cm")) # t, r, b, l

  #annotate_figure(p,
  #top = text_grob("timeline", color = "black", face = "bold", size = 14),
  #bottom = text_grob(paste("Data source: singer", tag, " Post date: ", usdate, sep=' '), 
  #color = "black", just = "centre", hjust = 1.5, x = 1, face = "plain", size = title_size[2]))
  ggsave(height = 10, width = 10, file=paste0(folder_ft, "hgrowth_", name, "_", date, ".pdf"))
}
########################create a function to plot reposts for a single trainee (minutely)########################
                        ######on construction######
plot_mono_minute <- function(name, date, usdate, tag){
  trainee<-read.csv(paste(folder, name, "_", date, ".csv", sep='')) #read files
  #View(trainee)
  ttrainee<-repost_count_minute(trainee) #count the daily number of reposts
  write.csv(ttrainee,
            file = paste(folder, "R/cumulative/",  "azy_151212.csv", sep = ""), row.names = FALSE)
  #View(ttrainee)
  n = 2*24*60 #set time range(how many minutes)
  ttrainee_lg <- ttrainee[1:n,]
  ttrainee_lg$index <- 1:nrow(ttrainee_lg) #add indices
  #calculate initial parameters
  para <- as.vector(getInitial(cumreposts ~ SSlogis(index, alpha, xmid, scale), data = ttrainee_lg))
  para0 <- c(alpha=para[1], beta=para[2]/para[3], gamma=1/para[3])
  fit0 <- nls(cumreposts~alpha/(1+exp(beta-gamma*index)), ttrainee_lg, start=para0, trace=T)
  #summary(fit0)
  abc <- as.vector(coef(fit0)) #attain parameter alpha, beta and gamma
  ttrainee_lg <- mutate(ttrainee_lg,logis = abc[1]/(1+exp(abc[2]-abc[3]*index)))
  p = ggplot(data = ttrainee_lg) + 
    geom_segment(aes(datetime, 0, xend = datetime, yend = reposts), color = "cornflowerblue",
                                                size = 2) 
  ggplot(data = ttrainee_lg) + #plot partial timeline
    geom_line(aes(x=datetime,y=cumreposts), size = 1.5)
  
    
  annotate_figure(p,
                  #top = text_grob("timeline", color = "black", face = "bold", size = 14),
                  bottom = text_grob(paste("Data source: trainee", tag, " Post date: ", usdate, sep=' '), 
                                     color = "black", just = "centre", hjust = 1.5, x = 1, face = "plain", size = 15))
  ggsave(file=paste0(folder_f, "lgrowth_", name, "_", date, ".eps"))
  print(summary(fit0))
}
########################create a function to plot monochrome and bichrome reposts########################
plot_bi_day <- function(date, usdate){
  #read files
  dcx<-read.csv(paste(folder, "dcx_", date, ".csv", sep=''))
  #View(dcx)
  azy<-read.csv(paste(folder, "azy_", date, ".csv", sep=''))
  #View(azy)
  #extract the monochrome reposts
  monodcx <- anti_join(dcx,azy, by = "user_id")
  #View(monodcx)
  monoazy <- anti_join(azy,dcx,by = "user_id")
  #View(monoazy)
  #extract the bichrome reposts
  coupledcx <- anti_join(dcx,monodcx, by = "user_id")
  coupleazy <- anti_join(azy,monoazy, by = "user_id")
  couple <- rbind(coupledcx, coupleazy)
  #View(couple)
  #count the daily number of reposts
  tmonodcx <- reposts_count(monodcx)
  tmonoazy <- reposts_count(monoazy)
  tcouple<-reposts_count(couple)
  #View(tcouple)
  #set time range for the plot(how many days)
  #find peaks
  n = min(nrow(tmonodcx), nrow(tmonoazy), nrow(tcouple))
  peakdcx = findPeaks(tmonodcx[1:n,]$reposts, 50) -1
  peakazy = findPeaks(tmonoazy[1:n,]$reposts, 50) -1
  peakcouple = findPeaks(tcouple[1:n,]$reposts, 50) -1
  #plot
  p = ggplot()+
    geom_line(data = tmonodcx[1:n,],aes(x=day,y=reposts,color = "mchrome_1",linetype = "daily"),size = 1.5)+
    geom_line(data = tmonoazy[1:n,],aes(x=day,y=reposts,color = "mchrome_2",linetype = "daily"),size = 1.5)+
    geom_line(data = tcouple[1:n,],aes(x=day,y=reposts,color = "bchrome",linetype = "daily"),size = 1.5)+
    geom_line(data = tmonodcx[1:n,], aes(x=day,y=cumreposts,color = "mchrome_1",linetype = "cumulative"),size = 1.5)+
    geom_line(data = tmonoazy[1:n,], aes(x=day,y=cumreposts,color = "mchrome_2",linetype = "cumulative"),size = 1.5)+
    geom_line(data = tcouple[1:n,], aes(x=day,y=cumreposts,color = "bchrome",linetype = "cumulative"),size = 1.5)+
    geom_point(data = tmonodcx[peakdcx,], aes(x = day, y = reposts),color = "cornflowerblue",size = 3)+
    geom_point(data = tmonoazy[peakazy,], aes(x = day, y = reposts),color = "pink",size = 3)+
    geom_point(data = tcouple[peakcouple,], aes(x = day, y = reposts),color = "limegreen",size = 3)+
    scale_color_manual(values = c(mchrome_1 = "cornflowerblue",mchrome_2 = "pink",
                                  bchrome = "limegreen")) +
    scale_linetype_manual(values = c(daily = "dotted",cumulative = "solid")) +
    theme_bw() +
    theme(text = element_text(size=15), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
  annotate_figure(p,
                  #top = text_grob("couple mpg", color = "black", face = "bold", size = 14),
                  bottom = text_grob(paste("Data source: trainee 1 & 2", " Post date: ", usdate, sep=' '), 
                                     color = "black", just = "centre", hjust = 1.5, x = 1, face = "plain", size = 15))
  ggsave(file=paste0(folder_f, "timeline_double_", date, ".eps"))
}

repost_list <- read.csv("/Users/xingruchen/Dropbox/Sina_weibo/list.csv") #get the data list
repost_list$date <- format(strptime(as.character(repost_list$date), "%m/%d/%Y"), "%Y%m%d")
repost_list$date <- substring(repost_list$date, 3)
View(repost_list)
date_list <- as.data.frame(table(repost_list$date)) #find common dates when trainees posted weibo
names(date_list)[1] = 'date'
date_list_common <- date_list[date_list$Freq>1, ]
View(date_list_common)

########################main function########################
repost_timeline <-
  function(job=0){
    if(job==0){
      plot_mono_day_0("dcx", "151212", "12/12/15", "cornflowerblue", "1")
      plot_mono_day_0("azy", "151212", "12/12/15", "pink", "2")
    }
    if(job==0.1){ #nonlinear regression of logistic growth
      plot_mono_day("dcx", "171127", "11/27/17", "1")
      plot_mono_day("azy", "171127", "11/27/17", "2")
    }
    #plot monochrome and bichrome reposts for a single date
    if(job==1){
      plot_bi_day("151212", "12/12/15")
      plot_bi_day("171127", "11/27/17")
      
    }
    if(job==1.1){
      date <- date_list_common[c(1,4,7,17,57), ]$date
      dcx <- list()
      azy <- list()
      tdcx <- list()
      tazy <- list()
      n = 15 #set time range for the plot(how many days)
      # read in each .csv file in file_list and create a data frame with the same name as the .csv file
      for (i in 1:length(date)){
        dcx[[i]] <- read.csv(paste(folder, "dcx_", date[i], ".csv", sep=''))
        azy[[i]] <- read.csv(paste(folder, "azy_", date[i], ".csv", sep=''))
        tdcx[[i]] <- reposts_count(dcx[[i]])
        tazy[[i]] <- reposts_count(azy[[i]])
      }
      #p <- plot_ly(data, x = ~x, y = ~y, z = ~cut, type = 'scatter3d', mode = 'lines', color = ~cut)
    }
  }
