#load libraries
library(tidyverse)
library(dplyr)
library(quantmod)
library(ggpubr)
library(cowplot)
library(lubridate)
library(tibble)
library(gridExtra) # when grid.arrange does not work, add the library to fix it
library(gtable)
library(grid) # low-level grid functions are required
library(scales) # scientific notation for legend labels
library(cowplot) # arrange plot

rm(list=ls())

folder <- "/Users/xingruchen/Dropbox/Sina_weibo/data/" # path to folder that holds multiple .csv files

folder_s <- "/Users/xingruchen/Dropbox/Sina_weibo/data/R/statistics/" # path to folder that holds multiple .csv files

folder_f <- "/Users/xingruchen/Dropbox/Sina_weibo/manuscript/figure/" # path to folder that holds multiple figures

folder_fr <- "/Users/xingruchen/Dropbox/Sina_weibo/manuscript/figure_reg/" # path to folder that holds multiple regression figures

folder_ft <- "/Users/xingruchen/Dropbox/Sina_weibo/manuscript/figure_timeline/" # path to folder that holds multiple timeline figures

folder_fd <- "/Users/xingruchen/Dropbox/Sina_weibo/manuscript/figure_diff/" # path to folder that holds multiple figures

folder_d <- "/Users/xingruchen/Dropbox/Sina_weibo/data/R/diff/" # path to folder that holds multiple .csv files on repost time

interval <- c(days(1), days(3), days(7), days(14), days(21), days(28)) # cutoff time interval
interval_folder <- c("1d/", "3d/", "7d/", "14d/", "21d/", "28d/")
interval_name <- c("1d", "3d", "7d", "14d", "21d", "28d")
object_folder <- c("child/", "dust/", "general/", "pair/", "self/") # observed objects
object_name <- c("child", "dust", "general", "pair", "self")
interval_length <- c(1440, 4320, 10080, 20160, 30240, 40320)
object_text <- c("repost pattern of individuals", "repost pattern of dusts", "general repost pattern", "repost pattern of pairs", "repost pattern of self reposts")
interval_text <- c("(first day)", "(first three days)", "(first week)", "(first two weeks)", "(first three weeks)", "(first four weeks)")

regression_name_print <- c("linear regression", "exponential regression", "power regression", "log power regression")

regression_name <- c("linear", "exp", "power", "log_power")

title_size <- c(18, 28); axis_size <- c(16, 26)

########################create a function to prepare date for print###########################
date_print_0 <- function(date){
  date <- format(as.Date(date, format = "%Y-%m-%d"), "%m/%d/%Y") # convert 2017-09-06 to 09/06/2017
  date <- as.character(date)
}
########################create a function to prepare date for print###########################
date_print <- function(date){
  date <- format(as.Date(date, format = "%y%m%d"), "%m/%d/%Y") # convert 2017-09-06 to 09/06/2017
  date <- as.character(date)
}

########################create a function to perform the four different regressions###########################
regression <- function(trainee_df, i){
  y <- trainee_df[,i]
  b_start <- trainee_df[1,i]
  
  mod_linear <-  nls(y ~ a*index+b, data = trainee_df, start = list(a = 0, b = b_start))
  print(summary(mod_linear))
  
  mod_exp <- nls(y ~ b*exp(a*index), data = trainee_df, start = list(a = 0, b = b_start))
  print(summary(mod_exp))
  
  mod_power <- nls(y ~ b*index^a, data = trainee_df, start = list(a = 1, b = b_start), control = list(maxiter = 500))
  print(summary(mod_power))
  
  
  mod_log_power <- nls(y ~ b*(log(index))^a, data = trainee_df, start = list(a = 0, b = b_start), control = list(maxiter = 500))
  print(summary(mod_log_power))
}

########################create a function to perform a particular regressions and plot the figure###########################
plot_mono <- function(trainee_df, n, variable_name, plot_name, tag, job){
  last_date = format(as.Date(trainee_df[nrow(trainee_df),1], format = "%Y-%m-%d"), "%m/%d/%Y")
  mid = mean(trainee_df[,n])
  shape = c(16, 16) #16, 18
  name = c("dcx", "azy")
  if(tag==1){color = c("#eff3ff","#bdd7e7","#6baed6", "#2171b5")}else{color = c("#fee5d9","#fcae91","#fb6a4a","#cb181d")}
  options(bitmapType="cairo")
  
  y <- trainee_df[,n]
  b_start <- trainee_df[1,n]
  
  if(job==1){ # perform linear regression y = ax + b
    mod <-  nls(y ~ a*index+b, data = trainee_df, start = list(a = 0, b = b_start)) 
    }
  if(job==2){ # perform exponential regression y = b exp(ax)
    mod <- nls(y ~ b*exp(a*index), data = trainee_df, start = list(a = 0, b = b_start)) 
  }
  if(job==3){ # perform power regression y = b x^a
    mod <- nls(y ~ b*index^a, data = trainee_df, start = list(a = 1, b = b_start), control = list(maxiter = 500)) 
  }
  if(job==4){ # perform log power regression y = b (log(x))^a
    mod <- nls(y ~ b*(log(index))^a, data = trainee_df, start = list(a = 0, b = b_start), control = list(maxiter = 500))
  }
  
  #print(summary(mod_linear))
  a <- round(coefficients(mod)[1], 6) # get the coefficients
  b <- round(coefficients(mod)[2], 6)
  y_pre <- predict(mod, list(index = trainee_df$index))
  
  trainee_df$reg <- y_pre
  
  p = ggplot(data = trainee_df)+
    scale_shape_discrete(solid=F) +
    geom_point(aes(x=index,y=trainee_df[,n], color = trainee_df[,n]), shape = shape[tag], size = 2) +
    scale_color_gradient2(midpoint=mid, low=color[1], mid=color[2],
                          high=color[4], space ="Lab", labels=scientific) +
    geom_line(aes(x=index,y=trainee_df$reg), color = color[3], size = 2) + # add fitted curve
    scale_y_continuous(name=variable_name, labels = function(x) format(x, scientific = TRUE)) +
    xlab("time (day)") +
    ylab(variable_name) +
    #ggtitle(paste0("a = ", a, ", b = ", b)) +
    theme_bw() +
    theme(plot.title = element_text(size = title_size[2])) + # hjust = 0.5, 
    theme(legend.title=element_blank(), legend.position="none") +
    theme(text = element_text(size=title_size[2]), axis.text.x = element_text(size = axis_size[2]), axis.text.y = element_text(size = axis_size[2])) +
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))
  #annotate_figure(p,
                  #top = text_grob("timeline", color = "black", face = "bold", size = 14),
                  #bottom = text_grob(paste("Data source: trainee ", tag, " Post date: 12/12/2015 to ", as.character(last_date), sep=''), 
                                     #color = "black", just = "centre", hjust = 1, x = 1, face = "plain", size = 15))
  ggsave(height = 10, width = 10, file=paste0(folder_fr, plot_name, "_", name[tag],"_", regression_name[job], ".pdf"))
  print(paste0("a = ", a, ", b = ", b))
  return(p)
}

########################create a function to get and save side-by-side plot###########################
plot_mono_grid <- function(trainee_df_1, trainee_df_2, p1, p2, plot_name, job){
  date_1_f <- date_print(trainee_df_1[1,1]) # extract the date of the first post for trainee 1
  date_1_l <- date_print(trainee_df_1[nrow(trainee_df_1),1]) # extract the date of the last post for trainee 1
  date_2_f <- date_print(trainee_df_2[1,1]) # extract the date of the first post for trainee 2
  date_2_l <- date_print(trainee_df_2[nrow(trainee_df_2),1]) # extract the date of the last post for trainee 2
  title1 <- paste("trainee 1 post date: ", date_1_f, " to ", date_1_l)
  title2 <- paste("trainee 2 post date: ", date_2_f, " to ", date_2_l)
  options(scipen = 20) # restore scientific notation in printing
  g1 <- ggplotGrob(p1)
  g2 <- ggplotGrob(p2)
  maxWidth = grid::unit.pmax(g1$widths[2:5], g2$widths[2:5])
  g1$widths[2:5] <- as.list(maxWidth)
  g2$widths[2:5] <- as.list(maxWidth)
  p <- grid.arrange(g1, g2, ncol=1)
  #annotate_figure(p,
                  #top = text_grob(regression_name_print[job], color = "black", face = "bold", size = 15),
                  #bottom = text_grob(paste(title1, title2, sep = "\n"), color = "black",
                                     #just = "centre", hjust = 1, x = 1, face = "plain", size = 15))
  ggsave(height = 16, width = 16, file=paste0(folder_fr, plot_name,  "_", regression_name[job], ".pdf"), plot=p)
}


k = 4
dcx_booster <- read.csv(paste0(folder_s, "dcx_booster_", interval_name[k], ".csv")) 
dcx_booster_index <- read.csv(paste0(folder_s, "dcx_booster_index_", interval_name[k], ".csv"))
dcx_para <- read.csv(paste0(folder_s, "dcx_para.csv"))
dcx_event <- read.csv(paste0(folder_s, "dcx_event_", interval_name[k], ".csv"))

dcx_booster_evo_fanclub <- read.csv(paste0(folder_s, "dcx_booster_evo_fanclub_", interval_name[k], ".csv"))
azy_booster_evo_fanclub <- read.csv(paste0(folder_s, "azy_booster_evo_fanclub_", interval_name[k], ".csv"))


azy_booster <- read.csv(paste0(folder_s, "azy_booster_", interval_name[k], ".csv")) 
azy_booster_index <- read.csv(paste0(folder_s, "azy_booster_index_", interval_name[k], ".csv"))
azy_para <- read.csv(paste0(folder_s, "azy_para.csv"))
azy_event <- read.csv(paste0(folder_s, "azy_event_", interval_name[k], ".csv"))

dcx_para_sub <- dcx_para[!is.na(dcx_para$alpha), ]
azy_para_sub <- azy_para[!is.na(azy_para$alpha), ]
########################################################################################################################
dcx_count <- read.csv(paste0(folder_d, object_folder[3], interval_folder[3], "dcx", "_", "171127", "_", object_name[3],".csv"))
dcx_count_sub <- dcx_count[!is.na(dcx_count$interval), ]
diff_count <- as.data.frame(table(dcx_count_sub$min))
colnames(diff_count) <- c("min", "count")
diff_count$min <- as.integer(as.character(diff_count$min))
diff_count$cumcount <- cumsum(diff_count$count)
diff_count_sub <- diff_count[diff_count$min <= 120,]
#calculate initial parameters
#para <- as.vector(getInitial(cumcount ~ SSlogis(min, alpha, xmid, scale), data = diff_count))
#para0 <- c(alpha=para[1], beta=para[2]/para[3], gamma=1/para[3])
#fit0 <- nls(cumcount~alpha/(1+exp(beta-gamma*min)), diff_count, start=para0, trace=T)
#summary(fit0)
#abc <- as.vector(coef(fit0)) #attain parameter alpha, beta and gamma
#diff_count <- mutate(diff_count,logis = abc[1]/(1+exp(abc[2]-abc[3]*min)))
#para <- as.vector(getInitial(cumcount ~ SSlogis(min, alpha, xmid, scale), data = diff_count_sub))
#para0 <- c(alpha=para[1], beta=para[2]/para[3], gamma=1/para[3])
#fit0 <- nls(cumcount~alpha/(1+exp(beta-gamma*min)), diff_count_sub, start=para0, trace=T)
#summary(fit0)
#abc <- as.vector(coef(fit0)) #attain parameter alpha, beta and gamma
#diff_count_sub <- mutate(diff_count_sub,logis = abc[1]/(1+exp(abc[2]-abc[3]*min)))
p = ggplot(data = diff_count_sub) + #plot partial timeline
  geom_point(aes(x=min,y=cumcount), color = "#0066CC", size = 1.5) + 
  geom_segment(aes(min, 0, xend = min, yend = count), color = "#0066CC",
               size = 0.5) +
  xlab("time (minute)") +
  scale_y_continuous(name="number of reposts", breaks=c(0,10,100,1000,10000), trans="log1p", expand=c(0,0), labels = function(x) format(x, scientific = TRUE)) +
  theme_bw() +
  theme(text = element_text(size=title_size[1]), axis.text.x = element_text(size = axis_size[1]), axis.text.y = element_text(size = axis_size[1])) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  theme(plot.margin=unit(c(0.5,1,0.5,0.5),"cm")) # t, r, b, l
#add daily reposts line segments
#annotate_figure(p,
                #top = text_grob("timeline", color = "black", face = "bold", size = 14),
                #bottom = text_grob(paste("Data source: celebrity", "1", " Post date: ", "11/27/2017", sep=' '), 
                                   #color = "black", just = "centre", hjust = 1, x = 1, face = "plain", size = title_size[2]))
ggsave(height = 10, width = 10, file=paste0(folder_ft, "mgrowth_", "dcx", "_", "171127", ".pdf"))
########################################################################################################################
########################
########################create a function to plot the density distribution for a diff dataframe (do not consider different type of fans, for dust only)###########################
plot_diff_00 <- function(name, date, usdate, tag, i_tag, o_tag){
  diff_df <- read.csv(paste0(folder_d, object_folder[o_tag], interval_folder[i_tag], name, "_", date[1], "_", object_name[o_tag],".csv"))
  diff_df_sub <- diff_df[!is.na(diff_df$min), ]
  if(tag==1){color <- c("#08519c", "#eff3ff")}else{
    color <- c("#a50f15", "#fee5d9")
  }
  p <- ggplot(diff_df_sub, aes(x=min)) +
    stat_density(aes(y=..count.., ), color = color[1], fill = color[2], alpha=0.8) +
    scale_x_continuous(name="min", breaks=c(0,1,2,3,4,5,10,30,100,300,1000,5000,21000), trans="log1p", limits=c(0, interval_length[i_tag]), expand=c(0,0)) +
    #scale_y_continuous(breaks=c(0,2000,4000,6000,8000,10000,20000,40000,100000), trans="log1p",expand=c(0,0)) +
    scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
    annotate("text", x=Inf, y=Inf, label= usdate, size = 5, hjust = 1.5, vjust = 1.5, fontface = 1.5) +
    theme_bw() + 
    theme(text = element_text(size=15), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
    theme(legend.position="bottom") +
    theme(plot.margin=unit(c(0.5,0.5,0.5,1.5),"cm")) # t, r, b, l
  return(p)
}

########################create a function to plot the density distribution for a diff dataframe (do not consider different type of fans)###########################
plot_diff_0 <- function(name, date, usdate, tag, i_tag, o_tag){
  diff_df <- read.csv(paste0(folder_d, object_folder[o_tag], interval_folder[i_tag], name, "_", date[1], "_", object_name[o_tag],".csv"))
  diff_df_sub <- diff_df[!is.na(diff_df$interval), ]
  if(tag==1){color <- c("#08519c", "#eff3ff")}else{
    color <- c("#a50f15", "#fee5d9")
  }
  p <- ggplot(diff_df_sub, aes(x=interval)) +
    stat_density(aes(y=..count.., ), color = color[1], fill = color[2], alpha=0.8) +
    scale_x_continuous(name="interval (min)", breaks=c(0,1,2,3,4,5,10,30,100,300,1000,5000,21000), trans="log1p", limits=c(0, interval_length[i_tag]), expand=c(0,0)) +
    #scale_y_continuous(breaks=c(0,2000,4000,6000,8000,10000,20000,40000,100000), trans="log1p",expand=c(0,0)) +
    scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
    annotate("text", x=Inf, y=Inf, label= usdate, size = 8, hjust = 1.5, vjust = 1.5, fontface = 1.5) +
    theme_bw() + 
    theme(text = element_text(size=title_size[2]), axis.text.x = element_text(size = axis_size[2]), axis.text.y = element_text(size = axis_size[2])) +
    theme(legend.position="bottom") +
    theme(plot.margin=unit(c(0.5,0.5,0.5,1.5),"cm")) # t, r, b, l
  return(p)
}
########################create a function to plot the density distribution for a diff dataframe (consider different type of fans)###########################
plot_diff <- function(name, date, usdate, tag, i_tag, o_tag){
  diff_df <- read.csv(paste0(folder_d, object_folder[o_tag], interval_folder[i_tag], name, "_", date, "_", object_name[o_tag],".csv"))
  diff_df_sub <- diff_df[!is.na(diff_df$interval), ]
  if(tag==1){color <- c("#bdd7e7", "#6baed6", "#3182bd", "#08519c")}else{
    color <- c("#fee5d9", "#fcae91", "#fb6a4a", "#cb181d")
  }
  names(color) <- c("dust", "others", "sock puppet", "booster")
  p <- ggplot(diff_df_sub, aes(x=interval)) +
    stat_density(aes(y=..count.., color=type, fill=type),alpha=0.35) +
    scale_x_continuous(name="interval (min)", breaks=c(0,1,2,3,4,5,10,30,100,300,1000,5000,21000), trans="log1p", limits=c(0, interval_length[i_tag]), expand=c(0,0)) +
    scale_colour_manual(name = "type",values = color) +
    scale_fill_manual(name = "type",values = color) +
    #scale_y_continuous(breaks=c(0,2000,4000,6000,8000,10000,20000,40000,100000), trans="log1p",expand=c(0,0)) +
    scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
    annotate("text", x=Inf, y=Inf, label= usdate, size = 8, hjust = 1.5, vjust = 1.5, fontface = 1.5) +
    theme_bw() + 
    theme(text = element_text(size=title_size[2]), axis.text.x = element_text(size = axis_size[2]), axis.text.y = element_text(size = axis_size[2])) +
    theme(legend.position="bottom") +
    theme(plot.margin=unit(c(0.5,0.5,0.5,1.5),"cm")) # t, r, b, l
  return(p)
}
########################create a function to get and save side-by-side plot (do not consider different type of fans, for dust only)###########################
plot_diff_grid_00 <- function(name, date, tag, i_tag, o_tag){
  usdate <- sapply(date, date_print)
  p1 <- plot_diff_00(name, date[1], usdate[1], tag, i_tag, o_tag) + 
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + # hide x axis
    theme(legend.position="none") # hide legend
  p2 <- plot_diff_00(name, date[2], usdate[2], tag, i_tag, o_tag) + 
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + # hide x axis
    theme(legend.position="none") # hide legend
  p3 <- plot_diff_00(name, date[3], usdate[3], tag, i_tag, o_tag)
  options(bitmapType="cairo")
  p <- plot_grid(p1, p2, p3, align = "v", nrow = 3, rel_heights = c(3/10, 3/10, 4/10))
  #g1 <- ggplotGrob(p1)
  #g2 <- ggplotGrob(p2)
  #g3 <- ggplotGrob(p3)
  #maxWidth = grid::unit.pmax(g1$widths[2:5], g2$widths[2:5], g3$widths[2:5])
  #g1$widths[2:5] <- as.list(maxWidth)
  #g2$widths[2:5] <- as.list(maxWidth)
  #g3$widths[2:5] <- as.list(maxWidth)
  #p <- grid.arrange(g1, g2, g3, ncol=1)
  
  p <- annotate_figure(p,
                       top = text_grob(paste0(object_text[o_tag], " ", interval_text[i_tag]), color = "black", face = "bold", size = 15))
  
  ggsave(paste0(folder_fd, name, "_", object_name[o_tag], "_", interval_name[i_tag], "_00.pdf"), p, width=10, height=10, dpi=100, units="in")
  
}
########################create a function to get and save side-by-side plot (do not consider different type of fans)###########################
plot_diff_grid_0 <- function(name, date, tag, i_tag, o_tag){
  usdate <- sapply(date, date_print)
  p1 <- plot_diff_0(name, date[1], usdate[1], tag, i_tag, o_tag) + 
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + # hide x axis
    theme(legend.position="none") # hide legend
  p2 <- plot_diff_0(name, date[2], usdate[2], tag, i_tag, o_tag) + 
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + # hide x axis
    theme(legend.position="none") # hide legend
  p3 <- plot_diff_0(name, date[3], usdate[3], tag, i_tag, o_tag)
  options(bitmapType="cairo")
  p <- plot_grid(p1, p2, p3, align = "v", nrow = 3, rel_heights = c(3/10, 3/10, 4/10))
  #g1 <- ggplotGrob(p1)
  #g2 <- ggplotGrob(p2)
  #g3 <- ggplotGrob(p3)
  #maxWidth = grid::unit.pmax(g1$widths[2:5], g2$widths[2:5], g3$widths[2:5])
  #g1$widths[2:5] <- as.list(maxWidth)
  #g2$widths[2:5] <- as.list(maxWidth)
  #g3$widths[2:5] <- as.list(maxWidth)
  #p <- grid.arrange(g1, g2, g3, ncol=1)
  
  #p <- annotate_figure(p,
                       #top = text_grob(paste0(object_text[o_tag], " ", interval_text[i_tag]), color = "black", face = "bold", size = title_size[2]))
  
  ggsave(paste0(folder_fd, name, "_", object_name[o_tag], "_", interval_name[i_tag], "_0.pdf"), p, width=10, height=10, dpi=100, units="in")
  
}
########################create a function to get and save side-by-side plot (consider different type of fans)###########################
plot_diff_grid <- function(name, date, tag, i_tag, o_tag){
  usdate <- sapply(date, date_print)
  p1 <- plot_diff(name, date[1], usdate[1], tag, i_tag, o_tag) + 
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + # hide x axis
    theme(legend.position="none") # hide legend
  p2 <- plot_diff(name, date[2], usdate[2], tag, i_tag, o_tag) + 
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + # hide x axis
    theme(legend.position="none") # hide legend
  p3 <- plot_diff(name, date[3], usdate[3], tag, i_tag, o_tag)
  options(bitmapType="cairo")
  p <- plot_grid(p1, p2, p3, align = "v", nrow = 3, rel_heights = c(3/10, 3/10, 4/10))
  #g1 <- ggplotGrob(p1)
  #g2 <- ggplotGrob(p2)
  #g3 <- ggplotGrob(p3)
  #maxWidth = grid::unit.pmax(g1$widths[2:5], g2$widths[2:5], g3$widths[2:5])
  #g1$widths[2:5] <- as.list(maxWidth)
  #g2$widths[2:5] <- as.list(maxWidth)
  #g3$widths[2:5] <- as.list(maxWidth)
  #p <- grid.arrange(g1, g2, g3, ncol=1)
  
  #p <- annotate_figure(p,
                  #top = text_grob(paste0(object_text[o_tag], " ", interval_text[i_tag]), color = "black", face = "bold", size = 15))
  
  ggsave(paste0(folder_fd, name, "_", object_name[o_tag], "_", interval_name[i_tag], ".pdf"), p, width=10, height=10, dpi=100, units="in")
  
}


dcx_date <- c("151212", "161218", "180411")

azy_date <- c("151212", "161218", "180401")

plot_diff_batch <- function(name, date, tag){
  plot_diff_grid_0(name, date, tag, 1, 1) # child first day
  plot_diff_grid_0(name, date, tag, 4, 1) # child first two weeks
  plot_diff_grid(name, date, tag, 1, 1) # child first day
  plot_diff_grid(name, date, tag, 4, 1) # child first two weeks
  
  plot_diff_grid_0(name, date, tag, 1, 3) # general first day
  plot_diff_grid_0(name, date, tag, 4, 3) # general first two weeks
  plot_diff_grid(name, date, tag, 1, 3) # general first day
  plot_diff_grid(name, date, tag, 4, 3) # general first two weeks
  
  plot_diff_grid_0(name, date, tag, 1, 4) # pair first day
  plot_diff_grid_0(name, date, tag, 4, 4) # pair first two weeks
  plot_diff_grid(name, date, tag, 1, 4) # pair first day
  plot_diff_grid(name, date, tag, 4, 4) # pair first two weeks
  
  plot_diff_grid_0(name, date, tag, 1, 2) # dust first day
  plot_diff_grid_0(name, date, tag, 4, 2) # dust first two weeks
  plot_diff_grid_00(name, date, tag, 1, 2) # dust first day
  plot_diff_grid_00(name, date, tag, 4, 2) # dust first two weeks
}

plot_diff_batch("dcx", dcx_date, 1)
plot_diff_batch("azy", azy_date, 2)

plot_diff_grid("dcx", dcx_date, 1, 1, 5) # self repost first day

# ggplot(dcx_child_sub,aes(x=interval))+geom_histogram()+facet_grid(~type)+theme_bw()

# ggplot(dcx_child_sub, aes(x=interval, color=type)) +
  # geom_histogram(fill="white", alpha=0.5, position="identity")


########################main function########################
repost_statistics <- function(job=0){
  if(job==1.1){ # plot number of reposts with linear regression
    # plot_mono <- function(trainee_df, n, variable_name, plot_name, tag, job)
    p1 <- plot_mono(dcx_event, 2, "number of reposts", "repost_number",  1, 1)
    p2 <- plot_mono(azy_event, 2, "number of reposts", "repost_number",  2, 1)
    plot_mono_grid(dcx_event, azy_event, p1, p2, "repost_number", 1)
  }
  if(job==1.2){ # plot number of reposts with exponential regression
    p1 <- plot_mono(dcx_event, 2, "number of reposts", "repost_number",  1, 2)
    p2 <- plot_mono(azy_event, 2, "number of reposts", "repost_number",  2, 2)
    plot_mono_grid(dcx_event, azy_event, p1, p2, "repost_number", 2)
  }
  if(job==1.3){ # plot number of reposts with power regression
    p1 <- plot_mono(dcx_event, 2, "number of reposts", "repost_number",  1, 3)
    p2 <- plot_mono(azy_event, 2, "number of reposts", "repost_number",  2, 3)
    plot_mono_grid(dcx_event, azy_event, p1, p2, "repost_number", 3)
  } # error
  if(job==1.4){ # plot number of reposts with power regression
    p1 <- plot_mono(dcx_event, 2, "number of reposts", "repost_number",  1, 4)
    p2 <- plot_mono(azy_event, 2, "number of reposts", "repost_number",  2, 4)
    plot_mono_grid(dcx_event, azy_event, p1, p2, "repost_number", 3)
  } # error
  
  
  if(job==2.1){ # plot number of fans with linear regression
    p1 <- plot_mono(dcx_event, 3, "number of fans", "fan_number", 1, 1)
    p2 <- plot_mono(azy_event, 3, "number of fans", "fan_number", 2, 1)
    plot_mono_grid(dcx_event, azy_event, p1, p2, "fan_number", 1)
  }
  if(job==2.2){ # plot number of fans with exponential regression
    p1 <- plot_mono(dcx_event, 3, "number of fans", "fan_number", 1, 2)
    p2 <- plot_mono(azy_event, 3, "number of fans", "fan_number", 2, 2)
    plot_mono_grid(dcx_event, azy_event, p1, p2, "fan_number", 2)
  }
  if(job==2.3){ # plot number of fans with power regression
    p1 <- plot_mono(dcx_event, 3, "number of reposters", "fan_number", 1, 3)
    p2 <- plot_mono(azy_event, 3, "number of reposters", "fan_number", 2, 3)
    plot_mono_grid(dcx_event, azy_event, p1, p2, "fan_number", 3)
  }
  if(job==2.4){ # plot number of fans with log power regression
    p1 <- plot_mono(dcx_event, 3, "number of fans", "fan_number", 1, 4)
    p2 <- plot_mono(dcx_event, 3, "number of fans", "fan_number", 1, 4)
    plot_mono_grid(dcx_event, azy_event, p1, p2, "fan_number", 4)
  }
  
  
  if(job==3.1){ # plot number of boosters with linear regression
    p1 <- plot_mono(dcx_booster, 2, "number of boosters", "booster_number", 1, 1)
    p2 <- plot_mono(azy_booster, 2, "number of boosters", "booster_number", 2, 1)
    plot_mono_grid(dcx_booster, azy_booster, p1, p2, "booster_number", 1)
  }
  if(job==3.2){ # plot number of boosters with exponential regression
    p1 <- plot_mono(dcx_booster, 2, "number of boosters", "booster_number", 1, 2)
    p2 <- plot_mono(azy_booster, 2, "number of boosters", "booster_number", 2, 2) # error
    #plot_mono_grid(dcx_booster, azy_booster, p1, p2, "booster_number", 2)
  } 
  if(job==3.3){ # plot number of boosters with power regression
    p1 <- plot_mono(dcx_booster, 2, "number of boosters", "booster_number", 1, 3)
    p2 <- plot_mono(azy_booster, 2, "number of boosters", "booster_number", 2, 3) 
    #plot_mono_grid(dcx_booster, azy_booster, p1, p2, "booster_number", 3)
  } # error
  if(job==3.4){ # plot number of boosters with log power regression
    p1 <- plot_mono(dcx_booster, 2, "number of boosters", "booster_number", 1, 4)
    p2 <- plot_mono(azy_booster, 2, "number of boosters", "booster_number", 2, 4) # error
    #plot_mono_grid(dcx_booster, azy_booster, p1, p2, "booster_number", 4)
  }
  if(job==4.1){ # plot maximal number of sockpuppets with linear regression
    p1 <- plot_mono(dcx_booster, 4, "maximal number of sps", "sp_number", 1, 1)
    p2 <- plot_mono(azy_booster, 4, "maximal number of sps", "sp_number", 2, 1)
    plot_mono_grid(dcx_booster, azy_booster, p1, p2, "sp_number", 1)
  }
  if(job==4.2){ # plot maximal number of sockpuppets with exponential regression
    p1 <- plot_mono(dcx_booster, 4, "maximal number of sps", "sp_number", 1, 2)
    p2 <- plot_mono(azy_booster, 4, "maximal number of sps", "sp_number", 2, 2) # error
    #plot_mono_grid(dcx_booster, azy_booster, p1, p2, "sp_number", 2)
  }
  if(job==4.3){ # plot maximal number of sockpuppets with power regression
    # plot_mono <- function(trainee_df, n, variable_name, plot_name, tag, job)
    p1 <- plot_mono(dcx_booster, 4, "maximal number of sps", "sp_number", 1, 3) # error
    p2 <- plot_mono(azy_booster, 4, "maximal number of sps", "sp_number", 2, 3) # error
    #plot_mono_grid(dcx_booster, azy_booster, p1, p2, "sp_number", 3)
  } # error
  if(job==4.4){ # plot maximal number of sockpuppets with log power regression
    # plot_mono <- function(trainee_df, n, variable_name, plot_name, tag, job)
    p1 <- plot_mono(dcx_booster, 4, "maximal number of sps", "sp_number", 1, 4) # error
    p2 <- plot_mono(azy_booster, 4, "maximal number of sps", "sp_number", 2, 4) # error
    #plot_mono_grid(dcx_booster, azy_booster, p1, p2, "sp_number", 4)
  } # error
  
  if(job==4.11){ # plot number of sockpuppets of a particular booster with linear regression
    p1 <- plot_mono(dcx_booster_evo_fanclub, 7, "number of sps", "boo_sp_number", 1, 1)
    p2 <- plot_mono(azy_booster_evo_fanclub, 7, "number of sps", "boo_sp_number", 2, 1)
    plot_mono_grid(dcx_booster_evo_fanclub, azy_booster_evo_fanclub, p1, p2, "boo_sp_number", 1)
  }
  if(job==4.12){ # plot number of sockpuppets of a particular booster with exponential regression
    p1 <- plot_mono(dcx_booster_evo_fanclub, 7, "number of sps", "boo_sp_number", 1, 2)
    p2 <- plot_mono(azy_booster_evo_fanclub, 7, "number of sps", "boo_sp_number", 2, 2)
    plot_mono_grid(dcx_booster_evo_fanclub, azy_booster_evo_fanclub, p1, p2, "boo_sp_number", 2)
  }
  
  if(job==5.1){ # plot logistic parameter alpha with linear regression
    p1 <- plot_mono(dcx_para_sub, 2, "alpha", "alpha",  1, 1)
    p2 <- plot_mono(azy_para_sub, 2, "alpha", "alpha", 2, 1)
    plot_mono_grid(dcx_para_sub, azy_para_sub, p1, p2, "alpha", 1)
  }
  if(job==5.2){ # plot logistic parameter alpha with exponential regression
    p1 <- plot_mono(dcx_para_sub, 2, "alpha", "alpha",  1, 2)
    p2 <- plot_mono(azy_para_sub, 2, "alpha", "alpha", 2, 2)
    plot_mono_grid(dcx_para_sub, azy_para_sub, p1, p2, "alpha", 2)
  }
  if(job==5.3){ # plot logistic parameter alpha with power regression
    p1 <- plot_mono(dcx_para_sub, 2, "alpha", "alpha",  1, 3) # error
    p2 <- plot_mono(azy_para_sub, 2, "alpha", "alpha", 2, 3) # error
    plot_mono_grid(dcx_para_sub, azy_para_sub, p1, p2, "alpha", 3)
  } # error
  if(job==5.4){ # plot logistic parameter alpha with log power regression
    p1 <- plot_mono(dcx_para_sub, 2, "alpha", "alpha",  1, 4)
    p2 <- plot_mono(azy_para_sub, 2, "alpha", "alpha", 2, 4)
    plot_mono_grid(dcx_para_sub, azy_para_sub, p1, p2, "alpha", 4)
  } # error
  
  
  if(job==6.1){ # plot logistic parameter gamma with linear regression
    p1 <- plot_mono(dcx_para_sub, 4, "gamma", "gamma",  1, 1)
    p2 <- plot_mono(azy_para_sub, 4, "gamma", "gamma", 2, 1)
    plot_mono_grid(dcx_para_sub, azy_para_sub, p1, p2, "gamma", 1)
  }
  if(job==6.2){ # plot logistic parameter gamma with exponential regression
    p1 <- plot_mono(dcx_para_sub, 4, "gamma", "gamma",  1, 2)
    p2 <- plot_mono(azy_para_sub, 4, "gamma", "gamma", 2, 2)
    plot_mono_grid(dcx_para_sub, azy_para_sub, p1, p2, "gamma", 2)
  }
  if(job==6.3){ # plot logistic parameter gamma with power regression
    p1 <- plot_mono(dcx_para_sub, 4, "gamma", "gamma",  1, 3)
    p2 <- plot_mono(azy_para_sub, 4, "gamma", "gamma", 2, 3)
    plot_mono_grid(dcx_para_sub, azy_para_sub, p1, p2, "gamma", 3)
  }
  if(job==6.4){ # plot logistic parameter gamma with log power regression
    p1 <- plot_mono(dcx_para_sub, 4, "gamma", "gamma",  1, 4)
    p2 <- plot_mono(azy_para_sub, 4, "gamma", "gamma", 2, 4)
    plot_mono_grid(dcx_para_sub, azy_para_sub, p1, p2, "gamma", 4)
  }
  if(job==7.1){ # plot depths with linear regression
    p1 <- plot_mono(dcx_event, 4, "depth", "depth", 1, 1)
    p2 <- plot_mono(azy_event, 4, "depth", "depth", 2, 1)
    plot_mono_grid(dcx_event, azy_event, p1, p2, "depth", 1)
  }
  if(job==7.2){ # plot depths with exponential regression
    p1 <- plot_mono(dcx_event, 4, "depth", "depth", 1, 2)
    p2 <- plot_mono(azy_event, 4, "depth", "depth", 2, 2)
    plot_mono_grid(dcx_event, azy_event, p1, p2, "depth", 2)
  }
  if(job==7.3){ # plot depths with power regression
    p1 <- plot_mono(dcx_event, 4, "depth", "depth", 1, 3)
    p2 <- plot_mono(azy_event, 4, "depth", "depth", 2, 3)
    plot_mono_grid(dcx_event, azy_event, p1, p2, "depth", 3)
  }
  if(job==7.4){ # plot depths with log power regression
    p1 <- plot_mono(dcx_event, 4, "depth", "depth", 1, 4)
    p2 <- plot_mono(azy_event, 4, "depth", "depth", 2, 4)
    plot_mono_grid(dcx_event, azy_event, p1, p2, "depth", 4)
  }
  if(job==8.1){ # plot distances with linear regression
    p1 <- plot_mono(dcx_event, 6, "distance", "distance", 1, 1)
    p2 <- plot_mono(azy_event, 6, "distance", "distance", 2, 1)
    plot_mono_grid(dcx_event, azy_event, p1, p2, "distance", 1)
  }
  if(job==8.2){ # plot distances with exponential regression
    p1 <- plot_mono(dcx_event, 6, "distance", "distance", 1, 2)
    p2 <- plot_mono(azy_event, 6, "distance", "distance", 2, 2)
    plot_mono_grid(dcx_event, azy_event, p1, p2, "distance", 2)
  }
  if(job==8.3){ # plot distances with power regression
    p1 <- plot_mono(dcx_event, 6, "distance", "distance", 1, 3)
    p2 <- plot_mono(azy_event, 6, "distance", "distance", 2, 3)
    plot_mono_grid(dcx_event, azy_event, p1, p2, "distance", 3)
  }
  if(job==8.3){ # plot distances with log power regression
    p1 <- plot_mono(dcx_event, 6, "distance", "distance", 1, 3)
    p2 <- plot_mono(azy_event, 6, "distance", "distance", 2, 3)
    plot_mono_grid(dcx_event, azy_event, p1, p2, "distance", 3)
  }
  if(job==8.4){ # plot distances with log power regression
    p1 <- plot_mono(dcx_event, 6, "distance", "distance", 1, 4) # error
    p2 <- plot_mono(azy_event, 6, "distance", "distance", 2, 4) # error
    #plot_mono_grid(dcx_event, azy_event, p1, p2, "distance", 4) 
  } # error
  
}
