rm(list=ls())

########################
# Eduardo Fierro
# Some descriptive stats and data exploration
# Causal Inference
# 4/5/2018
########################

########################
# Setup
########################

library(tidyverse)
library(lubridate)
library(quanteda)
library(topicmodels)
library(doBy)
library(ggrepel)

data_dir <- "/Users/eduardofierro/Google Drive/CuartoSemestre/Causal Inference/Project/Data"
graph_dir <- "/Users/eduardofierro/Google Drive/CuartoSemestre/Causal Inference/Project/Graphs"
load(paste0(data_dir,  "/TopicModelOuts.RData"))

########################
# Topics and topics correlation
########################

# Topic 1: Campaing. America great again
# Topic 2: definitley border and military. 
# Topic 3: Work and jobs in MAGA
# Topic 4: Hillary and Obama. 
# Topic 5: Jobs and taxes. 
# Topic 6: Fake news rants. 
# Topic 7: Presidential toned tweets. 
# Topic 8: Nationalistic tweets. 
# Topic 9: Pro-trump news? 
# Topic 10: Republican primary. 

names(distribution_k10) <- paste0("Topic_", 1:10)

trump.tweets <- cbind(trump.tweets, distribution_k10)

# The problem is that teh distribution is pretty evenly distributed among most tweets. Most probas lie on the ranges of 0.1. 

########################
# Correlations - Exchange rates VS Election Data
########################

predictwise <- read.csv(paste0(data_dir, "/cleaned_predictwise.csv"), stringsAsFactors = F)
predictwise$complete_date <- ymd_hms(predictwise$complete_date)
predictwise$date <- ymd(paste(predictwise$year, predictwise$month, predictwise$day, sep="-"))
predictwise <- predictwise[,c("complete_date", "date", "percent")]
names(predictwise)[3] <- "predictwise"
predictwise_dailys <- summaryBy(predictwise ~ date, FUN=c(mean, min, max), data=predictwise)

exrate <- read.csv(paste0(data_dir, "/cleaned_exrate_wide.csv"), stringsAsFactors = F)
exrate$date <- ymd(exrate$date)


predictwise_dailys <- merge(predictwise_dailys, exrate, by="date", all.x=T, all.y = T)

plot <- ggplot() + 
        geom_point(data=predictwise_dailys, aes(x=dif_high_low, y=predictwise.mean), color="#7fcdbb") +
        geom_point(data= subset(predictwise_dailys, predictwise_dailys$dif_high_low>0.9),  
                   aes(x=dif_high_low, y=predictwise.mean), color="#253494") +
  geom_text_repel(data=subset(predictwise_dailys, predictwise_dailys$dif_high_low>0.9), 
                  aes(x=dif_high_low, y=predictwise.mean, label=as.character(date)), color="#253494") + 
        xlab("High - Low per day (MXN per USD)") + ylab("Trump's probability of win") + 
        ggtitle("Trump's probability of winning VS Exchange Rate (High/Low)") + 
        theme(axis.text.x = element_text(angle=90, size=rel(0.8), hjust=1), 
              panel.background = element_blank(), 
              panel.grid.major.x = element_line(colour = "#DBDBDB", size = rel(1)), 
              axis.line = element_line(colour = "black", size=rel(1.5)), 
              legend.position = "top", 
              legend.justification = "left")  
ggsave(paste(graph_dir, "predictwise_exrate_1.png", sep="/"), plot= plot,  width = 12, height = 12)  
  
plot <- ggplot() + 
        geom_point(data=predictwise_dailys, aes(x=dif_open_close, y=predictwise.mean), color="#7fcdbb") + 
        geom_point(data=subset(predictwise_dailys, abs(predictwise_dailys$dif_open_close)>0.45), 
                   aes(x=dif_open_close, y=predictwise.mean), color="#253494") + 
        geom_text_repel(data=subset(predictwise_dailys, abs(predictwise_dailys$dif_open_close)>0.45), 
                   aes(x=dif_open_close, y=predictwise.mean, label=as.character(date)), color="#253494") + 
        xlab("Open - Close per day (MXN per USD)") + ylab("Trump's probability of win") + 
        ggtitle("Trump's probability of winning VS Exchange Rate (Open/Close)") + 
        theme(axis.text.x = element_text(angle=90, size=rel(0.8), hjust=1), 
              panel.background = element_blank(), 
              panel.grid.major.x = element_line(colour = "#DBDBDB", size = rel(1)), 
              axis.line = element_line(colour = "black", size=rel(1.5)), 
              legend.position = "top", 
              legend.justification = "left")    
ggsave(paste(graph_dir, "predictwise_exrate_2.png", sep="/"), plot= plot,  width = 12, height = 12)  

########################
# Correlations - Exchange rates VS Trumps Tweets
########################

plot.trump.tweets <- subset(trump.tweets, trump.tweets$any==1)
plot.trump.tweets$date <- ymd(paste(plot.trump.tweets$year, plot.trump.tweets$month, plot.trump.tweets$day, sep="-"))
plot.trump.tweets <- plot.trump.tweets[,c("date", "mexico", "nafta", "wall")]
names(plot.trump.tweets) <- paste("t_", names(plot.trump.tweets), sep="")  
names(plot.trump.tweets)[1] <- "date"
plot.trump.tweets <- reshape(plot.trump.tweets, direction="long", varying=names(plot.trump.tweets)[2:4], sep="_")

exrate.plot <- subset(exrate, exrate$date>=ymd("2016-01-01"))

plot <- ggplot() + 
  geom_segment(data=plot.trump.tweets, aes(x=date, xend=date, y=0, yend=3), color="#253494") +
  geom_point(data=exrate.plot, aes(x=date, y=dif_high_low), color="#7fcdbb") + 
  geom_segment(aes(x=ymd("2016-11-08"), xend=ymd("2016-11-08"), y=0, yend=3), color="#D60000") + 
  xlab("") + ylab("High - Low per day USD/MEX") + 
  ggtitle("Exchange rate volatility and Trump's tweets regarding Mexico over time") + 
  theme(axis.text.x = element_text(angle=90, size=rel(0.8), hjust=1), 
        panel.background = element_blank(), 
        panel.grid.major.x = element_line(colour = "#DBDBDB", size = rel(1)), 
        axis.line = element_line(colour = "black", size=rel(1.5)), 
        legend.position = "top", 
        legend.justification = "left")      
ggsave(paste(graph_dir, "trump_tweets_exrate.png", sep="/"), plot= plot,  width = 12, height = 12)  

plot <- ggplot() + 
  geom_point(data=exrate, aes(x=date, y=dif_high_low), color="#7fcdbb") + 
  geom_segment(aes(x=ymd("2016-11-08"), xend=ymd("2016-11-08"), y=0, yend=3), color="#D60000") + 
  xlab("") + ylab("High - Low per day USD/MEX") + 
  ggtitle("Exchange rate volatility and Trump's tweets regarding Mexico over time") + 
  theme(axis.text.x = element_text(angle=90, size=rel(0.8), hjust=1), 
        panel.background = element_blank(), 
        panel.grid.major.x = element_line(colour = "#DBDBDB", size = rel(1)), 
        axis.line = element_line(colour = "black", size=rel(1.5)), 
        legend.position = "top", 
        legend.justification = "left")     
ggsave(paste(graph_dir, "trump_tweets_exrate_2.png", sep="/"), plot= plot,  width = 12, height = 12)  

rm(plot, exrate.plot, plot.trump.tweets)  
  
