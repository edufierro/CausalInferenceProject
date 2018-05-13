rm(list=ls())

########################
# Eduardo Fierro
# Matching tryouts
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
library(MatchIt)
library(cobalt)

data_dir <- "/Users/eduardofierro/Google Drive/CuartoSemestre/Causal Inference/Project/Data"
graph_dir <- "/Users/eduardofierro/Google Drive/CuartoSemestre/Causal Inference/Project/Graphs"

########################
# Load Data / Create Data
########################

###### TRUMP TWEETS ######
load(paste0(data_dir,  "/TopicModelOuts.RData"))
names(distribution_k10) <- paste0("topic", 1:10)
trump.tweets <- cbind(trump.tweets, distribution_k10)
rm(list=setdiff(ls(), c("data_dir", "graph_dir", "trump.tweets")))
trump.tweets$complete_date <- ymd_hms(trump.tweets$complete_date)
trump.tweets$date <- ymd(paste(year(trump.tweets$complete_date), month(trump.tweets$complete_date), day(trump.tweets$complete_date), sep="-"))
trump.tweets <- subset(trump.tweets, year(trump.tweets$date) %in% c(2016, 2017))
trump.tweets <- summaryBy(mexico + nafta + wall + any + 
                          topic1 + topic2 + topic3 + topic4 + topic5 + 
                          topic6 + topic7 + topic8 + topic9 + topic10 ~ 
                          date, FUN=c(sum, mean), data=trump.tweets)
trump.tweets$mexico.ifelse <- ifelse(trump.tweets$mexico.sum>0,1,0)
trump.tweets$nafta.ifelse <- ifelse(trump.tweets$nafta.sum>0,1,0)
trump.tweets$wall.ifelse <- ifelse(trump.tweets$wall.sum>0,1,0)
trump.tweets$any.ifelse <- ifelse(trump.tweets$any.sum>0,1,0)
trump.tweets$topic1.sum <- trump.tweets$topic2.sum <- trump.tweets$topic3.sum <- NULL
trump.tweets$topic4.sum <- trump.tweets$topic5.sum <- trump.tweets$topic6.sum <- NULL
trump.tweets$topic7.sum <- trump.tweets$topic8.sum <- trump.tweets$topic9.sum <- NULL
trump.tweets$topic10.sum <- NULL
names(trump.tweets)[2:ncol(trump.tweets)] <- paste("tw", names(trump.tweets)[2:ncol(trump.tweets)], sep="_")

###### EXCHANGE RATE ######
exrate <- read.csv(paste0(data_dir, "/cleaned_exrate_wide.csv"), stringsAsFactors = F)
exrate$date <- ymd(exrate$date)
reference <- subset(exrate, exrate$date==ymd("2013-12-31"))$close
exrate$index <- exrate$close / reference
exrate <- subset(exrate, year(exrate$date) %in% c(2016, 2017))
names(exrate) <- c("date", "open", "high", "low", "close", "difOpenClose", "difHighLow", "perDifOpenClose", "index")
rm(reference)

###### WDS data ######
other.currencies <- read.csv(paste0(data_dir, "/cleaned_FX_WDS.csv"), stringsAsFactors = F)
other.currencies$date <- ymd(other.currencies$date)
other.currencies <- subset(other.currencies, year(other.currencies$date) %in% c(2016, 2017))
other.currencies <- other.currencies[,c(1, 27:ncol(other.currencies))]
other.currencies$indvzus <- other.currencies$indmxus <- other.currencies$indhkus <-  NULL
# Venezuela, Mexico and Hong Kong (Which is one)
names(other.currencies)[2:ncol(other.currencies)] <- paste("oc", names(other.currencies)[2:ncol(other.currencies)], sep="_")

other.indexes <- read.csv(paste0(data_dir, "/cleaned_FX_INDEXES.csv"), stringsAsFactors = F)
other.indexes$date <- ymd(other.indexes$date)
other.indexes <- other.indexes[,c(1:4)] 
other.indexes <- subset(other.indexes, year(other.indexes$date) %in% c(2016, 2017))
# indexgx has only NAs. The rest is day/month/year which is in date
names(other.indexes)[2:ncol(other.indexes)] <- paste("oi", names(other.indexes)[2:ncol(other.indexes)], sep="_")


###### Mexico's election ######
mex.elections <- read.csv(paste0(data_dir, "/cleaned_mex_elections.csv"), stringsAsFactors = F)
mex.elections$date <- ymd(mex.elections$date)
mex.elections <- subset(mex.elections, year(mex.elections$date) %in% c(2016, 2017))
mex.elections <- mex.elections[,c(4:8)]
mex.elections$other_nr <- NULL
mex.elections <- summaryBy(meade + amlo + anaya  ~ date, FUN=mean, data=mex.elections, keep.names=T)
complete_dates <- data.frame(date = seq(ymd('2016-01-01'),ymd('2017-12-31'), by="day"))
mex.elections <- merge(complete_dates, mex.elections, by="date", all.x=T, all.y=F)
rm(complete_dates)
current_vals <- c(NA,NA,NA)
for(row in 1:nrow(mex.elections)){
  if(is.na(mex.elections[row,2])){
    mex.elections[row,2:4] <- current_vals
  } else {
    current_vals <- mex.elections[row,2:4] 
  }
}
rm(row, current_vals)
mex.elections$meade[is.na(mex.elections$meade)] <- mex.elections$meade[18]
mex.elections$amlo[is.na(mex.elections$amlo)] <- mex.elections$amlo[18]
mex.elections$anaya[is.na(mex.elections$anaya)] <- mex.elections$anaya[18]
names(mex.elections) <- c("date", "me_meade", "me_amlo", "me_anaya")

###### Mexico's central bank ######
mex.bank <- read.csv(paste0(data_dir, "/cleaned_central_bank.csv"), stringsAsFactors = F)
mex.bank$date <- ymd(mex.bank$date)
mex.bank <- mex.bank[,c(4:6)]
complete_dates <- data.frame(date = seq(ymd('2016-01-01'),ymd('2017-12-31'), by="day"))
mex.bank.2 <- merge(complete_dates, mex.bank, by="date", all.x=T, all.y=T)
rm(complete_dates)
for(row in 1:nrow(mex.bank)){
  changes_res_int <- (mex.bank$res_int[row + 1] - mex.bank$res_int[row]) / (as.numeric(mex.bank$date[row + 1] - mex.bank$date[row] ))
  changes_act_int  <- (mex.bank$act_int[row + 1] - mex.bank$act_int[row]) / (as.numeric(mex.bank$date[row + 1] - mex.bank$date[row]))
  mex.bank.2$res_int[mex.bank.2$date > mex.bank$date[row] & mex.bank.2$date < mex.bank$date[row + 1]] <- changes_res_int
  mex.bank.2$act_int[mex.bank.2$date > mex.bank$date[row] & mex.bank.2$date < mex.bank$date[row + 1]] <- changes_act_int
  rm(changes_res_int, changes_act_int)
}
current_res_int <- NA
current_act_int <- NA
for(row in 1:nrow(mex.bank.2)){
   if(mex.bank.2$date[row] %in% mex.bank$date){
     current_res_int <- mex.bank.2$res_int[row]
     current_act_int <- mex.bank.2$act_int[row]
   } else {
     mex.bank.2$res_int[row] <-  current_res_int + mex.bank.2$res_int[row]
     mex.bank.2$act_int[row] <-  current_act_int + mex.bank.2$act_int[row]
     current_res_int <- mex.bank.2$res_int[row]
     current_act_int <- mex.bank.2$act_int[row]
   }
}
mex.bank <- mex.bank.2
rm(row, current_res_int, current_act_int, mex.bank.2)
mex.bank <- subset(mex.bank, year(mex.bank$date) %in% c(2016, 2017))
names(mex.bank) <- c("date", "mb_resInt", "mb_actInt")

###### Predictwise ######
predictwise.data <- read.csv(paste0(data_dir, "/cleaned_predictwise.csv"), stringsAsFactors = F)
predictwise.data$complete_date <- ymd_hms(predictwise.data$complete_date)
predictwise.data$date <- ymd(paste(year(predictwise.data$complete_date), 
                                   month(predictwise.data$complete_date),
                                   day(predictwise.data$complete_date), sep="-"))
names(predictwise.data)[names(predictwise.data)=="percent"] <- "predictwise"
predictwise.data <- summaryBy(predictwise~date, FUN=mean, data=predictwise.data, keep.names = T)
predictwise.data <- subset(predictwise.data, year(predictwise.data$date) %in% c(2016, 2017))
complete_dates <- data.frame(date = seq(ymd('2016-01-01'),ymd('2017-12-31'), by="day"))
predictwise.data <- merge(complete_dates, predictwise.data, by="date", all.x=T, all.y=T)
rm(complete_dates)
current_val <- NA
for(row in 1:nrow(predictwise.data)){
  if(is.na(predictwise.data$predictwise[row])){
    predictwise.data$predictwise[row] <- current_val
  } else {
    current_val <- predictwise.data$predictwise[row]
  }
}
rm(row, current_val)
names(predictwise.data)[2] <- "pw_predictwise"

###### All together ######
complete_data <- data.frame(date = seq(ymd('2016-01-01'),ymd('2017-12-31'), by="day"))
complete_data <- merge(complete_data, exrate, by="date", all=T)
names(complete_data)[2:9] <- paste("er", names(complete_data)[2:9], sep="_")
complete_data <- merge(complete_data, mex.bank, by="date", all=T)
complete_data <- merge(complete_data, mex.elections, by="date", all=T)
complete_data <- merge(complete_data, other.currencies, by="date", all=T)
complete_data <- merge(complete_data, other.indexes, by="date", all=T)
complete_data <- merge(complete_data, predictwise.data, by="date", all=T)
complete_data <- merge(complete_data, trump.tweets, by="date", all=T)

write.csv(complete_data, paste0(data_dir, "/MatchingData.csv"))
  
########################
# Matching!
########################

###### Trial1 - ALL. Logistic distance. Nearest neighbors ######
data_matching <- subset(complete_data, complete.cases(complete_data))
fmla <- as.formula("tw_any.ifelse ~ me_meade + me_amlo + me_anaya +
                                    oc_indalus + oc_indbzus + oc_indcaus + oc_indchus + oc_inddnus + oc_indinus + oc_indjpus + oc_indkous + oc_indmaus + oc_indnzus + oc_indnous + oc_indsdus + oc_indsfus + oc_indsius + oc_indslus + oc_indszus + oc_indtaus + oc_indthus + oc_indukus + 
                                    oi_twexb + oi_twexm +
                                    mb_resInt + mb_actInt +
                                    pw_predictwise + 
                                    tw_topic1.mean + tw_topic2.mean + tw_topic3.mean + tw_topic4.mean + tw_topic5.mean + tw_topic6.mean + tw_topic7.mean + tw_topic8.mean + tw_topic9.mean + tw_topic10.mean
                                    ")
match_trial1 <- matchit(fmla, data=data_matching, method="nearest")
plot_trial1 <- love.plot(bal.tab(match_trial1), threshold = .1)
png(filename=paste0(graph_dir, "/CovariatePlots/All_Logit.png"))
love.plot(bal.tab(match_trial1), threshold = .1)
dev.off()

match_trial1.2 <- matchit(fmla, data=data_matching, method="nearest", distance="mahalanobis")
plot_trial1.2 <- love.plot(bal.tab(match_trial1.2), threshold = .1)
png(filename=paste0(graph_dir, "/CovariatePlots/All_Mahalanobis.png"))
love.plot(bal.tab(match_trial1.2), threshold = .1)
dev.off()

match_trial1.3 <- matchit(fmla, data=data_matching, method="nearest", distance="linear.logit")
plot_trial1.3 <- love.plot(bal.tab(match_trial1.3), threshold = .1)
png(filename=paste0(graph_dir, "/CovariatePlots/All_LinLogit.png"))
love.plot(bal.tab(match_trial1.3), threshold = .1)
dev.off()

###### Trial2 - Cherry picked Logistic distance. Nearest neighbors ######

fmla <- as.formula("tw_any.ifelse ~ me_meade + me_amlo + me_anaya +
                                    mb_resInt + mb_actInt +
                                    oc_indszus + oc_indkous + 
                                    pw_predictwise + 
                                    tw_topic1.mean + tw_topic2.mean + tw_topic3.mean + tw_topic4.mean + tw_topic5.mean + tw_topic6.mean + tw_topic7.mean + tw_topic8.mean + tw_topic9.mean + tw_topic10.mean
                                    ")
match_trial2 <- matchit(fmla, data=data_matching, method="nearest")
plot_trial2 <- love.plot(bal.tab(match_trial2), threshold = .1)
png(filename=paste0(graph_dir, "/CovariatePlots/Trial2_Logit.png"))
love.plot(bal.tab(match_trial2), threshold = .1)
dev.off()

match_trial2.2 <- matchit(fmla, data=data_matching, method="nearest", distance="mahalanobis")
plot_trial2.2 <- love.plot(bal.tab(match_trial2.2), threshold = .1)
png(filename=paste0(graph_dir, "/CovariatePlots/Trial2_Mahalanobis.png"))
love.plot(bal.tab(match_trial2.2), threshold = .1)
dev.off()

match_trial2.3 <- matchit(fmla, data=data_matching, method="nearest", distance="linear.logit")
plot_trial2.3 <- love.plot(bal.tab(match_trial2.3), threshold = .1)
png(filename=paste0(graph_dir, "/CovariatePlots/Trial2_LinLogit.png"))
love.plot(bal.tab(match_trial2.3), threshold = .1)
dev.off()

###### Trial3 - Cherry picked Logistic distance. Nearest neighbors ######

fmla <- as.formula("tw_any.ifelse ~ me_amlo +
                   mb_resInt + 
                   oc_indszus + oc_indkous + 
                   pw_predictwise + 
                   tw_topic2.mean + tw_topic3.mean
                   ")
match_trial3 <- matchit(fmla, data=data_matching, method="nearest")
plot_trial3 <- love.plot(bal.tab(match_trial3), threshold = .1)
png(filename=paste0(graph_dir, "/CovariatePlots/Trial3_Logit.png"))
love.plot(bal.tab(match_trial3), threshold = .1)
dev.off()

match_trial3.2 <- matchit(fmla, data=data_matching, method="nearest", distance="mahalanobis")
plot_trial3.2 <- love.plot(bal.tab(match_trial3.2), threshold = .1)
png(filename=paste0(graph_dir, "/CovariatePlots/Trial3_Mahalanobis.png"))
love.plot(bal.tab(match_trial3.2), threshold = .1)
dev.off()

match_trial3.3 <- matchit(fmla, data=data_matching, method="nearest", distance="linear.logit")
plot_trial3.3 <- love.plot(bal.tab(match_trial3.3), threshold = .1)
png(filename=paste0(graph_dir, "/CovariatePlots/Trial3_LinLogit.png"))
love.plot(bal.tab(match_trial3.3), threshold = .1)
dev.off()


###### Trial4 - Cherry picked Logistic distance. Nearest neighbors ######

fmla <- as.formula("tw_any.ifelse ~ me_amlo +
                   mb_resInt + 
                   oc_indszus + oc_indkous + 
                   pw_predictwise 
                   ")
match_trial4 <- matchit(fmla, data=data_matching, method="nearest")
plot_trial4 <- love.plot(bal.tab(match_trial4), threshold = .1)
png(filename=paste0(graph_dir, "/CovariatePlots/Trial4_Logit.png"))
love.plot(bal.tab(match_trial4), threshold = .1)
dev.off()

match_trial4.2 <- matchit(fmla, data=data_matching, method="nearest", distance="mahalanobis") # THIS WORKS!
plot_trial4.2 <- love.plot(bal.tab(match_trial4.2), threshold = .1)
png(filename=paste0(graph_dir, "/CovariatePlots/Trial4_Mahalanobis.png"))
love.plot(bal.tab(match_trial4.2), threshold = .1)
dev.off()

match_trial4.3 <- matchit(fmla, data=data_matching, method="nearest", distance="linear.logit")
plot_trial4.3 <- love.plot(bal.tab(match_trial4.3), threshold = .1)
png(filename=paste0(graph_dir, "/CovariatePlots/Trial4_LinLogit.png"))
love.plot(bal.tab(match_trial4.3), threshold = .1)
dev.off()