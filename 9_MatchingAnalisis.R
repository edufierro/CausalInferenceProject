rm(list=ls())
set.seed(123)

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
library(plyr)

data_dir <- "/Users/eduardofierro/Google Drive/CuartoSemestre/Causal Inference/Project/Data"
graph_dir <- "/Users/eduardofierro/Google Drive/CuartoSemestre/Causal Inference/Project/Graphs"

########################
# Load Data
########################

complete_data <- read.csv(paste0(data_dir, "/MatchingData.csv"), stringsAsFactors=F)
complete_data$date <- ymd(complete_data$date)
data_matching <- subset(complete_data, complete.cases(complete_data))

########################
# Working Matching specification
########################

fmla <- as.formula("tw_any.ifelse ~ me_amlo +
                   mb_resInt + 
                   oc_indszus + oc_indkous + 
                   pw_predictwise 
                   ")

final_match <- matchit(fmla, data=data_matching, method="nearest", distance="mahalanobis") 
love.plot(bal.tab(final_match), threshold = .1)

matched_sample <- match.data(final_match)
# ATT on full matched data:
with(matched_sample, t.test(er_index ~ tw_any.ifelse))
with(matched_sample, t.test(er_difHighLow ~ tw_any.ifelse))

# ATT before election:
with(subset(matched_sample, matched_sample$pw_predictwise<=99), t.test(er_index ~ tw_any.ifelse))
with(subset(matched_sample, matched_sample$pw_predictwise<=99), t.test(er_difHighLow ~ tw_any.ifelse))

# ATT after election:
with(subset(matched_sample, matched_sample$pw_predictwise>99), t.test(er_index ~ tw_any.ifelse))
with(subset(matched_sample, matched_sample$pw_predictwise>99), t.test(er_difHighLow ~ tw_any.ifelse))


########################
# Sensitivity analysis on Observatins
########################

### FUll mathcing 
set.seed(123)
final_match_full <- matchit(fmla, data=data_matching, method="full", distance="mahalanobis") 
love.plot(bal.tab(final_match_full), threshold = .1) # This sucks

### Observations 
set.seed(123)
getVals <- function(ttest, type_, var_, sample_, seed_){
  pval_ <- ttest$p.value
  val_ <- ttest$estimate[2] - ttest$estimate[1]
  attributes(val_) <- NULL
  return(data.frame(pval = pval_, val = val_, var=var_, type=type_, sample=sample_, seed=seed_))
}

results <- data.frame()
for(x in 1:500){
  random.seed <- sample(1:1000000, 1)
  set.seed(random.seed)
  temp.match <- matchit(fmla, data=data_matching, method="nearest", distance="mahalanobis")
  temp.matched_sample <- match.data(temp.match)
  results <- rbind.fill(results, 
                        getVals(with(temp.matched_sample, t.test(er_index ~ tw_any.ifelse)), 
                                var_ = "er_index", type_ = "All", sample_ = x, seed_ = random.seed))
  results <- rbind.fill(results, 
                        getVals(with(temp.matched_sample, t.test(er_difHighLow ~ tw_any.ifelse)), 
                                var_ = "er_difHighLow", type_ = "All", sample_ = x, seed_ = random.seed))
  results <- rbind.fill(results, 
                        getVals(with(subset(temp.matched_sample, temp.matched_sample$pw_predictwise<=99), t.test(er_index ~ tw_any.ifelse)), 
                                var_ = "er_index", type_ = "Before", sample_ = x, seed_ = random.seed)) 
  results <- rbind.fill(results, 
                        getVals(with(subset(temp.matched_sample, temp.matched_sample$pw_predictwise<=99), t.test(er_difHighLow ~ tw_any.ifelse)), 
                                var_ = "er_difHighLow", type_ = "Before", sample_ = x, seed_ = random.seed))   
  results <- rbind.fill(results, 
                        getVals(with(subset(temp.matched_sample, temp.matched_sample$pw_predictwise>99), t.test(er_index ~ tw_any.ifelse)), 
                                var_ = "er_index", type_ = "After", sample_ = x, seed_ = random.seed)) 
  results <- rbind.fill(results, 
                        getVals(with(subset(temp.matched_sample, temp.matched_sample$pw_predictwise>99), t.test(er_difHighLow ~ tw_any.ifelse)), 
                                var_ = "er_difHighLow", type_ = "After", sample_ = x, seed_ = random.seed))   
  rm(random.seed, temp.match, temp.matched_sample)
}

plot <- ggplot(data=subset(results, results$type=="After" & results$var=="er_difHighLow"), aes(pval)) + 
  geom_density(color="#253494", fill="#7fcdbb") + 
  xlab("pvalues") + ylab("Density") + 
  ggtitle("Histogram of pvalues for After Election sub-sample and High-Low") + 
  theme(axis.text.x = element_text(angle=90, size=rel(0.8), hjust=1), 
        panel.background = element_blank(), 
        panel.grid.major.x = element_line(colour = "#DBDBDB", size = rel(1)), 
        axis.line = element_line(colour = "black", size=rel(1.5)), 
        legend.position = "top", 
        legend.justification = "left")     
ggsave(paste(graph_dir, "500Samples.png", sep="/"), plot= plot,  width = 12, height = 12)  


########################
# Working Matching specification: Date
########################

set.seed(123)
fmla <- as.formula("tw_any.ifelse ~ me_amlo +
                   mb_resInt + 
                   oc_indszus + oc_indkous + 
                   pw_predictwise + date
                   ")

final_match <- matchit(fmla, data=data_matching, method="nearest", distance="mahalanobis") 
png(filename=paste0(graph_dir, "/CovariatePlots/FinalWithDate.png"))
love.plot(bal.tab(final_match), threshold = .1)
dev.off()


set.seed(123)
results <- data.frame()
for(x in 1:500){
  random.seed <- sample(1:1000000, 1)
  set.seed(random.seed)
  temp.match <- matchit(fmla, data=data_matching, method="nearest", distance="mahalanobis")
  temp.matched_sample <- match.data(temp.match)
  results <- rbind.fill(results, 
                        getVals(with(temp.matched_sample, t.test(er_index ~ tw_any.ifelse)), 
                                var_ = "er_index", type_ = "All", sample_ = x, seed_ = random.seed))
  results <- rbind.fill(results, 
                        getVals(with(temp.matched_sample, t.test(er_difHighLow ~ tw_any.ifelse)), 
                                var_ = "er_difHighLow", type_ = "All", sample_ = x, seed_ = random.seed))
  results <- rbind.fill(results, 
                        getVals(with(subset(temp.matched_sample, temp.matched_sample$pw_predictwise<=99), t.test(er_index ~ tw_any.ifelse)), 
                                var_ = "er_index", type_ = "Before", sample_ = x, seed_ = random.seed)) 
  results <- rbind.fill(results, 
                        getVals(with(subset(temp.matched_sample, temp.matched_sample$pw_predictwise<=99), t.test(er_difHighLow ~ tw_any.ifelse)), 
                                var_ = "er_difHighLow", type_ = "Before", sample_ = x, seed_ = random.seed))   
  results <- rbind.fill(results, 
                        getVals(with(subset(temp.matched_sample, temp.matched_sample$pw_predictwise>99), t.test(er_index ~ tw_any.ifelse)), 
                                var_ = "er_index", type_ = "After", sample_ = x, seed_ = random.seed)) 
  results <- rbind.fill(results, 
                        getVals(with(subset(temp.matched_sample, temp.matched_sample$pw_predictwise>99), t.test(er_difHighLow ~ tw_any.ifelse)), 
                                var_ = "er_difHighLow", type_ = "After", sample_ = x, seed_ = random.seed))   
  rm(random.seed, temp.match, temp.matched_sample)
}

plot <- ggplot(data=subset(results, results$type=="After" & results$var=="er_difHighLow"), aes(pval)) + 
  geom_density(color="#253494", fill="#7fcdbb") + 
  xlab("pvalues") + ylab("Density") + 
  ggtitle("Histogram of pvalues for After Election sub-sample and High-Low \n Including Date as covariate") + 
  theme(axis.text.x = element_text(angle=90, size=rel(0.8), hjust=1), 
        panel.background = element_blank(), 
        panel.grid.major.x = element_line(colour = "#DBDBDB", size = rel(1)), 
        axis.line = element_line(colour = "black", size=rel(1.5)), 
        legend.position = "top", 
        legend.justification = "left")     
ggsave(paste(graph_dir, "500Samples_2.png", sep="/"), plot= plot,  width = 12, height = 12)  

plot <- ggplot(data=subset(results, results$type=="After" & results$var=="er_difHighLow"), aes(val)) + 
  geom_density(color="#253494", fill="#7fcdbb") + 
  xlab("ATT") + ylab("Density") + 
  ggtitle("Histogram of ATT for After Election sub-sample and High-Low \n Including Date as covariate") + 
  theme(axis.text.x = element_text(angle=90, size=rel(0.8), hjust=1), 
        panel.background = element_blank(), 
        panel.grid.major.x = element_line(colour = "#DBDBDB", size = rel(1)), 
        axis.line = element_line(colour = "black", size=rel(1.5)), 
        legend.position = "top", 
        legend.justification = "left")   
ggsave(paste(graph_dir, "500Samples_ATT2.png", sep="/"), plot= plot,  width = 12, height = 12)  

########################
# Sensitivity analysis on Variables
########################

set.seed(123)
fmla <- as.formula("tw_any.ifelse ~ me_amlo +
                   mb_resInt + 
                   oc_indszus + oc_indkous + 
                   pw_predictwise + date + tw_topic10.mean
                   ")

# Twitter: 
# 2, 5, 7, 8, 10 NOT BALANCE
final_match <- matchit(fmla, data=data_matching, method="nearest", distance="mahalanobis") 
love.plot(bal.tab(final_match), threshold = .1)
