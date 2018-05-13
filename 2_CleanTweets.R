rm(list=ls())

########################
# Eduardo Fierro
# Clean Trump tweets
# Causal Inference
# 20/4/2018
########################

########################
# Setup
########################

library(rjson)
library(tidyverse)
library(lubridate)

data_dir <- "/Users/eduardofierro/Google Drive/CuartoSemestre/Causal Inference/Project/Data/TweetsOriginal"
graph_dir <- "/Users/eduardofierro/Google Drive/CuartoSemestre/Causal Inference/Project/Graphs"
out_dir <- "/Users/eduardofierro/Google Drive/CuartoSemestre/Causal Inference/Project/Data"

########################
# Import data and bind 
########################

importSingle <- function(filepath){
    json_data <- suppressWarnings(fromJSON(readLines(filepath)))
                 # All files give me an incomplete final line  for readLines. 
    json_data <- unnest(as.data.frame(do.call(rbind, json_data)))
    return(json_data)
}

importData <- function(years, dir=data_dir){
    main_data <- importSingle(paste0(dir, "/tweets_", years[1], ".json"))
    for(x in 2:length(years)){
        data <- importSingle(paste0(dir, "/tweets_", years[x], ".json"))
        main_data <- rbind(main_data, data)
    }
    main_data <- subset(main_data, is.na(main_data$id_str)==F)
    return(main_data)
}

trump_data <- importData(seq(2014, 2018))

########################
# Clean Data 
########################

# keep only non-RTs
trump_data <- subset(trump_data, trump_data$is_retweet==F)
trump_data$is_retweet <- NULL

# Date variables
date_data <- str_split(trump_data$created_at, " ")
date_data <- do.call(rbind, date_data)
trump_data$dow <- date_data[,1]
trump_data$month <- date_data[,2]
trump_data$day <- date_data[,3]
trump_data$time <- date_data[,4]
trump_data$year <- date_data[,6]
trump_data$created_at <- NULL
rm(date_data)

trump_data$complete_date <- ymd_hms(paste0(trump_data$year,"-", trump_data$month,"-",trump_data$day, " ",
                                      trump_data$time))

# Dummy if mentions mexico

dummyPatern <- function(pattern, mytext, is_fixed=T, is_perl=F, is_lowercase=F){
  
  myvec <-  as.numeric(grepl(pattern[1], mytext, fixed=is_fixed, perl = is_perl, ignore.case = is_lowercase))
  
  for(x in 1:length(pattern)){
    myvec2 <- as.numeric(grepl(pattern[x], mytext, fixed=is_fixed, perl = is_perl, ignore.case = is_lowercase))
    myvec <- myvec + myvec2
  }
  myvec <- ifelse(myvec>0,1,0)
  return(myvec)
  
}


trump_data$mexico <- dummyPatern(c("México", "Mexico", "mexico", "méxico"), trump_data$text)
trump_data$nafta <- dummyPatern(c("NAFTA", "nafta", "Nafta"), trump_data$text, is_fixed = F, is_lowercase=T)
trump_data$wall <- dummyPatern(c("The Wall", "The wall", "the wall"), trump_data$text)
trump_data$any <- ifelse(trump_data$mexico + trump_data$nafta + trump_data$wall>0, 1,0) 

########################
# Quick Graph
########################

data.plot <-  subset(trump_data, trump_data$any==1)
data.plot$plot_var <- ""
data.plot$plot_var[data.plot$mexico==1 & data.plot$nafta==1 & data.plot$wall==1] <- "All"
data.plot$plot_var[data.plot$mexico==0 & data.plot$nafta==1 & data.plot$wall==1] <- "NAFTA and The Wall"
data.plot$plot_var[data.plot$mexico==0 & data.plot$nafta==0 & data.plot$wall==1] <- "The Wall"
data.plot$plot_var[data.plot$mexico==1 & data.plot$nafta==0 & data.plot$wall==1] <- "Mexico and The Wall"
data.plot$plot_var[data.plot$mexico==1 & data.plot$nafta==1 & data.plot$wall==0] <- "Mexico and NAFTA"
data.plot$plot_var[data.plot$mexico==1 & data.plot$nafta==0 & data.plot$wall==0] <- "Mexico"
data.plot$plot_var[data.plot$mexico==0 & data.plot$nafta==1 & data.plot$wall==0] <- "NAFTA"

plot <- ggplot() + 
        geom_line(data=trump_data, aes(x=complete_date, y=retweet_count/1000), color="#0c2c84") + 
        geom_point(data=data.plot, aes(x=complete_date, y=retweet_count/1000, color=plot_var)) + 
        scale_color_manual(values=c("#8dd3c7", "#ffffb3", "#bebada", "#fb8072", "#80b1d3", "#fdb462"), name="Type") +
        theme_bw() + 
        labs(y="Thousand RTs", x="", title="RTs of every trump tweet") 
ggsave(paste(graph_dir, "TweetsDescriptive1.png", sep="/"), plot= plot,  width = 12, height = 12)  

  
#######################
# Export
########################

write.csv(trump_data, paste0(out_dir, "/cleaned_tweets.csv"), row.names = F, na = )
