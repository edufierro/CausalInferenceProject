rm(list=ls())

########################
# Eduardo Fierro
# Clean Exchange Rate Data
# Causal Inference
# 20/4/2018
########################

########################
# Setup
########################

library(tidyverse)
library(lubridate)

data_dir <- "/Users/eduardofierro/Google Drive/CuartoSemestre/Causal Inference/Project/Data/ExchangeRate"
graph_dir <- "/Users/eduardofierro/Google Drive/CuartoSemestre/Causal Inference/Project/Graphs"
out_dir <- "/Users/eduardofierro/Google Drive/CuartoSemestre/Causal Inference/Project/Data"

########################
# Import data 
########################

main.data <- read.csv(paste(data_dir, "globalfinancialdata__daily.csv", sep="/"), skip=2, header=T, stringsAsFactors = F)
main.data$Openint <- main.data$Volume <- NULL # All are NAs
main.data$Ticker <- NULL # All are "USDMXN"

names(main.data) <- tolower(names(main.data))

main.data$date <- mdy(main.data$date)
main.data$dif_open_close <- main.data$open - main.data$close
main.data$dif_high_low <- main.data$high - main.data$low
main.data$per_dif_open_close <- 100*(main.data$close - main.data$open) / main.data$open

# Create a long data
long.data <- main.data[c("date", "open", "close")]
names(long.data)[2:3] <- c("rate_open", "rate_close")
long.data <- reshape(long.data, varying= names(long.data)[2:3], sep="_", direction = "long")
row.names(long.data) <- seq(from=1, to=nrow(long.data))
long.data$id <- NULL
long.data$time[long.data$time=="open"] <- "9:00"
long.data$time[long.data$time=="close"] <- "16:00"
long.data$date <- ymd_hm(paste0(as.character(long.data$date), long.data$time, sep=" "))
long.data$time <- NULL
long.data <- long.data[order(long.data$date),] 
row.names(long.data) <- seq(from=1, to=nrow(long.data))

# Create indexed data. Last data point of 2013 = 1
reference <- subset(long.data, long.data$date == ymd_hm("2013-12-31 16:00"))$rate # 13.0315
long.data$indexed <- long.data$rate / reference

########################
# Quick plot using long data
########################

plot <- ggplot(data=long.data, aes(x=date, y=indexed)) + 
  geom_line(color="#0c2c84") + 
  geom_hline(aes(yintercept=1), color="red") + 
  theme_bw() + 
  labs(y="Indexed exchange rate (12/31/13 = 1)", x="", title="Exchange rate variation (MXN/USD)") 
ggsave(paste(graph_dir, "ExcRateDescriptive.png", sep="/"), plot= plot,  width = 12, height = 12)  

#######################
# Export
########################

write.csv(long.data, paste0(out_dir, "/cleaned_exrate_long.csv"), row.names = F, na = )
write.csv(main.data, paste0(out_dir, "/cleaned_exrate_wide.csv"), row.names = F, na = )
