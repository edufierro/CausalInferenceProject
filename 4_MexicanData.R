rm(list=ls())

########################
# Eduardo Fierro
# Clean Mexican Data
# Causal Inference
# 4/5/2018
########################

########################
# Setup
########################

library(tidyverse)
library(lubridate)
library(doBy)
library(readxl)

data_dir <- "/Users/eduardofierro/Google Drive/CuartoSemestre/Causal Inference/Project/Data"
graph_dir <- "/Users/eduardofierro/Google Drive/CuartoSemestre/Causal Inference/Project/Graphs"

########################
# Elections data
########################

mexican_elections <- read_excel(paste0(data_dir, "/Mexico/OraculusParties.xlsx"))
mexican_elections <- data.frame(mexican_elections)

mydates <- str_split(mexican_elections$Fecha, " ")
mydates <- do.call(rbind, mydates)

get.month <- function(myvec){
  
  myvec[myvec=="Apr"] <- "4"
  myvec[myvec=="Aug"] <- "8"
  myvec[myvec=="Dec"] <- "12"
  myvec[myvec=="Feb"] <- "2"
  myvec[myvec=="Jan"] <- "1"
  myvec[myvec=="Jul"] <- "7"
  myvec[myvec=="Jun"] <- "6"
  myvec[myvec=="Mar"] <- "3"
  myvec[myvec=="May"] <- "5"
  myvec[myvec=="Nov"] <- "11"
  myvec[myvec=="Oct"] <- "10"
  myvec[myvec=="Sep"] <- "9"
  
  if(sum(myvec %in% as.character(1:12))<length(myvec)){
    warning("Some months were not updated")
  }
  
  return(myvec)
  
}

mexican_elections$day <- as.numeric(mydates[,1])
mexican_elections$month <- get.month(mydates[,2])
mexican_elections$year <- as.numeric(mydates[,3])
mexican_elections$date <- ymd(paste(mexican_elections$year, mexican_elections$month, mexican_elections$day, sep="-"))
mexican_elections$meade <- mexican_elections$PRI + mexican_elections$PVEM + mexican_elections$PANAL
mexican_elections$amlo <- mexican_elections$MORENA + mexican_elections$PES + mexican_elections$PT
mexican_elections$anaya <- mexican_elections$PAN + mexican_elections$PRD + mexican_elections$MC
mexican_elections$other_nr <- mexican_elections$NR + mexican_elections$Indep

mexican_elections <- mexican_elections[,17:ncol(mexican_elections)]

write.csv(mexican_elections, paste0(data_dir, "/cleaned_mex_elections.csv"), row.names = F, na = "")
rm(mexican_elections, mydates, get.month)

########################
# Central bank data
########################

cent_bank <- read.csv(paste0(data_dir, "/Mexico/Banxico.csv"), stringsAsFactors = F, skip = 17)

mydates <- str_split(cent_bank$Fecha, "/")
mydates <- do.call(rbind, mydates)
cent_bank$day <- as.numeric(mydates[,1])
cent_bank$month <- as.numeric(mydates[,2])
cent_bank$year <- as.numeric(mydates[,3])
cent_bank$date <- ymd(paste(cent_bank$year, cent_bank$month, cent_bank$day, sep="-"))

rm(mydates)

# Keeping net numbers in dollars. 
cent_bank$res_int <- cent_bank$SF43707
cent_bank$act_int <- cent_bank$SF43704

cent_bank <- cent_bank[,11:ncol(cent_bank)]

write.csv(cent_bank, paste0(data_dir, "/cleaned_central_bank.csv"), row.names = F, na = "")

