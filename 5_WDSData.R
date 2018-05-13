rm(list=ls())

########################
# Eduardo Fierro
# Clean Wharton Data
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
# Data
########################

all.data <- read.table(paste0(data_dir, "/ExchangeRate/FX_WDS.txt"), stringsAsFactors = F)
names(all.data) <- all.data[1,]
all.data <- all.data[2:nrow(all.data),]
all.data[,2:ncol(all.data)] <- suppressWarnings(lapply(all.data[,2:ncol(all.data)], as.numeric) )# Some warnings, because of points indicating missing vals.

mydates <- str_split(all.data$date, "/")
mydates <- do.call(rbind, mydates)
all.data$year <- as.numeric(mydates[,1])
all.data$month <- as.numeric(mydates[,2])
all.data$day <- as.numeric(mydates[,3])
all.data$date <- ymd(paste(all.data$year, all.data$month, all.data$day, sep="-"))
rm(mydates)

# Indexing (as code 1_CleanExchangeRate): Last data point of 2013 = 1.
reference <- subset(all.data, all.data$date==ymd("2013-12-31")) # No missing values. Ok. 
temp.data <- all.data
for(col in 2:(ncol(temp.data)-3)){
  temp.data[col] <- temp.data[col] / reference[1,col]
}
rm(col)
names(temp.data) <- gsub("ex", "ind", names(temp.data))
temp.data$year <- temp.data$month <- temp.data$day <- NULL
all.data <- merge(all.data, temp.data, by="date", all=T)
rm(temp.data)

write.csv(all.data, paste0(data_dir, "/cleaned_FX_WDS.csv"), row.names = F, na = "")

########################
# Sanity Check: Plot USD/MXN withe the other data frame
########################

data.sec <- read.csv(paste0(data_dir, "/cleaned_exrate_long.csv"), stringsAsFactors = F)
names(data.sec)[1] <- "complete_date"
data.sec$complete_date <- ymd_hms(data.sec$complete_date)
data.sec$date <- ymd(paste(year(data.sec$complete_date), 
                           month(data.sec$complete_date),
                           day(data.sec$complete_date), sep="-"))
data.sec <- subset(data.sec, hour(data.sec$complete_date)==16)
data.sec$complete_date <- NULL

plot <- ggplot() + 
        geom_line(data=data.sec, aes(x=date, y=indexed), col="red") + 
        geom_line(data=all.data, aes(x=date, y=indmxus), col="blue")

# Looks good! 
rm(plot)

########################
# Aggregate Indexes
########################

data.indexes <- read.table(paste0(data_dir, "/ExchangeRate/FX_Indexes.txt"), stringsAsFactors = F)
names(data.indexes) <- data.indexes[1,]
data.indexes <- data.indexes[2:nrow(data.indexes),]
data.indexes$day <- as.numeric(substr(data.indexes$date, 7,8))
data.indexes$month <- as.numeric(substr(data.indexes$date, 5,6))
data.indexes$year <- as.numeric(substr(data.indexes$date, 1,4))
data.indexes$date <- ymd(paste(data.indexes$year, data.indexes$month, data.indexes$day, sep="-"))
data.indexes[,2:5] <- suppressWarnings(lapply(data.indexes[,2:5], as.numeric) )# Some warnings, because of points indicating missing vals.

write.csv(data.indexes, paste0(data_dir, "/cleaned_FX_INDEXES.csv"), row.names = F, na = "")

########################
# Sanity Check: Plot USD/MXN VS Data Indexes
#   Other descriptve also here
########################

temp.data <- merge(data.sec, data.indexes, by="date", all.x=F, all.y=F)
temp.data <- temp.data[,c("date", "indexed", "twexb", "twexm", "twexo")]
names(temp.data)[3:5] <- paste("v", names(temp.data)[3:5], sep="_")
  
temp.data <- reshape(temp.data, direction="long", idvar = c("date", "indexed"), 
                     varying = names(temp.data)[3:5], sep="_")

temp.data$time[temp.data$time=="twexb"] <- "Broad"
temp.data$time[temp.data$time=="twexm"] <- "Major Currencies"
temp.data$time[temp.data$time=="twexo"] <- "Other Currencies"

plot <- ggplot(data=temp.data, aes(x=indexed*100, y=v, color=time)) + 
         geom_point() + 
         geom_smooth(method = "lm", formula = y ~ x + I(x^2)) + 
         scale_color_manual(values=c("#7fcdbb", "#1d91c0", "#0c2c84"), name="Index") + 
         xlab("Mexican Index (12-31-2013 = 100) \n (2012 - April 2018)") + ylab("Other indexes") + 
         ggtitle("Mexican exchange rate against other indexes") + 
         theme(axis.text.x = element_text(angle=90, size=rel(0.8), hjust=1), 
               panel.background = element_blank(), 
               panel.grid.major.x = element_line(colour = "#DBDBDB", size = rel(1)), 
               axis.line = element_line(colour = "black", size=rel(1.5)), 
               legend.position = "top", 
               legend.justification = "left")              
ggsave(paste(graph_dir, "IndexesVSusdmxn.png", sep="/"), plot= plot,  width = 12, height = 12)  
rm(plot)

temp.data <- subset(temp.data, year(temp.data$date) >= 2016) 

plot <- ggplot(data=temp.data, aes(x=indexed*100, y=v, color=time)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ x + I(x^2)) + 
  scale_color_manual(values=c("#7fcdbb", "#1d91c0", "#0c2c84"), name="Index") + 
  xlab("Mexican Index (12-31-2013 = 100) \n (2016 - April 2018)") + ylab("Other indexes") + 
  ggtitle("Mexican exchange rate against other indexes") + 
  theme(axis.text.x = element_text(angle=90, size=rel(0.8), hjust=1), 
        panel.background = element_blank(), 
        panel.grid.major.x = element_line(colour = "#DBDBDB", size = rel(1)), 
        axis.line = element_line(colour = "black", size=rel(1.5)), 
        legend.position = "top", 
        legend.justification = "left")              
ggsave(paste(graph_dir, "IndexesVSusdmxn_2.png", sep="/"), plot= plot,  width = 12, height = 12)  
rm(plot, temp.data)

temp.data <- all.data[,c(1, 27:ncol(all.data))]
names(temp.data)[names(temp.data)=="indmxus"] <- "mex"
names(temp.data)[names(temp.data)!="mex"] <- paste("v", names(temp.data)[names(temp.data)!="mex"], sep="_")
names(temp.data)[1] <- "date"

temp.data <- reshape(temp.data, direction="long", idvar = c("date", "mex"), sep="_", 
                     varying = names(temp.data)[substr(names(temp.data), 1,2)=="v_"], 
                     new.row.names=1:10000000)
temp.data$time[temp.data$time=="indalus"] <- "Australia"
temp.data$time[temp.data$time=="indbzus"] <- "Brazil"
temp.data$time[temp.data$time=="indcaus"] <- "Canada"
temp.data$time[temp.data$time=="indchus"] <- "China"
temp.data$time[temp.data$time=="inddnus"] <- "Denmark"
temp.data$time[temp.data$time=="indhkus"] <- "Hong Kong"
temp.data$time[temp.data$time=="indinus"] <- "India"
temp.data$time[temp.data$time=="indjpus"] <- "Japan"
temp.data$time[temp.data$time=="indkous"] <- "Korea"
temp.data$time[temp.data$time=="indmaus"] <- "Malaysia"
temp.data$time[temp.data$time=="indnzus"] <- "New Zealand	"
temp.data$time[temp.data$time=="indnous"] <- "Norway"
temp.data$time[temp.data$time=="indsdus"] <- "Sweden"
temp.data$time[temp.data$time=="indsfus"] <- "South Africa"
temp.data$time[temp.data$time=="indsius"] <- "Singapore"
temp.data$time[temp.data$time=="indslus"] <- "Sri Lanka"
temp.data$time[temp.data$time=="indszus"] <- "Switzerland"
temp.data$time[temp.data$time=="indtaus"] <- "Taiwan"
temp.data$time[temp.data$time=="indthus"] <- "Thailand"
temp.data$time[temp.data$time=="indukus"] <- "United Kingdom"
temp.data$time[temp.data$time=="indvzus"] <- "Venezuela"

# Latex table
for(x in unique(temp.data$time)){
  temp.data.2 <- subset(temp.data, temp.data$time==x)
  mycor <- cor(temp.data.2$mex, temp.data.2$v, use="pairwise.complete.obs", method="pearson")
  print(paste0(x, " & ", round(mycor, 4), " \\"))
  rm(mycor, temp.data.2)
}
rm(x)

plot <- ggplot(data=subset(temp.data, temp.data$time!="Venezuela"), aes(x=mex, y=v)) + 
  geom_point(color="#7fcdbb") +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), color="#0c2c84") + 
  facet_wrap(~time) + 
  xlab("Mexican Index (12-31-2013 = 100) \n (2010 - April 2018)") + ylab("Other currencies indexes") + 
  ggtitle("Mexican exchange rate against other currencies") + 
  theme(axis.text.x = element_text(angle=90, size=rel(0.8), hjust=1), 
        panel.background = element_blank(), 
        panel.grid.major.x = element_line(colour = "#DBDBDB", size = rel(1)), 
        axis.line = element_line(colour = "black", size=rel(1.5)), 
        legend.position = "top", 
        legend.justification = "left")  
ggsave(paste(graph_dir, "IndexesVSusdmxn_3.png", sep="/"), plot= plot,  width = 12, height = 12)  
rm(plot, temp.data)