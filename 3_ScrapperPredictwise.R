rm(list=ls())

library('RSelenium')
library('XML')
library('plyr')
library(stringr)
library(lubridate)
library(ggplot2)

graph_dir <- "/Users/eduardofierro/Google Drive/CuartoSemestre/Causal Inference/Project/Graphs"
out_dir <- "/Users/eduardofierro/Google Drive/CuartoSemestre/Causal Inference/Project/Data"

# First having to download doccker: 
# https://docs.docker.com/docker-for-mac/install/#install-and-run-docker-for-mac

# And configure and image: 
# https://docs.docker.com/docker-for-mac/#explore-the-application
# https://stackoverflow.com/questions/45395849/cant-execute-rsdriver-connection-refused?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa
# https://cran.r-project.org/web/packages/RSelenium/vignettes/RSelenium-docker.html 

################## 
# Set conenction and Daemon
################## 

url <- "https://predictwise.com/politics/2016-president-winner"

# docker run -d -p 4445:4444 selenium/standalone-firefox :: browserName = "firefox"
# docker run -d -p 4445:4444 selenium/standalone-chrome :: browserName = "chrome"
remDr <- remoteDriver(remoteServerAddr = "localhost", 
                      port = 4445L,
                      browserName = "chrome")
remDr$open()
remDr$navigate(url)

################## 
# Scrape
################## 
 
htmlGetVals <- function(dirty){
  
  # arg :: dirty = webElem$getElementAttribute("outerHTML")
  
  as_html <- htmlParse(dirty[[1]])
  val <- xpathSApply(as_html, "//*/div[@id = 'tooltip']", xmlValue)
  return(val)
  
}

webElem <- remDr$findElement(using = 'class', 'chart-wrapper')
remDr$mouseMoveToLocation(webElement = webElem)
elemtxt <- webElem$getElementAttribute("outerHTML") # Centers.... 
main.data <- data.frame(x=0, val=htmlGetVals(elemtxt))

for(offsets in 1:500){
  
  webElem$mouseMoveToLocation(x=1, y=0)
  elemtxt <- webElem$getElementAttribute("outerHTML") 
  temp.data <- data.frame(x=offsets, val=htmlGetVals(elemtxt))
  main.data <- rbind.fill(main.data, temp.data)
  rm(temp.data, elemtxt)
  
}
                            
webElem$mouseMoveToLocation(webElement = webElem) # Re-center, go left.

for(offsets in 1:500){
  
  webElem$mouseMoveToLocation(x=-1, y=0)
  elemtxt <- webElem$getElementAttribute("outerHTML") 
  temp.data <- data.frame(x=offsets, val=htmlGetVals(elemtxt))
  main.data <- rbind.fill(main.data, temp.data)
  rm(temp.data, elemtxt)
  
}

main.data <- subset(main.data, main.data$val!="")

################## 
# Clean Data
################## 

exact_date <- str_extract(main.data$val, "\\([^()]+\\)")
exact_date <- gsub("[(]", "", exact_date)
exact_date <- gsub("[)]", "", exact_date)
exact_date_temp <- str_split(exact_date, "-")
exact_date_temp <- do.call(rbind, exact_date_temp)

main.data$month <- as.numeric(exact_date_temp[,1])
main.data$day <- as.numeric(exact_date_temp[,2])

exact_date_temp2 <- str_split(exact_date_temp[,3], " ")
exact_date_temp2 <- do.call(rbind, exact_date_temp2)

main.data$year <- as.numeric(exact_date_temp2[,1])

exact_time <- exact_date_temp2[,2]

rm(exact_date_temp2, exact_date_temp)

is_am <- str_detect(exact_time, "AM")
exact_time_temp <- str_split(exact_time, ":")
exact_time_temp <- do.call(rbind, exact_time_temp)

main.data$hour <- ifelse(is_am==T, as.numeric(exact_time_temp[,1]), as.numeric(exact_time_temp[,1]) + 12)
main.data$hour[main.data$hour==24] <- 0
minutes <- exact_time_temp[,2]
minutes <- gsub("AM", "", minutes)
minutes <- gsub("PM", "", minutes)

main.data$minutes <- as.numeric(minutes)

rm(minutes, is_am, exact_time_temp, exact_date, exact_time)

main.data$complete_date <- ymd_hm(paste0(main.data$year,"-", main.data$month,"-",main.data$day, " ",
                                          main.data$hour, ":", main.data$minutes))

extract_val <- str_split(main.data$val, " ")
extract_val <- do.call(rbind, extract_val)


main.data$percent <- as.numeric(gsub("%", "", extract_val[,2]))

write.csv(main.data, paste0(out_dir, "/cleaned_predictwise.csv"), row.names = F, na = "")

################## 
# Graph for sanity check
################## 

plot <- ggplot(data=main.data, aes(x=complete_date, y=percent)) + 
  geom_line(color="#0c2c84") + 
  geom_point(color="#0c2c84") + 
  theme_bw() + 
  labs(y="Percent", x="", title="Trump's probability of winning \n Source: Predictwise") 
ggsave(paste(graph_dir, "PredictwiseDescriptive1.png", sep="/"), plot= plot,  width = 12, height = 12)  

