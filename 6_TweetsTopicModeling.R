rm(list=ls())

########################
# Eduardo Fierro
# Topic modeling on tweets
# Causal Inference
# 20/4/2018
########################

########################
# Setup
########################

library(tidyverse)
library(lubridate)
library(quanteda)
library(topicmodels)

data_dir <- "/Users/eduardofierro/Google Drive/CuartoSemestre/Causal Inference/Project/Data"
graph_dir <- "/Users/eduardofierro/Google Drive/CuartoSemestre/Causal Inference/Project/Graphs"

########################
# Import data 
########################

trump.tweets <- read.csv(paste0(data_dir, "/cleaned_tweets.csv"), stringsAsFactors = F)
trump.tweets$id_str <- as.character(trump.tweets$id_str)

#Subset only for the tweets starting november 2016 (when predictwise data starts):
trump.tweets$complete_date <- ymd_hms(trump.tweets$complete_date)
trump.tweets <- subset(trump.tweets, trump.tweets$complete_date >= ymd("2016-01-01"))

# Remove URLs

trump.tweets$text <- gsub("\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)", "", trump.tweets$text)
# Regex from https://stackoverflow.com/questions/42460803/regex-in-r-remove-multiple-urls-from-string
#trump.tweets$text <- gsub("^(t.co)*", "", trump.tweets$text, perl=T)
#trump.tweets$text <- gsub("^(http)*", "", trump.tweets$text, perl=T)

stoplist <- c(stopwords("english"))

trump.tweets.corpus <- dfm(trump.tweets$text, remove=stoplist, tolower=TRUE, remove_punct = TRUE)
rowTotals <- apply(trump.tweets.corpus , 1, sum)
trump.tweets$rowTotals <- rowTotals

# Losing 5 tweets, probably because of stopwords & url-only tweets. It only contains "WE". id_str == 863446552559398912:
trump.tweets.corpus <- subset(trump.tweets.corpus, rowTotals>0)
trump.tweets <- subset(trump.tweets, rowTotals>0)

########################
# Topic Model 
########################

### k=10 topis
lda_model_k10 <- LDA(trump.tweets.corpus, k = 10, method = "Gibbs",  control = list(seed = 10012))
distribution_k10 <- lda_model_k10@gamma
distribution_k10 <- data.frame(distribution_k10)

top10_terms <- get_terms(lda_model_k10, 10)
print(top10_terms)

### INTRESTING!!!
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

## PLOT DENSITY: 
data.plot <- distribution_k10
names(data.plot) <- paste("Dist", 1:10, sep="_")
data.plot$cons <- 1:nrow(data.plot)
data.plot <- reshape(data=data.plot, direction="long", idvar = "cons", varying = names(data.plot)[1:ncol(data.plot)-1], sep="_")
names(data.plot)[2] <- "Topic"
data.plot$Topic_f <-factor(data.plot$Topic, labels=paste("Topic", 1:10))

plot <- ggplot(data=data.plot, aes(Dist)) + 
        geom_density(color="#253494", fill="#7fcdbb") + 
        facet_wrap(~Topic_f) +
        xlab("") + ylab("Density") + 
        ggtitle("Density per topic distribution") + 
        theme(axis.text.x = element_text(angle=90, size=rel(0.8), hjust=1), 
              panel.background = element_blank(), 
              panel.grid.major.x = element_line(colour = "#DBDBDB", size = rel(1)), 
              axis.line = element_line(colour = "black", size=rel(1.5)), 
              legend.position = "top", 
              legend.justification = "left")     
ggsave(paste(graph_dir, "TweetsTopicDist.png", sep="/"), plot= plot,  width = 12, height = 12)  

rm(plot, data.plot)
### k=5 topis
lda_model_k5 <- LDA(trump.tweets.corpus, k = 5, method = "Gibbs",  control = list(seed = 10012))
distribution_k5 <- lda_model_k10@gamma
distribution_k5 <- data.frame(distribution_k5)

top10_terms <- get_terms(lda_model_k5, 10)
print(top10_terms)

### INTRESTING!!!
# Topic 1: Tax and "the people"
# Topic 2: Crooked Hillary
# Topic 3: Fake news and campaign
# Topic 4: Jobs. 
# Topic 5: Campaing tweets.

########################
# Save results 
########################

rm(top10_terms, stoplist, graph_dir)
save.image(paste0(data_dir, "/TopicModelOuts.RData"))