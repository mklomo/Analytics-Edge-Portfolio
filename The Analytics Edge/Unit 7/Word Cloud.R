#Word clouds

library(tidyverse)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

#Lets load the data
Tweet_Data <-read_csv(file = "tweets.csv")

#Lets create a corpus
Tweet_Corpus <- Corpus(VectorSource(Tweet_Data$Tweet))

#Convert Corpus to lowercase
Tweet_Corpus <- tm_map(Tweet_Corpus, tolower)


#Lets remove punctuations
Tweet_Corpus <- tm_map(Tweet_Corpus, removePunctuation)

#Remove all english stopwords
Tweet_Corpus <- tm_map(Tweet_Corpus, removeWords, stopwords("english"))

#Lets build a document-term matrix
Tweet_Freq <- DocumentTermMatrix(Tweet_Corpus)

#New dataframe
all_Tweets <- as_tibble(as.matrix(Tweet_Freq))

#My first word cloud
wordcloud(colnames(all_Tweets), colSums(all_Tweets),
                                       scale = c(2, 0.25))


negative_tweets <- subset(all_Tweets, Tweet_Data$Avg <= -1)
wordcloud(colnames(negative_tweets), colSums(negative_tweets))

wordcloud(colnames(all_Tweets), colSums(all_Tweets),
          scale = c(2, 0.25), random.order =  FALSE)


wordcloud(colnames(all_Tweets), colSums(all_Tweets),
          colors=brewer.pal(9, "Blues"))











