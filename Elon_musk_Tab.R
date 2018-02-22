rm(list = ls())
setwd()
install.packages("igraph")
install.packages("ggraph")
library(rtweet)
# plotting and pipes - tidyverse!
library(ggplot2)
library(dplyr)
# text mining library
library(tidytext)
# plotting packages
library(igraph)
library(ggraph)
library(data.table)
tweets <- fread("Elonmusk.csv")
head(tweets)
#Cleaning the text by removing https.* with blank spaces
tweets$stripped_text <- gsub("http.*","",  tweets$Tweet)
tweets$stripped_text <- gsub("https.*","", tweets$stripped_text)
#Cleaning text using dyplyr and storing : remove punctuation, convert to lowercase, add id for each tweet!
tweets_clean <- tweets%>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(word, stripped_text)
head(tweets_clean)
c(nrow(tweets), nrow(tweets_clean))
#plotting the top 15 words but it ha generic words like the to etc.
tweets_clean %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in tweets")
#Utilizing package tidytext
data("stop_words")
head(stop_words)
nrow(tweets_clean)
#removing the stop words
tweet_words <- tweets_clean %>%
  anti_join(stop_words)
#plotting the top 15 unique words
tweet_words %>%
  count(word, sort = TRUE) %>%
  top_n(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in tweets")
install.packages("igraph")
library("igraph")

# Finding top mentions 
users <- function(x){
  xx <- strsplit(x, " ")
  lapply(xx, function(xx)xx[grepl("@[[:alnum:]]", xx)])
}
mentions <- users(tweets$stripped_text)
df<-data.frame(mentions = unlist(mentions))  # to rectify error
write.csv(df, "mentions_elon.csv")

#Finding top hashtags

users <- function(y){
  yy <- strsplit(y, " ")
  lapply(yy, function(yy)yy[grepl("#[[:alnum:]]", yy)])
}
hashtag <- users(tweets$stripped_text)
dy<-data.frame(hashtag = unlist(hashtag))  # to rectify error
write.csv(dy, "hashtag_elon.csv")

