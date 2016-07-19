#install the necessary packages
install.packages("twitteR")
install.packages("wordcloud")
install.packages("tm")
install.packages("SnowballC")
install.packages("RCurl")


#Load necessary libraries
library("twitteR")
library("wordcloud")
library("tm")
library("SnowballC")
library("RCurl")

#Authentication Information
consumer_key <- '##########'
consumer_secret <- '##########'
access_token <- '##########'
access_secret <- '##########'
setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token,
                    access_secret)

#Set the Hashtag to search twitter for
Deloitte <- searchTwitter("##########")
length(Deloitte)
#save as text
Deloitte_text <- sapply(Deloitte, function(x) x$getText())
#Create the corpus
Deloitte_text_corpus <- Corpus(VectorSource(Deloitte_text))

#run this step if you get the error:
#(please break it!)' in 'utf8towcs'
Deloitte_text_corpus <- tm_map(Deloitte_text_corpus,
                              content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')),
                              mc.cores=1)
Deloitte_text_corpus <- tm_map(Deloitte_text_corpus, content_transformer(tolower), mc.cores=1)
Deloitte_text_corpus <- tm_map(Deloitte_text_corpus, removePunctuation, mc.cores=1)
Deloitte_text_corpus <- tm_map(Deloitte_text_corpus, function(x)removeWords(x,stopwords("english")), mc.cores=1)
Deloitte_text_corpus <- tm_map(Deloitte_text_corpus, stripWhitespace)
Deloitte_text_corpus <- tm_map(Deloitte_text_corpus, removeNumbers)
Deloitte_text_corpus <- tm_map(Deloitte_text_corpus, removeWords, c("deloitte", "https"))

wordcloud(Deloitte_text_corpus)

