# DATA 630 Group 2 Project
# Written by Group 2
# Last Updated June 27, 2021

# NOTES FOR TEAMMATES
# Action items = AI: - search for the text AI: for guidance
# This script demonstrates ..... 

###############################################
# Load Libraries
###############################################
# AI:Mike Only: paste this code in the library for your needs: , lib.loc="C:/R/packages")
library("NLP")
library("tm")
library("ggplot2")
library("wordcloud")
library("biclust")
library("cluster")
library("igraph")
library("fpc")
library("RColorBrewer")

# AI:Mike Only:needed for word stemming but unable to install *****************************************
#library("SnowballCC")
library(SnowballC) # JM added 6/27/2021 - only a single C. packages load with or without quotes. not sure why
###############################################
# Load DATA
###############################################
# Set the working directory
# AI:Mike Only: setwd("C:/DATA630/Group 2 Project")
setwd("Z:\\data\\Learn\\UMGC\\DATA630\\Project") # JM Only 
# dir() # confirm the working directory is set correctly

# Use the read.csv function to read the HotelReviews.csv into a data frame
reviews<-read.csv(file="HotelReviews.csv", head=TRUE, sep=",", as.is=FALSE)
# Data Sampling - the matrix is expensive and we should look at releasing the ram when it is not needed
# AI: Mike Only:reviews<- reviews[0:10000, ] # memory limitation
###############################################
# Initial EDA
###############################################
# Review some basics about the data
class(reviews)    # confirm we are working with a data.frame object
dim(reviews)      # display number of rows and variables
head(reviews)     # display the first six rows
summary(reviews)  # display descriptive statistics
str(reviews)      # display dataframe structure BEFORE data cleaning (35912 obs. of  13 variables)
# View(reviews)     # view data 
###############################################
# Data Cleaning
###############################################
# Remove unneeded columns
reviews$address<-NULL
reviews$country<-NULL
reviews$reviews.doRecommend<-NULL
reviews$reviews.id<-NULL
reviews$reviews.userCity<-NULL
reviews$reviews.username<-NULL
# reviews$reviews.userProvince <-NULL # ********************* CONSIDER, TIME REMAINING

# View(reviews)     # view data after removing columns

# write.csv(reviews,"reviewsv1.csv")
###############################################
# Initial Text mining
###############################################
TextDoc <- Corpus(VectorSource(reviews$reviews.text))

# AI: Teammates create subfolder called Corpus before running scripts.
# SAVE CORPUS TO TEXT FILES - place in subfolder - T
# writeCorpus(TextDoc, path = ".\\Corpus", filenames = NULL)

#Replacing "/", "@" and "|" with space
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
TextDoc <- tm_map(TextDoc, toSpace, "/")
TextDoc <- tm_map(TextDoc, toSpace, "@")
TextDoc <- tm_map(TextDoc, toSpace, "\\|")

# Convert the text to lower case
TextDoc <- tm_map(TextDoc, content_transformer(tolower))

# Remove numbers
TextDoc <- tm_map(TextDoc, removeNumbers)

# Remove english common stopwords
TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))

# Remove your own stop word
# specify your custom stopwords as a character vector
TextDoc <- tm_map(TextDoc, removeWords, c("ett", "s", "company", "team"))

# Remove punctuations
TextDoc <- tm_map(TextDoc, removePunctuation)

# Eliminate extra white spaces
TextDoc <- tm_map(TextDoc, stripWhitespace)

# TEXT STEMMING - CODE NOT WORKING - NEED TO TROUBLESHOOT
# Text stemming - which reduces words to their root form
TextDoc <- tm_map(TextDoc, stemDocument)

# View(TextDoc)

# Build a term-document matrix
TextDoc_dtm <- TermDocumentMatrix(TextDoc)
dtm_m <- as.matrix(TextDoc_dtm)
# Sort by decreasing value of frequency
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
# Display the top 5 most frequent words
head(dtm_d, 5)

myWords <- c("ok", "chat", 'okay', 'day', 'today', "might", "bye", "hello", "thank", "you", "please", "sorry", "hello", "hi")
cleantext <- function(corpus){
  clean_corpus <- tm_map(corpus, removeNumbers)
  clean_corpus <- tm_map(clean_corpus, content_transformer(tolower)) #!! modified
  #clean_corpus <- tm_map(clean_corpus, PlainTextDocument) ### !!!! PlainTextDocument function erases metadata from corpus = document id! So this needs to be erased
  #clean_corpus <- tm_map(clean_corpus, replacePunctuation)
  clean_corpus <- tm_map(clean_corpus, removePunctuation)
  clean_corpus <- tm_map(clean_corpus, removeWords, c(stopwords("english"), myWords))
  clean_corpus <- tm_map(clean_corpus, stripWhitespace)
  clean_corpus <- tm_map(clean_corpus, stemDocument, language = "english")
  
  clean_corpus
}
