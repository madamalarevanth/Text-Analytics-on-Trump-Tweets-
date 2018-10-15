library(tidyverse) 
library(ggplot2)                        
library(data.table)                    
library(tm)                             
library(textreg)                      
library(magrittr)
library(Cairo)
library(lubridate)
library(ggplot2)
library("wordcloud")

#importing data from csv file 
raw_data <- read.csv("C:/Users/REVANTH/Desktop/Data/Donald-Tweets!.csv")

#creating new columns for each hours ,day and time exclusively
raw_data$HMS <- hms(as.character(raw_data$Time))
raw_data$YMD <- ymd(as.character(raw_data$Date))
raw_data$time <- hour(raw_data$HMS)
raw_data$day <-  day(raw_data$YMD)
raw_data$month <- month(raw_data$YMD)

#getting the tweet frequency
ggplot(raw_data,aes(x=time))+geom_histogram(aes(y = (..count..)),binwidth=1)
ggplot(raw_data,aes(x=month))+geom_histogram(aes(y = (..count..)),binwidth=1)
ggplot(raw_data,aes(x=day))+geom_histogram(aes(y = (..count..)),binwidth=1)


##sentiment Analysis 

#function to clean comments 

cleaned_corpus<- function(corpus)
{
  # to remove all punctuation 
   corpus<- tm_map(corpus, removePunctuation) 
   
  #to remove all non alpha numeric characters
  #content transformer creates a function used to modify the r object
   
   corpus <- tm_map(corpus, content_transformer(function(x) gsub(pattern = "[^a-zA-Z0-9]", replacement = " ", x = x))) 
   
   # to convert all the corpus to lowercase
   corpus <- tm_map(corpus, content_transformer(tolower))   
   
   # removes numbers from corpus
   corpus <- tm_map(corpus, removeNumbers)
   
   # removes common words like the, and, a etc.
   corpus <- tm_map(corpus, removeWords, c(stopwords(kind = "en")))   
   
   # removes extra white spaces
   corpus <- tm_map(corpus, stripWhitespace) 
  
  return(corpus)
   
}

comments <- raw_data %>% select(Tweet_Text)

#creating a corpus of comments from raw data 
comments_corpus <- VCorpus(VectorSource(comments))

#cleaning the comments using above created function 
cleaned_comments <- cleaned_corpus(comments_corpus)

#converting the corpus back to corpus
new_comments <- convert.tm.to.character(cleaned_comments)

#creating a term document matrix
textdoc <- TermDocumentMatrix(cleaned_comments)

inspect(textdoc)

freq<- sort(rowSums(as.matrix(textdoc)),decreasing = TRUE)
head(freq,100)

findFreqTerms(textdoc,lowfreq = 100)

word_frame <- data.frame(words=names(freq),freq=freq)
head(word_frame)

#to plot the freq of the words 
subset(word_frame,freq>200) %>% ggplot(aes(words,freq))+geom_bar(stat = "identity",color=" yellow")+coord_flip()

#to find the most frequently used words 
barplot(word_frame[1:15,]$freq,las = 3,names.arg = word_frame[1:15,]$words,col="orange",main = "most frequent words",ylab = "word frequency")

#forming a wordcloud to better visualize the data 
wordcloud(words = word_frame$words,freq = word_frame$freq,min.freq = 25,max.words = 200,random.order = FALSE,rot.per = 0.45,colors = brewer.pal(8,"Dark2"))
