
##library------------------------

#load the library

library(wordcloud)
library(mgsub)
library(stopwords)
library(ggplot2)
library(tm)
library(stringr)
library(qdap)
library(lubridate)
library(caret)
library(data.table)
library(readxl)
#-----

#Read dataset
setwd("C:/Users/mktmi/Desktop/huawei_reviews")
#不赋权数据集
#ba.data <- read.csv("p10.csv",header=TRUE)

#赋权数据集
ba.data = read_excel("right_P20.xlsx",sheet = 1)
ba.data <- data.table(ba.data)

#delete repeat reviews
index <- duplicated(ba.data$text)
ba.data <- ba.data[!index,]

#subset time
ba.data$created_at <- as.POSIXct (ba.data$created_at,format="%Y-%m-%d")
ba.data12<-subset(ba.data,created_at>=as.POSIXct('2018-05-01')
                  &created_at<=as.POSIXct('2018-06-30'))


#Order according to time
ba.data12.o <- ba.data12[order(ba.data12$created_at),]

#check data to see if there are missing values
length(which(!complete.cases(ba.data12.o$text)))

#have a look of top 10
head(ba.data12.o,n=10)
#tailor-made a few things

#A function changes all to lower case (and return NA stead of error if it is a special character)
#Return NA instead of tolower error
tryTolower <-function(x){
  #return NA when there is an error
  y=NA
  #tryCatch error
  try_error=tryCatch(tolower(x),error=function(e) e)
  #if not an error 
  if (!inherits(try_error, 'error'))
    y=tolower(x)
  return(y)
}

# create a pre-processing function using gsub 

clean.gsub<-function(corpus){
  corpus$text <-gsub("@\\S*", "", corpus$text) 
  corpus$text <-gsub("https\\S*", "", corpus$text)
  corpus$text <- gsub("[’]" , "",corpus$text)
  corpus$text <- gsub("[,]" , "",corpus$text)
  corpus$text <- gsub("[[:punct:]]" , "",corpus$text)
  return(corpus)
}
ba.data12.o <- clean.gsub(ba.data12.o)
#write.table(ba.data12.o,"gsub_P30.csv",row.names=FALSE,col.names=TRUE,sep=",")

#create my stop words list
custom.stopwords<-c(stopwords('english'),'the', 'and', 'with', 'phone', 'you', 'this', 
                    'for', 'have', 'that', 'but', 'are', 'not',
                    'which', 'can', 'has', 'was', 'all', 'from', 'get', 'great', 'its', 'some', 'when', 
                    'one', 'there', 'also', 'just', 'more', 'will', 'about', 'had', 'than', 'want', 'after',
                    'take', 'they', 'your', 'much', 'out', 'what', 'would', 'day', 'too', 'does',
                    'use', 'phone', 'work', 'really', 'life', 'still', 'lite', 'got', 'need', 'feel',
                    'say', 'even', 'set', 'amazon', 'year', 'think', 'buy', 'user', 'bought', 'seem',
                    'used', 'uses', 'thing', 'things', 'works', 'worked', 'sets', 'using', 'pro', '/', ','
                    )

#create a pre-processing function using tm functions and the above two
clean.corpus<-function(corpus){
  corpus<-tm_map(corpus,content_transformer(tryTolower))
  corpus<-tm_map(corpus,removeWords,custom.stopwords)
  corpus<-tm_map(corpus,removePunctuation)
  corpus<-tm_map(corpus,stripWhitespace)
  corpus<-tm_map(corpus,removeNumbers)
  corpus<-tm_map(corpus,stemDocument, language = "english")
  return(corpus)
}


#define the tweets object
#when measuring va using the following codes
# the.corpus <- VCorpus(VectorSource(va.data12.o$text))
the.corpus <- Corpus(VectorSource(ba.data12.o$text))

#clean the tweets with the function created earlier
the.corpus<-clean.gsub(the.corpus)
the.corpus<-clean.corpus(the.corpus)

the.corpus<-tm_map(the.corpus,removePunctuation)
the.corpus<-tm_map(the.corpus,removeWords,c('flight'))

the.corpus <- Corpus(VectorSource(the.corpus))
head(the.corpus)
#Create the term document matrix
tdm <- DocumentTermMatrix(the.corpus,control=list(weighting=weightTf))

#remove sparse terms from a doucment if the sparsity is more than 99%
tdm.n<-removeSparseTerms(tdm, 0.99)

#redefine it as matrix for easy to computation
tdm.tweets<-as.matrix(tdm.n)

#save the pre-processed document term matrix
saveRDS(tdm.tweets, file="matrix.tweets")

#check dimension of the tweets
dim(tdm.tweets)

#check term frequency
term.freq<-colSums(tdm.tweets)

#create a dataframe with the term and then the frequency as the second column
freq.df<-data.frame(word=names(term.freq),frequency=term.freq)
freq.df<-freq.df[order(freq.df[,2],decreasing=T),]
freq.df[1:100,]

write.table(freq.df[1:100,],"freq_P20_导入.csv",row.names=FALSE,col.names=TRUE,sep=",")

#Plot word frequencies when frequency is higher than 150
hp <- ggplot(subset(freq.df, term.freq>150), aes(word, frequency))    
hp <- hp + geom_bar(stat="identity")   
hp <- hp + theme(axis.text.x=element_text(angle=45, hjust=1))   
hp   


