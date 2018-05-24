#setting working directory
setwd("D:/PREP/R_study/projects/Twitter Sentiment Analysis")
getwd()
#Installing and loading required packages
# ROAuth Provides an interface to the OAuth 1.0 specification
#allowing users to authenticate via OAuth to the server of their choice.
install.packages("ROAuth")
#twitteR provides interface to twitter web api
install.packages("twitteR")
library(ROAuth)
library(twitteR)

#Storing Twitter credentials
#downloading caret file
download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")
#saving credentials in cred
cred<-OAuthFactory$new(consumerKey='Dfge9jJbSaRFiGYqe9FdTXl2y',
                       consumerSecret='300zbsPqBBTO4PmemXJuAwYs7qKIfkxlVeftLjhk3IFtrK6XMm',
                       requestURL='https://api.twitter.com/oauth/request_token',
                       accessURL='https://api.twitter.com/oauth/access_token',
                       authURL='https://api.twitter.com/oauth/authorize')

#initialize Authentication
cred$handshake(info="cacert.pem")

#saving credentials for later use
save(cred,file="twitter authentication.Rdata")

#loading authentication data
load("twitter authentication.Rdata")

#Authenticate
setup_twitter_oauth(consumer_key ='Dfge9jJbSaRFiGYqe9FdTXl2y',
                    consumer_secret = '300zbsPqBBTO4PmemXJuAwYs7qKIfkxlVeftLjhk3IFtrK6XMm',
                    access_token = '1579395499-EXhiWDTIxfZbajDd3q0B3brpfK0uofcwdommPIj',
                    access_secret = 'CcbRYOWRxcmNdzCzKyR0NNdtWEJFG4qXwRifbKD2c71mb')

#Fetching tweets
string<-"iniesta's transfer"
tweets<-searchTwitter(string,n=1000,lang = "en")
tweets.text <- sapply(tweets, function(x) x$getText())
head(tweets.text,10)


##Cleaning up the tweets

#-replace blank space
tweets.text<-gsub("RT", "",tweets.text)

#-remove usernames
tweets.text<-gsub("@\\w+", "",tweets.text)

#-removing punctuations
tweets.text<-gsub("[[:punct:]]", "",tweets.text)

#-removing links
tweets.text<-gsub("http\\w+", "",tweets.text)

#-removing tabs
tweets.text <- gsub("[ |\t]{2,}", "", tweets.text)

#-removing blank spaces at the begining
tweets.text<-gsub("^ ","",tweets.text)

#-reoving blank spacces at the end
tweets.text<-gsub(" $","",tweets.text)

#-removing smileys by iconv which is a function from the tm package
tweets.text <- iconv(tweets.text, "latin1", "ASCII", sub = "")

#-convert to lower cases
tweets.text<-tolower(tweets.text)

head(tweets.text)
##Removing the stop words
install.packages("tm")
library("tm")

#Create Corpus
tweets.text.corpus<-Corpus(VectorSource(tweets.text))

#remove stop words
tweets.text.corpus<-tm_map(tweets.text.corpus,function(x) removeWords(x,stopwords()))

##creating word cloud
install.packages("wordcloud")
library("wordcloud")

wordcloud(tweets.text.corpus,min.freq = 3,scale=c(3,0.5),colors = brewer.pal(8,"Dark2"),random.color = TRUE,random.order = FALSE,max.words = 150)


##Loading packages for sentimentanalysis and plotting
install.packages("syuzhet")

library(syuzhet)

#Sentiment score for different sentiments

emotions<-get_nrc_sentiment(tweets.text)
#Segregating into positive, negative and neutral tweets    
val<-get_sentiment(tweets.text)
positive_tweets<-tweets.text[val>0]
negative_tweets<-tweets.text[val<0]
neutral_tweets<-tweets.text[val=0]
category_senti <- ifelse(val < 0, "Negative", ifelse(val > 0, "Positive", "Neutral"))
plot_data<-table(category_senti)
plot_data
#Saving Data into csv file 
write.csv(plot_data,file = "Iniesta_retirement.csv",row.names = F)

##Plots where developed using Tableau Software
