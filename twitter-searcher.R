library(twitteR)
library(dplyr)
library(ggplot2)
source("twitter-auth.r")

# Function to split search request into daily chunks, to circumvent Twitter API rate limits
twitterSearch<-function(start=as.character(Sys.Date()-7), end=as.character(Sys.Date()), searchTerm) {
  startDate<-as.Date(start)
  endDate<-as.Date(end)
  numDays<-endDate-startDate
  currentDay<-startDate
  fullResults<-NULL
  for ( day in 1:numDays ) {
    dayResults<-searchTwitter(searchTerm, since=as.character(currentDay), until=as.character(currentDay+1), n=9999)
    dayResults.df<-NULL
    for ( tweet in 1:length(dayResults) ) {
      tweetText<-dayResults[[tweet]]$text # get the tweet text
      tweetDate<-dayResults[[tweet]]$created # get the tweet created date
      loop.df<-data.frame(tweet=character(1), date=as.Date("2015-01-01")) # initialise empty df
      loop.df$tweet<-as.character(loop.df$tweet)
      loop.df[1,1]<-tweetText
      loop.df[1,2]<-tweetDate
      loop.df$count<-1
      dayResults.df<-rbind(dayResults.df, loop.df)
    }
    currentDay<-currentDay+1
    fullResults<-rbind(fullResults, dayResults.df)
  }
  fullResults
}

mysearch<-twitterSearch(searchTerm="jaguar")

# summarise the tweets by day and sum the tweet count for each day
fullResults.bydate<-mysearch %>% group_by(date) %>% summarise (totalTweets = sum(count))

# quick plot of daily tweets matching the search
ggplot(fullResults.bydate, aes(x=date, y=totalTweets)) + geom_line()