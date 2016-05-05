library(twitteR)
library(dplyr)
library(ggplot2)
source("twitter-auth.r")

# Function to split search request into daily chunks, to circumvent Twitter API rate limits
twitterSearch<-function(startDate, endDate, searchTerm) {
  startDate<-as.Date(startDate)
  endDate<-as.Date(endDate)
  numDays<-endDate-startDate
  currentDay<-startDate
  fullResults<-NULL
  for ( day in 1:numDays ) {
    dayResults<-searchTwitter(searchTerm, since=as.character(currentDay), until=as.character(currentDay+1))
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

# Run the twitter search for the #likeagirl hashtag. You can use + to separate query terms.
always<-searchTwitter("#likeagirl", n=1588, since='2016-04-01')

# Join the results from the "always" list into a data frame
always.df<-NULL
for ( tweet in 1:length(always)) {
  tweetText<-always[[tweet]]$text # get the tweet text
  tweetDate<-always[[tweet]]$created # get the tweet created date
  loop.df<-data.frame(tweet=character(1), date=as.Date("2015-01-01")) # initialise empty df
  loop.df$tweet<-as.character(loop.df$tweet)
  loop.df[1,1]<-tweetText
  loop.df[1,2]<-tweetDate
  loop.df$count<-1
  always.df<-rbind(always.df, loop.df)
}

rm(list=c("loop.df"))

# summarise the tweets by day and sum the tweet count for each day
always.df2<-always.df %>% group_by(date) %>% summarise (totalTweets = sum(count))

# quick plot of daily tweets matching the search
ggplot(always.df2, aes(x=date, y=totalTweets)) + geom_line()