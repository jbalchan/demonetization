library(ggplot2)
library(plyr)
#,col.names = optnames
#"%a, %d %b %Y %H:%M:%S %z",
dt <- 10 #10 mins

#optnames <- c("sno","text","favtd" ,"favcount","replysn", "date","trunc","replysid","id","replyuid","statussrc","screenname","rtcount", "isrt","rtd")
tweetsjsr <- read.csv("D:/1 DATA BYTE/1_projects/demonetization proj/from shivani/demonetization-tweets.csv")
#tweets <- unique(read.table("D:/1 DATA BYTE/1_projects/demonetization proj/from shivani/demonetization-tweets.csv", sep="\t", quote="", comment.char="",stringsAsFactors=FALSE, header=TRUE, nrows=8000))
tweets <- unique(tweetsjsr)
names(tweets) <- c("sno","text","favtd" ,"favcount","replysn", "date","trunc","replysid","id","replyuid","statussrc","screenname","rtcount", "isrt","rtd")

#names(tweets) <- c("id", "date", "user", "text")
#posixformt <- strptime(tweets$date,format = "%Y-%m-%d %H:%M")
#tweets$date <- as.POSIXct(strptime(tweets$date,  tz = "GMT", format = "%Y-%m-%d %H:%M:%S"))
tweets$date <- as.POSIXct(tweets$date,  tz = "GMT", format = "%d-%m-%Y %H:%M")

#cutoff later if necessary
minDate <- min(tweets$date)
maxDate <- max(tweets$date) + 60 * dt
dateBreaks <- seq(minDate, maxDate, by=60 * dt)

tweetCount <- hist(tweets$date, breaks=dateBreaks, plot=FALSE)
binBreaks <- tweetCount$breaks[1:length(tweetCount$breaks)-1]
userCount <- sapply(binBreaks, function(d) length(unique(tweets$user[which((tweets$date >= d) & (tweets$date <= d + 60*dt))]))) 
plotData <- data.frame(dates=dateBreaks[1:length(dateBreaks)-1], tweets=as.numeric(tweetCount$count), users=as.numeric(userCount))
ggplot(plotData) + 
  geom_bar(aes(x=dates, y=tweets, color=users), stat="identity") +
  scale_x_datetime("Date") +
  scale_y_continuous("Number of tweets") +
  labs(title = "Number of tweets and unique users : #demonetization")
  #opts(title="Number of tweets and unique users : #demonetization")
