library(twitteR)
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidytext)
library(data.table)
library(tm)
library(plotly)

# the following keys are user specific 
consumer_key <- "XYZ1" 
consumer_secret <- "XYZ2"
access_token <- "XYZ3"
access_secret <- "XYZ4"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

fn_twitter <- searchTwitter("$spy",n=10^6,lang="en")
fn_twitter_df <- twListToDF(fn_twitter) # Convert to data frame

ds <- data.table(fn_twitter_df)
ds$date <- date(ds$created)
median(ds[,.N,by =list(date)]$N)

median(ds)


tweet_words <- ds %>% select(id, text,date) %>% unnest_tokens(word,text)

tm_string <- tweet_words$word
tm_string <- removePunctuation(tm_string)
tm_string  <- removeWords(tm_string, c(stopwords("english"),"http","https") )
tm_string <- tm_string[tm_string != ""]
tweet_words <- tweet_words[tweet_words$word %in% tm_string,]

p <- tweet_words %>% count(word,sort=T) %>% slice(1:20) %>% 
  ggplot(aes(x = reorder(word, 
                         n, function(n) -n), y = n)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 60, 
        
                                                                                                                                                                                                                                    hjust = 1)) + xlab("")
words_list <- strsplit(ds$text," ")
words_list <- lapply(words_list,removePunctuation)
words_list <- lapply(words_list,function(x)   removeWords(x, c(stopwords("english"),"http","https")  ) )
words_list <- lapply(words_list,tolower)

dip_index <- sapply(words_list, function(x)  sum(x %like% "dip") )
sum(dip_index)

ds_dip <- ds[dip_index == 1,]

tweet_words <- ds_dip %>% select(id, text,date) %>% unnest_tokens(word,text)

tm_string <- tweet_words$word
tm_string <- removePunctuation(tm_string)
tm_string  <- removeWords(tm_string, c(stopwords("english"),"http","https") )
tm_string <- tm_string[tm_string != ""]
tweet_words <- tweet_words[tweet_words$word %in% tm_string,]

p <- tweet_words %>% count(word,sort=T) %>% slice(1:20) %>% 
  ggplot(aes(x = reorder(word, n, function(n) -n), y = n)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 60,                                                                                                                       hjust = 1)) + xlab("")
p

# tie it with BTC
library(alphavantager)
av_api_key("XYZ5")
DS <- av_get(symbol = "SPY", av_fun = "TIME_SERIES_INTRADAY", interval = "15min", outputsize = "full")
dim(DS)
DS <- data.table(DS)
DS$date <- date(DS$timestamp)
DS <- DS[DS$date >= min(ds_dip$date),]

# keep the tweets related to the dip
ds_dip <- ds[dip_index == 1,]
ds_dip$created <- ds_dip$created - hours(4)
ds_dip$timestamp <- ceiling_date(ds_dip$created,"15min")
ds_sum <- ds_dip[,.N,by =list(timestamp)]
ds_sum <- ds_sum[order(ds_sum$timestamp),]
ds_sum <- ds_sum[ds_sum$timestamp >=  min(DS$timestamp) ]
ds_sum <- ds_sum[ds_sum$timestamp <=  max(DS$timestamp) ]

ds.plot <- merge(DS,ds_sum,by = "timestamp",all = T)
ds.plot$N[is.na(ds.plot$N)] <- 0
ds.plot[,N_cum := cumsum(N), by = list(date) ]


{
  ds.plot.i <- ds.plot
  
  # choose the labels format
  f <- list(
    family = "Courier New, monospace",
    size = 18,
    color = "#7f7f7f"
  )
  
  # x-axis 
  xl <- list(
    title = "Time",
    titlefont = f
  )
  
  # y-axis for Tweets
  yl <- list(
    title = "# of Tweets",
    titlefont = f
  )
  
  # right hand y-axis AMC price
  yl2 <- list(title = "SPY",
              overlaying = "y",
              titlefont = f, 
              side = "right")
  
  
  p <- plot_ly()
  p <- add_lines(p,x = ~ds.plot.i$timestamp , y = ~ ds.plot.i$N_cum, name = "Dip Tweets")
  p <- add_lines(p,x = ~ ds.plot.i$timestamp, y = ~ ds.plot.i$close ,yaxis = "y2", name = "SPY") 
  p <- layout(p,yaxis = yl, yaxis2 = yl2,xaxis = xl)
  p
  
}
