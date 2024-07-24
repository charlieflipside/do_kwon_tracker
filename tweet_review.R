library(httr)
library(rvest)
library(rtweet)
library(lubridate)
library(ggplot2)
library(plotly)
library(dplyr)

# Create Twitter API Token
source("api-keys/twitter_api.R")

token = twitter_token
account = 'SBF_FTX'
max_id = NULL
all_tweets = data.frame()
for(i in 1:7){
n = 3200
# max id is oldest tweet grabbed 
if(i == 1){
  all_tweets_temp <- get_timeline(user = account,n = n, token = token)
} else { 
  all_tweets_temp <- rtweet::get_timeline(user = account,n = n, token = token, max_id = sort(all_tweets$id_str)[1])
  }
all_tweets <- rbind(all_tweets, all_tweets_temp)
}

na_cols <- lapply(all_tweets, function(x){mean(is.na(x))})
which(na_cols == 1)

# Saved in JSON just in case 
tweets_not_all_nas <- all_tweets[, -which(na_cols == 1)]

tweets_list <- as.list(tweets_not_all_nas)

tweet_text <- tweets_not_all_nas[, c("created_at", "text", "full_text", "source", "in_reply_to_screen_name")]

tweet_text <- tweet_text %>% mutate(
  short_source =  case_when(
    grepl('iPhone', tweet_text$source) ~ "iphone",
    grepl('iPad', tweet_text$source)   ~ "ipad",
    grepl('Web App', tweet_text$source) ~ "web"
  )
)

tweet_text$tweet_time_est <- as.POSIXct(tweet_text$created_at, tz = 'est')

# Move to Singapore time 
tweet_text$SGT <-  with_tz(tweet_text$tweet_time_est, tzone = "Singapore")

tweet_text$SGT_hour <- hour(tweet_text$SGT)

tweet_text$day <- as.Date(tweet_text$SGT, tz = 'Singapore')

tweet_days <- tweet_text %>% group_by(day, SGT_hour, .add = TRUE) %>% summarise(num_tweets = n())


terra_crash <- as.Date("2022-05-08")
terra_interview <- as.Date("2022-08-15")

vline <- function(x = 0, color = "black", name) {
  list(
    type = "line",
    y0 = 0,
    y1 = 1,
    yref = "paper",
    x0 = x,
    x1 = x,
    line = list(color = color, dash="dot"),
    name = name
  )
}

plot_ly(data = tweet_days, x = ~day) %>% 
  add_markers(y = ~SGT_hour, 
              mode = 'marker',
              type = 'scatter',
              marker = list(size = ~num_tweets),
              text = ~paste0("Day: ", day,
                            "\nHour: ", SGT_hour,
                            "\n# Tweets: ", num_tweets),
              hoverinfo = 'text') %>% 
  layout(title = "Do Kwon Twitter Activity w/ UST Depeg & Coinage Interview Noted",
         shapes = list(vline(terra_crash, color = 'red', name = "UST Depeg"), 
                       vline(terra_interview, color = 'black', name = "Coinage Interview")),
         xaxis = list(title = "Day"),
         yaxis = list(title = "Hour in Singapore"))

tweet_text <- tweet_text %>%  mutate(
  time_period = case_when(
    day < terra_crash ~ "pre-Depeg",
    day < terra_interview ~ "pre-Interview",
    day >= terra_interview ~ "Post-Public Interview"
  )
)
tweet_text$time_period <- factor(tweet_text$time_period, 
                                 levels = c("pre-Depeg", "pre-Interview", "Post-Public Interview"),
                                 ordered = TRUE)

tweet_text$source <- NULL 

plot_ly(tweet_text, x = ~time_period, y = ~SGT_hour, 
        color = ~time_period, type = 'box',
        boxpoints = "all", jitter = 0.3) %>% 
  layout(
    title = "Do Kwon Twitter Activity shows sizeable 4 Hour Time Shift",
    xaxis = list(title = "Time Period"),
    yaxis = list(title = "Hour in Singapore"),
    legend = list(traceorder = 'reversed')
  )

