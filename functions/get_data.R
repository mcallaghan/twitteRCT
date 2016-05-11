get_data <- function(con) {
  sql = "SELECT * FROM all_likes
    WHERE `like_date` > '2016-04-06'
    AND `like_date` < '2016-05-11'
  "
  
  likes <- as.data.frame(dbGetQuery(con,sql)) 
  save(likes,file="data/likes.Rda")
  
  sql = "SELECT * FROM observed_tweets
  WHERE `tweet_date` > '2016-04-06'
  AND `tweet_date` < '2016-05-11'
  AND reply = 0
  "
  
  tweets <- as.data.frame(dbGetQuery(con,sql)) 
  save(tweets,file="data/tweets.Rda")
  
  sql = "SELECT * FROM tweets
  WHERE time_sent > '2016-04-10'
  "
  
  sent_tweets <- as.data.frame(dbGetQuery(con,sql)) 
  save(sent_tweets,file="data/sent_tweets.Rda")
  
  sql = "SELECT user_id,username,t_group,location,country,lat,lng
  FROM observation_group
  "
  
  sample <- as.data.frame(dbGetQuery(con,sql)) 
  save(sample,file="data/sample.Rda")
  
  sql = "SELECT user_id,followers_count,friends_count,location
    FROM trump_tweeters
  "
  
  sample_info <- as.data.frame(dbGetQuery(con,sql)) 
  save(sample_info,file="data/sample_info.Rda")
  
  now <- Sys.time()
  save(now,file="data/last_download.Rda")
}