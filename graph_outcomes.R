library(RMySQL)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plm)

con <- try(dbConnect(MySQL(),
                     group='trump'),
           silent=TRUE
           )

if(class(con) == "try-error") {
  load("data/likes.Rda")
  load("data/tweets.Rda")
  load("data/sent_tweets.Rda")
  load("data/sample.Rda")
} else {
  sql = "SELECT * FROM all_likes
    WHERE `like_date` > '2016-04-01'
  "
  
  likes <- as.data.frame(dbGetQuery(con,sql)) 
  save(likes,file="data/likes.Rda")
  
  sql = "SELECT * FROM observed_tweets
    WHERE `tweet_date` > '2016-04-01'
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
}




#############################################
## Get counts of n likes for each user on each day
like_users <- likes %>%
  group_by(user_id,like_date) %>%
  summarise(
    like_n = length(PID)
    )

#############################################
## Get counts of n tweet data fo each user on each day
tweet_users <- tweets %>%
  group_by(user_id,tweet_date) %>%
  summarise(
    tweet_n = length(PID),
    trump_rt_n = sum(trump_retweet),
    trump_mention_n = sum(trump_mention),
    MAGA_n = sum(MAGA),
    trump_keyword_n = sum(trump_keyword)
  )

#############################################
## Get counts of sent tweets at each user on each day
sent_tweets$day <- as.character(as.Date(sent_tweets$time_sent))
sent_tweets_users <- sent_tweets %>%
  group_by(user,day) %>%
  summarise(
    sent_n = length(PID)
  )

days <- sort(unique(like_users$like_date))
users <- unique(like_users$user_id)

user_id <- rep(users,each=length(days))
date <- rep(days,times=length(users))

user_days <- data.frame(user_id,date)

user_days <- user_days %>%
  left_join(like_users,by=c("date" = "like_date","user_id" = "user_id")) %>%
  left_join(tweet_users,by=c("date" = "tweet_date","user_id" = "user_id")) %>%
  left_join(sample) %>%
  left_join(sent_tweets_users,by=c("date" = "day","username" = "user")) %>%
  select(user_id,date,username,t_group,like_n:trump_keyword_n,sent_n)


user_days[is.na(user_days)] <- 0

#########################################
## Summarise Group Averages
group_avs <-  user_days %>% 
  group_by(t_group,date) %>%
  summarise(
    average_likes = mean(like_n),
    average_rts = mean(trump_rt_n),
    average_mentions = mean(trump_mention_n),
    average_MAGA = mean(MAGA_n),
    average_keywords = mean(trump_keyword_n),
    average_tweets = mean(tweet_n),
    average_sent = mean(sent_n)
  ) 

group_avs$long_date <- group_avs$date
group_avs$date <- substr(group_avs$long_date,6,11)


########################
## Summarise User avs
user_avs <-  user_days %>% 
  group_by(user_id) %>%
  summarise(
    average_likes = mean(like_n),
    average_rts = mean(trump_rt_n),
    average_mentions = mean(trump_mention_n),
    average_MAGA = mean(MAGA_n),
    average_keywords = mean(trump_keyword_n),
    average_tweets = mean(tweet_n),
    average_sent = mean(sent_n)
  ) 

#####################################
## Plot likes
ggplot(group_avs,
       aes(date,average_likes,colour=t_group,group=t_group)) +
  geom_bar(
    aes(date,average_sent,fill=t_group),
    alpha = 0.3,
    stat="identity",
    show.legend = FALSE
  ) +
  geom_line() + 
  geom_point(shape=4,size=3) +
  theme_bw()

#####################################
## Plot RTs
ggplot(group_avs,
       aes(date,average_rts,colour=t_group,group=t_group)) +
  geom_bar(
    aes(date,average_sent,fill=t_group),
    alpha = 0.3,
    stat="identity",
    show.legend = FALSE
  ) +
  geom_line() + 
  geom_point(shape=4,size=3) +
  theme_bw()


#####################################
## Plot mentions
ggplot(group_avs,
       aes(date,average_mentions,colour=t_group,group=t_group)) +
  geom_bar(
    aes(date,average_sent,fill=t_group),
    alpha = 0.3,
    stat="identity",
    show.legend = FALSE
  ) +
  geom_line() + 
  geom_point(shape=4,size=3) +
  theme_bw()


#####################################
## Plot Make America Great Again
ggplot(group_avs,
       aes(date,average_MAGA,colour=t_group,group=t_group)) +
  geom_bar(
    aes(date,average_sent,fill=t_group),
    alpha = 0.3,
    stat="identity",
    show.legend = FALSE
  ) +
  geom_line() + 
  geom_point(shape=4,size=3) +
  theme_bw()


#####################################
## Plot Trump search term
ggplot(group_avs,
       aes(date,average_keywords,colour=t_group,group=t_group)) +
  geom_bar(
    aes(date,average_sent,fill=t_group),
    alpha = 0.3,
    stat="identity",
    show.legend = FALSE
  ) +
  geom_line() + 
  geom_point(shape=4,size=3) +
  theme_bw()


#####################################
## Plot Total Tweets
ggplot(group_avs,
       aes(date,average_tweets,colour=t_group,group=t_group)) +
  geom_bar(
    aes(date,average_sent,fill=t_group),
    alpha = 0.3,
    stat="identity",
    show.legend = FALSE
  ) +
  geom_line() + 
  geom_point(shape=4,size=3) +
  theme_bw()

group_avs_longer <- group_avs %>%
  gather(variable,value,average_likes:average_sent)

ggplot(group_avs_longer,
       aes(date,value,colour=t_group,group=interaction(variable,t_group))
       ) +
  geom_line(aes(linetype=variable)) +
  theme_bw()

user_days$date_d <- as.Date(user_days$date)

user_days$treated <- ifelse(
  user_days$date_d > as.Date("2016-04-13") &
    user_days$t_group=="T",
  1,
  0
)

write.csv(user_days,file="data/user_days.csv")

p_model <- plm(like_n ~ treated,
               data=user_days,
               index=c("user_id","date"),
               model ="within"
               )

summary(p_model)

model1 <- lm(like_n ~ t_group*date,data=user_days)
summary(model1)

model2 <- lm(trump_rt_n ~ t_group*date,data=user_days)
summary(model2)

model_maga <- lm(MAGA_n ~ t_group*date,data=user_days)
summary(model_maga)

model_keyword <- lm(trump_keyword_n ~ t_group*date,data=user_days)
summary(model_keyword)

ggplot(user_days,
       aes(date,like_n,colour=t_group)
       ) + geom_point(size=0.2) + geom_jitter(size=0.1) +
  theme_bw()

geo_sample <- sample %>%
  filter(lat > 0)

library(ggmap)
map <- get_map(location='united states',zoom=3,source="stamen",maptype="toner")

# map <- get_openstreetmap(
#   bbox = c(left=-90,
#            bottom=20,
#            right=-80,
#            top=40),
#   scale=20000000,
#   urlonly=TRUE
#   )
# ggmap(map)
# 
# library(OpenStreetMap)
# map <- openmap(c(40,120),
#                c(33,133),
#                type="osm")

ggmap(map) +
  geom_jitter(
    aes(x=lng,y=lat,colour=t_group),
    size=1,
    data=geo_sample
  ) + theme_void()
