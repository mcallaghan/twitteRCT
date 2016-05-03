library(RMySQL)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plm)
library(R.utils)


sourceDirectory("functions/",modifiedOnly = FALSE)


con <- try(dbConnect(MySQL(),
                     group='trump'),
           silent=TRUE
           )

if(class(con) != "try-error") {
  get_data(con)

} 

load("data/likes.Rda")
load("data/tweets.Rda")
load("data/sent_tweets.Rda")
load("data/sample.Rda")
load("data/sample_info.Rda")
load("data/last_download.Rda")



#############################################
## Sort info about sample likes and followers
sample <- sample %>%
  left_join(sample_info)


#############################################
## Some basic descriptive statistics about the 
## sample

#############################################
## Plot followers count

followers <- ggplot(
  sample,
  aes(followers_count,..density..,colour=t_group)
  ) + 
  geom_freqpoly() +
  theme_bw()

#############################################
## Plot friends count

friends <- ggplot(
  sample,
  aes(friends_count,..density..,colour=t_group)
) + 
  geom_freqpoly() +
  theme_bw()

############################################
## Be careful with the Rmisc package, it hides
## the summarise function from dplyr - don't
## load it in library
Rmisc::multiplot(followers,friends)


#############################################
## Get counts of n likes for each user on each day
like_users <- likes %>%
  group_by(user_id,like_date) %>%
  summarise(
    like_n = length(PID)
    )

#############################################
## Get counts of n tweet data for each user on each day
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
## Get counts of sent tweets at each user on each day, and show truth value
sent_tweets$day <- as.character(as.Date(sent_tweets$time_sent))
sent_tweets_users <- sent_tweets %>%
  group_by(user,day,truth) %>%
  summarise(
    sent_n = length(PID)
  )


####################################################################
## Create a dataframe with a row for each user*day combination

days <- sort(unique(like_users$like_date))
users <- unique(like_users$user_id)

user_id <- rep(users,each=length(days))
date <- rep(days,times=length(users))

user_days <- data.frame(user_id,date)

####################################################################
## Merge this panel dataset with the others so that we get counts of 
## each variable for each user on each day, replace NAs (where there)
## were no tweets or likes recorded with 0s

user_days <- user_days %>%
  left_join(like_users,by=c("date" = "like_date","user_id" = "user_id")) %>%
  left_join(tweet_users,by=c("date" = "tweet_date","user_id" = "user_id")) %>%
  left_join(sample) %>%
  left_join(sent_tweets_users,by=c("date" = "day","username" = "user")) %>%
  select(user_id,date,username,t_group,like_n:trump_keyword_n,sent_n,truth,location:friends_count)


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
    average_sent = mean(sent_n),
    average_truth = sum(truth) / sum(sent_n)
  ) 


tweet_examples <- sent_tweets %>%
  group_by(tweet_no) %>%
  arrange(PID) %>%
  filter(row_number()==1) %>%
  select(text,truth,day) %>%
  rename(`start date` = day) %>%
  mutate(
    tlength = nchar(text),
    nspaces = length(strsplit(text," ")[[1]]),
    nmid = nspaces/2,
    nmid1 = nmid + 1,
    w1 = paste(strsplit(text," ")[[1]][1:nmid],collapse=" "),
    w2 = paste(strsplit(text," ")[[1]][nmid1:nspaces],collapse=" "),
    broken_text = paste(w1,w2,sep="\n"),
    start_date_adj = ifelse(
      `start date` == "2016-04-30",
      "2016-05-01",
      `start date`
    )
  )


#########################################################
## See functions/plot_tc.R for the function to linegraph
## T and C groups. To use it we supply the name of the
## dependent variable as a string as the first arg


#####################################
## Plot likes
plot_tc('average_likes',t=0.6) 


#####################################
## Plot RTs
plot_tc('average_rts')


#####################################
## Plot mentions
plot_tc('average_mentions')


#####################################
## Plot Make America Great Again
plot_tc('average_MAGA')


#####################################
## Plot Trump search term
plot_tc('average_keywords')


#####################################
## Plot Total Tweets
plot_tc('average_tweets')


#####################################
## plot all vars on one graph

group_avs_longer <- group_avs %>%
  gather(variable,value,average_likes:average_truth)

ggplot() +
  geom_line(
    data = filter(group_avs_longer,variable!="average_sent" & variable != "average_truth"),
    aes(date,value,colour=t_group,group=interaction(variable,t_group),linetype=variable)
            ) +
  geom_bar(
    data=filter(group_avs,t_group=="T"),
    aes(date,average_sent,fill=average_truth),
    color='grey',
    alpha = 1,
    stat="identity",
    show.legend = FALSE
  ) +
  scale_fill_gradientn(colours=c("red","white","blue"),values=c(0,0.5,1),
                       limits=c(-2,2)) +
  theme_bw()


##################################################################
## Make date_d, the date as a date object, and set treated to 1 if it
## is past the day when treatment started
## (Need to revise this part to make it more sophisticated)
user_days$date_d <- as.Date(user_days$date)

user_days$treated <- ifelse(
  user_days$date_d > as.Date("2016-04-13") &
    user_days$t_group=="T",
  1,
  0
)



#################################################################
## Write the panel dataset to a csv file
write.csv(user_days,file="data/user_days.csv")
write.csv(sample,file="data/sample_info.csv")



################################################################
################################################################
## Try out some models
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
save(map,file="data/usa_map.Rdata")

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
