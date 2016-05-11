library(RMySQL)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plm)
library(R.utils)


sourceDirectory("functions/",modifiedOnly = FALSE)


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

user_days$date_d <- as.Date(user_days$date)
sent_tweets$date_d <- as.Date(sent_tweets$day)

save(sent_tweets,file="data/sent_tweets.Rdata")


for (t in unique(sent_tweets$tweet_no)) {
  tdummy <- paste0("tweet",t)
  tdummy2 <- paste0("tweetday",t)
  for(days in unique(sent_tweets[sent_tweets$tweet_no==t,"date_d"])) {
    users <- sent_tweets[sent_tweets$date_d==days,"user"]
    user_days[user_days$username %in% users & user_days$date_d>=days,tdummy] <- 1
    #user_days[user_days$username %in% users & user_days$date_d==days,tdummy2] <- 1
  }
}

user_days[is.na(user_days)] <- 0

user_days$temptweet1 <- ifelse(user_days$tweet2==1,
                           0,
                           user_days$tweet1)

user_days$temptweet2 <- ifelse(user_days$tweet3==1,
                           0,
                           user_days$tweet2)

user_days$temptweet3 <- ifelse(user_days$tweet4==1,
                           0,
                           user_days$tweet3)

user_days$temptweet4 <- ifelse(user_days$tweet5==1,
                           0,
                           user_days$tweet4)


user_days$temptweet5 <- ifelse(user_days$tweet6==1,
                           0,
                           user_days$tweet5)

user_days$temptweet6 <- ifelse(user_days$tweet7==1,
                           0,
                           user_days$tweet6)

user_days$temptweet7 <- user_days$tweet7



write.csv(user_days,file="data/user_days.csv")


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

save(group_avs,file="data/group_avs.Rdata")



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

save(tweet_examples,file="data/tweet_examples.Rdata")

treatment_sums <- user_days %>%
  filter(date_d > "2016-04-14" & date_d < "2016-05-03") %>%
  group_by(t_group) %>%
  summarise(
    like_n = sum(like_n),
    tweet_n = sum(tweet_n)
  )

treatment_sums$like_n <- ifelse(treatment_sums$t_group=="T",
                                treatment_sums$like_n,
                                treatment_sums$like_n/4420*1000
)

treatment_sums$tweet_n <- ifelse(treatment_sums$t_group=="T",
                                treatment_sums$tweet_n,
                                treatment_sums$tweet_n/4420*1000
)

before_treatment_sums <- user_days %>%
  filter(date_d < "2016-04-14" & date_d > "2016-04-01") %>%
  group_by(t_group) %>%
  summarise(
    like_n = sum(like_n),
    tweet_n = sum(tweet_n)
  )

before_treatment_sums$like_n <- ifelse(before_treatment_sums$t_group=="T",
                                before_treatment_sums$like_n,
                                before_treatment_sums$like_n/4420*1000
)

before_treatment_sums$tweet_n <- ifelse(before_treatment_sums$t_group=="T",
                                 before_treatment_sums$tweet_n,
                                 before_treatment_sums$tweet_n/4420*1000
)


treatment_means <- user_days %>%
  filter(date_d >= "2016-04-14" & date_d < "2016-05-04") %>%
  group_by(t_group) %>%
  summarise(
    like_n = mean(like_n),
    tweet_n = mean(tweet_n)
  )

before_treatment_means <- user_days %>%
  filter(date_d < "2016-04-14" & date_d > "2016-04-01") %>%
  group_by(t_group) %>%
  summarise(
    like_n = mean(like_n),
    tweet_n = mean(tweet_n)
  )

treatment_period <- user_days %>%
  filter(date_d >= "2016-04-14" & date_d < "2016-05-04")

t.test(
  treatment_period[treatment_period$t_group=="T","like_n"],
  treatment_period[treatment_period$t_group=="C","like_n"]
  )

t.test(
  treatment_period[treatment_period$t_group=="T","tweet_n"],
  treatment_period[treatment_period$t_group=="C","tweet_n"]
)

t.test(
  treatment_period[treatment_period$t_group=="T","MAGA_n"],
  treatment_period[treatment_period$t_group=="C","MAGA_n"]
)

treatment_summary <- treatment_period %>%
  group_by(t_group)





