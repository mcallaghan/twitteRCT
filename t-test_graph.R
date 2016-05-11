library(RMySQL)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plm)
library(R.utils)


user_days <- read.csv("data/user_days.csv")

## dependent variables:

depvars <- c("like_n","tweet_n","trump_rt_n",
             "trump_mention_n","MAGA_n",
             "trump_keyword_n")


## t-test for dependent variables 2 days after the last tweet (2016-05-04)
treatment_period <- user_days %>%
  mutate(date_d = as.Date(date_d)) %>%
  filter(date_d >= "2016-04-14" & date_d <= "2016-05-04")


for (y in depvars) {
  print(y)
  test <- t.test(
    treatment_period[treatment_period$t_group=="T",y],
    treatment_period[treatment_period$t_group=="C",y]
  )
  print(test)
}

treatment_summary <- treatment_period %>%
  group_by(t_group)


## calculate averages for dependentent variables during treatment period:
group_avs <-  user_days %>% 
  group_by(t_group) %>%
  summarise(
    average_likes = mean(like_n),
    average_rts = mean(trump_rt_n),
    average_mentions = mean(trump_mention_n),
    average_MAGA = mean(MAGA_n),
    average_keywords = mean(trump_keyword_n),
    average_tweets = mean(tweet_n)
  ) 


## rename dependent variables
#library(plyr)
#rename(group_avs, c("average_likes"="Avg likes", "average_rts"="Avg retweets", 
#                    "average_mentions"="Avg mentions", "average_MAGA"="Avg #MAGA",
#                    "average_keywords"="Avg tweet 'Trump'", "average_tweets"="Avg number of tweets"))


## make bar chart for dependent variables
group_avs_narrow <- group_avs %>%
  gather(stat,value,-t_group)

ggplot() +
  geom_bar(
    data=group_avs_narrow,
    aes(x=stat,y=value, fill=t_group),
    stat="identity",
    position="dodge"
  )





