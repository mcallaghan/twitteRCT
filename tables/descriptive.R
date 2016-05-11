library(RMySQL)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plm)
library(knitr)
library(R.utils)
library(stargazer)
library(xtable)
library(ggmap)

sourceDirectory("functions/",modifiedOnly = FALSE)

user_days <- read.csv("data/user_days.csv")
load("data/sample.Rda")
load("data/sample_info.Rda")

sample <- left_join(sample,sample_info)

rm(sample_info)

length(unique(user_days$user_id))

user_avs <- user_days %>%
  filter(as.Date(date_d) < as.Date("2016-04-14")) %>%
  group_by(t_group) %>%
  summarise(
    m = mean(like_n)
  )

user_avs <-  user_days %>% 
  filter(as.Date(date_d) < as.Date("2016-04-14")) %>%
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

user_avs_long <- user_avs %>%
  left_join(sample) %>%
  select(user_id,username,t_group,location,average_likes:average_keywords,followers_count,friends_count) %>%
  gather(variable,value,average_likes:friends_count) 

user_avs_summary <- user_avs_long %>%
  group_by(variable) %>%
  summarise(
    mean = mean(value,na.rm=TRUE),
    median = median(value,na.rm=TRUE),
    SE = sd(value,na.rm=TRUE)/sqrt(length(value))
  ) %>%
  ungroup()

user_avs_summary_split <- user_avs_long %>%
  group_by(variable,t_group) %>%
  summarise(
    mean = mean(value,na.rm=TRUE),
    median = median(value,na.rm=TRUE),
    SE = sd(value,na.rm=TRUE)/sqrt(length(value))
  ) %>%
  ungroup()

treatment <- user_avs_summary_split %>%
  filter(t_group == "T") %>%
  select(-t_group)

control <- user_avs_summary_split  %>%
  filter(t_group == "C") %>%
  select(-t_group)

kable(treatment,caption="Summary statistics - treatment group")

kable(control,caption="Summary statistics - control group")

library(gdata)
g1 <- sample(letters[1:5], 1000, replace=TRUE)
g2 <- sample(LETTERS[1:3], 1000, replace=TRUE )
dat <- rnorm(1000)

stderr <- function(x) sqrt( var(x,na.rm=TRUE) / nobs(x) )

user_avs <- user_days %>%
  filter(as.Date(date_d) < as.Date("2016-04-14")) %>%
  gather(var,value,like_n:sent_n) %>%
  mutate(var=factor(var))

avs <- user_avs %>%
  group_by(t_group,var) %>%
  summarise(
    median = median(value),
    mean = mean(value),
    SEM = stderr(value)
  )

means <- tapply(user_avs$value,
       list(user_avs$t_group,user_avs$var),
       mean)

stderrs <- tapply(user_avs$value,
                list(user_avs$t_group,user_avs$var),
                stderr)

medians <- tapply(user_avs$value,
                list(user_avs$t_group,user_avs$var),
                median)


blanks <- matrix( " ", nrow=7, ncol=3)

tab <- interleave( "Mean"=round(means,2),
                   "Std Err"=round(stderrs,2),
                   "Median"=medians)

tab <- interleave( "Mean"=round(means,2),
                   "Std Err"=round(stderrs,2),
                   "Median"=medians, " " = blanks, sep=" " )

print(tab, quote=FALSE)
