library(RMySQL)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plm)
library(R.utils)

sourceDirectory("functions/",modifiedOnly = FALSE)

user_days <- read.csv("data/user_days.csv")

load("data/tweet_examples.Rdata")
load("data/group_avs.Rdata")

i <- 0
for (d in unique(user_days$date)) {
  i <- i + 1
  user_days[user_days$date==d,"day"] <- i
  group_avs[group_avs$date==d,"day"] <- i
  tweet_examples[tweet_examples$`start date`==d,"day"] <- i
  print(d)
}

tweet_examples$daye <- lead(tweet_examples$day)

tweet_examples[is.na(tweet_examples$daye),"daye"] <- tweet_examples[is.na(tweet_examples$daye),"day"] + 2


depvars <- c("like_n","tweet_n","trump_rt_n",
                        "trump_mention_n","MAGA_n",
                        "trump_keyword_n")

likes <- user_days %>% 
  group_by(t_group,day) %>%
  summarise(
    m = mean(like_n),
    sem = sd(like_n)/sqrt(length(like_n)),
    uci = m + sem,
    lci = m - sem
  ) 

maga <- user_days %>% 
  group_by(t_group,day) %>%
  summarise(
    m = mean(MAGA_n),
    sem = sd(MAGA_n)/sqrt(length(MAGA_n)),
    uci = m + sem,
    lci = m - sem
  ) 

pd <- position_dodge(0.5)

w <- 8
h <- 5

ggplot(data=likes) +
  geom_line(
    aes(day,m,colour=t_group,group=t_group)
  ) +
  geom_line(
    aes(day,uci,colour=t_group,group=t_group),
    alpha=0.5,
    linetype=2
  ) + 
  geom_line(
    aes(day,lci,colour=t_group,group=t_group),
    alpha=0.5,
    linetype=2
  ) +
  scale_x_continuous(breaks=seq(1,i,by=1)) +
  scale_colour_discrete(guide = guide_legend(title = "Treatment\nGroup")) +
  labs(x="Day", y="Avg. Likes") +
  theme_bw() +
ggsave("plots/likes.png",width=w,height=h,dpi=500)


ggplot(data=maga) +
  geom_line(
    aes(day,m,colour=t_group,group=t_group)
  ) +
  geom_line(
    aes(day,uci,colour=t_group,group=t_group),
    alpha=0.5,
    linetype=2
  ) + 
  geom_line(
    aes(day,lci,colour=t_group,group=t_group),
    alpha=0.5,
    linetype=2
  ) +
  scale_x_continuous(breaks=seq(1,i,by=1)) +
  scale_colour_discrete(guide = guide_legend(title = "Treatment\nGroup")) +
  labs(x="Day", y="Avg. #MakeAmericaGreatAgain") +
  theme_bw() 
ggsave("plots/maga.png",width=w,height=h)


