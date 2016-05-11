library(RMySQL)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plm)
library(R.utils)

sourceDirectory("functions/",modifiedOnly = FALSE)

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
group_avs <-  treatment_period %>% 
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
group_avs_narrow <- treatment_period %>%
  rename(
    `Avg tweet "Trump"` = trump_keyword_n,
    `Avg likes` = like_n,
    `Avg #MAGA` = MAGA_n,
    `Avg mentions` = trump_mention_n,
    `Avg retweets` = trump_rt_n
  ) %>%
  gather(var,value,`Avg likes`:`Avg tweet "Trump"`, -tweet_n) %>%
  group_by(t_group,var) %>%
  summarise(
    mean = mean(value),
    SEM = stderr(value),
    uci = mean+SEM,
    lci = mean-SEM
  )

group_avs_narrow$bigsmall = ifelse(
  group_avs_narrow$var=="tweet_n",
  "big",
  "small"
)

ggplot() +
  geom_bar(
    data=group_avs_narrow,
    aes(x=t_group,y=mean, fill=t_group),
    stat="identity",
    position="dodge"
  ) + 
  geom_errorbar(
    data=group_avs_narrow,
    aes(x=t_group,ymin=lci,ymax=uci,group=t_group),
    position="dodge",
    width=0.5
  ) + 
  scale_fill_discrete(guide = guide_legend(title = "Treatment\nGroup")) +
  labs(x="", y="Mean (+/- SEM)") +
  facet_wrap(~var,scales="free",drop=TRUE) +
  theme_bw() +
  theme(
    legend.position = c(0.85,0.25),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank()
  )

ggsave("plots/mean_bars.png",width=8,height=5,dpi=500)





