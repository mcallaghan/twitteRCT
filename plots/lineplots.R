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

# tweet_examples$daye <- lead(tweet_examples$day)
# 
# tweet_days <- tweet_examples %>%
#   select(tweet_no,truth,day,daye) %>%
#   mutate(
#     day = day+0.1,
#     daye = daye-0.1
#   ) %>%
#   gather(se,day,-tweet_no,-truth) %>%
#   mutate(
#     ys = 0.2
#   )
# 
# 
# tweet_days <- rbind(tweet_days,mutate(tweet_days,ys=0.4,day=rev(day)))

depvars <- c("like_n","tweet_n","trump_rt_n",
                        "trump_mention_n","MAGA_n",
                        "trump_keyword_n")

likes <- user_days %>% 
  group_by(t_group,day) %>%
  summarise(
    average_likes = mean(like_n),
    sem = sd(like_n)/sqrt(length(like_n))
  ) 

yvar = 'average_likes'

pd <- position_dodge(0.5)

ggplot(data=likes) +
  geom_line(
    aes_string('day',yvar,colour='t_group',group='t_group')
  ) +
  geom_errorbar(
    aes(x=day,ymin=average_likes-sem, ymax=average_likes+sem,group=t_group,colour=t_group), 
    width=.1,
    position=pd
    ) +
  scale_x_continuous(breaks=seq(1,i,by=1)) +
  labs(x="Day", y="Avg. Likes") +
  theme_bw() 



