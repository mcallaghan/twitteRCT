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

yvar = 'average_likes'

tweet_examples$daye <- lead(tweet_examples$day)

tweet_days <- tweet_examples %>%
  select(tweet_no,truth,day,daye) %>%
  mutate(
    day = day+0.1,
    daye = daye-0.1
  ) %>%
  gather(se,day,-tweet_no,-truth) %>%
  mutate(
    ys = 0.2
  )


tweet_days <- rbind(tweet_days,mutate(tweet_days,ys=0.4,day=rev(day)))


ggplot() +
  geom_line(
    data=group_avs,
    aes_string('day',yvar,colour='t_group',group='t_group')
  ) +
  theme_bw() +
  labs(x="Avg. Likes", y="Day")



