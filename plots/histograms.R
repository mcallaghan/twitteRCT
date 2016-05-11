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

followers <- ggplot(
  sample,
  aes(followers_count,..density..,colour=t_group)
) + 
  geom_freqpoly() +
  scale_colour_discrete(guide = guide_legend(title = "Treatment\nGroup")) +
  labs(
    x="Followers"
    #y="Avg. #MakeAmericaGreatAgain"
    ) +
  theme_bw()

#############################################
## Plot friends count

friends <- ggplot(
  sample,
  aes(friends_count,..density..,colour=t_group)
) + 
  geom_freqpoly() +
  scale_colour_discrete(guide = guide_legend(title = "Treatment\nGroup")) +
  labs(
    x="Following"
    #y="Avg. #MakeAmericaGreatAgain"
  ) +
  theme_bw()

png("plots/hists.png",width=800,height=500)
Rmisc::multiplot(followers,friends,cols=2)
dev.off()
?png

??multiplot

