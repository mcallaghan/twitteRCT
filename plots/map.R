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


load("data/sample.Rda")
load("data/sample_info.Rda")


load("data/usa_map.Rdata")

sample <- sample %>%
  left_join(sample_info)

geo_sample <- sample %>%
  filter(lat > 0)

ggmap(map) +
  geom_jitter(
    aes(x=lng,y=lat,colour=t_group),
    size=0.5,
    data=geo_sample
  ) + theme_void()