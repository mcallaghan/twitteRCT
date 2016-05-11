library(RMySQL)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plm)
library(R.utils)
library(xtable)

sourceDirectory("functions/",modifiedOnly = FALSE)

load("data/sent_tweets.Rdata")

tweet_examples <- sent_tweets %>%
  group_by(tweet_no) %>%
  arrange(PID) %>%
  filter(row_number()==1) %>%
  select(text,truth,day) %>%
  rename(
    `start date` = day
  ) %>%
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
  ) %>%
  ungroup() %>%
  rename(
    `Tweet number` = tweet_no,
    Text = text,
    Truth = truth,
    `Start date` = `start date`
  )

print(xtable(
  select(tweet_examples,`Tweet number`,Text,Truth,`Start date`),
  align=c(
    "c",
    "c",
    "p{6cm}",
    "c",
    "c"),
  caption="Example tweets",
  label="table:example_tweets",
  table.placement="!h"
),
include.rownames=FALSE,
comment=FALSE,
file="tables/sent_tweets.tex"
)