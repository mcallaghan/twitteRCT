library(RMySQL)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plm)
library(R.utils)

sourceDirectory("functions/",modifiedOnly = FALSE)

user_days <- read.csv("data/user_days.csv")

ud <- plm.data(user_days,c("user_id","date"))

ind <- duplicated(ud[,1:2])

ud <- ud[!ind,]

depvars <- c("like_n","tweet_n","trump_rt_n",
             "trump_mention_n","MAGA_n",
             "trump_keyword_n")

models <- list()

ftests <- c("F-Test (Negative Tweets)")
fp <- c("Pr(>F) (Negative Tweets)")

i <- 0
for (depvar in depvars) {
  i <- i + 1
  ud[,"y"] <- ud[,depvar]
  model <- plm(
    y ~ tweet1 + 
      tweet2 + 
      tweet3 + 
      tweet4 + 
      tweet5 +
      tweet6 +
      tweet7, 
    data=ud, 
    #index=c("user_id","date"),
    model = "within", 
    effect = "time"
  )
  assign(paste0("m_",depvar),model)
  if(i == 1) {
    models <- list(model)
  } else {
    models <- list(models,model)
  }
  f <- round(lmtest::waldtest(model,c("tweet2","tweet5","tweet6","tweet7"),test="F")[2,3],3)
  fpi <- round(lmtest::waldtest(model,c("tweet2","tweet5","tweet6","tweet7"),test="F")[2,4],3)
  ftests <- c(ftests,f)
  fp <- c(fp,fpi)
}

stargazer::stargazer(
  models,
  column.labels = depvars,
  type="text"
  )

stargazer::stargazer(
  m_like_n,m_tweet_n,m_trump_rt_n,m_trump_mention_n,m_MAGA_n,m_trump_keyword_n,
  column.labels = c("likes","tweets","retweets","mentions","MAGA","keywords"),
  type="text",
  add.lines=list(ftests,fp)
)

like_model <- plm(
  like_n ~ tweet1 + 
    tweet2 + 
    tweet3 + 
    tweet4 + 
    tweet5 +
    tweet6 +
    tweet7, 
  data=ud, 
  model = "within", 
  effect = "time"
)

lmtest::waldtest(like_model,c("tweet2","tweet5","tweet6","tweet7"),test="F")



