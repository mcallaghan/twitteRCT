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

ftests <- c("F-Test (-tive Tweets)")
fp <- c("Pr(>F) (-tive Tweets)")

i <- 0
for (depvar in depvars) {
  i <- i + 1
  ud[,"y"] <- ud[,depvar]
  model <- plm(
    y ~ temptweet1 + 
      temptweet2 + 
      temptweet3 + 
      temptweet4 + 
      temptweet5 +
      temptweet6 +
      temptweet7, 
    data=ud, 
    #index=c("user_id","date"),
    model = "within", 
    effect = "time"
  )
  assign(paste0("m_",depvar),model)
  ses = vcovHC(model,type="HC0",cluster="group")
  assign(paste0("ses_",depvar),ses)
  f <- round(lmtest::waldtest(model,c("temptweet2","temptweet5","temptweet6","temptweet7"),test="F")[2,3],3)
  fpi <- round(lmtest::waldtest(model,c("temptweet2","temptweet5","temptweet6","temptweet7"),test="F")[2,4],3)
  ftests <- c(ftests,f)
  fp <- c(fp,fpi)
}


#lmtest::coeftest(m_like_n, vcov=vcovHC(m_like_n,type="HC0",cluster="group"))

model1 <- plm(
  like_n ~ temptweet1 + 
    temptweet2 + 
    temptweet3 + 
    temptweet4 + 
    temptweet5 +
    temptweet6 +
    temptweet7, 
  data=ud, 
  #index=c("user_id","date"),
  model = "within"
  #effect = c("time","group")
)

ses1 = lmtest::coeftest(model1, vcovHC(model1,type="HC0",cluster="group"))

model2 <- plm(
  like_n ~ temptweet1 + 
    temptweet2 + 
    temptweet3 + 
    temptweet4 + 
    temptweet5 +
    temptweet6 +
    temptweet7, 
  data=ud, 
  #index=c("user_id","date"),
  model = "within",
  effect = "twoway"
)


ses2 <- lmtest::coeftest(model2, vcovHC(model2,type="HC0",cluster="group"))

stargazer::stargazer(model1,model2,type="text",se=list(ses1[,2],ses2[,2]))

ud$negdummy <- ifelse(
  ud$temptweet2 == 1 | 
    ud$temptweet5 == 1 |
    ud$temptweet6 == 1 |
    ud$temptweet7 == 1,
  1,
  0
)

ud$posdummy <- ifelse(
  ud$temptweet3 == 1 |
    ud$tweet4 == 1,
  1,
  0
)

ud$neutdummy <- ifelse(
  ud$temptweet1 == 1,
  1,
  0
)

model3 <- plm(
  like_n ~ posdummy + 
    negdummy +
    neutdummy,
  data=ud, 
  #index=c("user_id","date"),
  model = "within",
  effect = "twoway"
)


ses2 <- lmtest::coeftest(model3, vcovHC(model3,type="HC0",cluster="group"))


stargazer::stargazer(
  m_like_n,m_tweet_n,m_trump_rt_n,m_trump_mention_n,m_MAGA_n,m_trump_keyword_n,
  column.labels = c("likes","tweets","retweets","mentions","MAGA","keywords"),
  type="latex",
  out ="tables/fe_results.tex",
  out.header = FALSE,
  add.lines=list(ftests,fp),
  se=list(ses_like_n,ses_tweet_n,ses_trump_rt_n,ses_trump_mention_n,ses_MAGA_n,ses_trump_keyword_n)
)



