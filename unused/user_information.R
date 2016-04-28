library("twitteR")

source("define_keys.R")


setup_twitter_oauth(
  CONSUMER_KEY,
  CONSUMER_SECRET,
  ACCESS_TOKEN,
  ACCESS_TOKEN_SECRET
)

trump <- readRDS("realDonaldTrump.Rda")

trumps <- as.numeric(trump[sample(nrow(trump),5),])

trumps

setup_twitter_oauth(
  CONSUMER_KEY,
  CONSUMER_SECRET,
  ACCESS_TOKEN,
  ACCESS_TOKEN_SECRET
)

user1 <- getUser(trumps[1])

u <- c('geoffjentry')

lookupUsers(trumps)

users <- lookupUsers(u)

f <- favorites(users)

?getUser

trumps[1]

