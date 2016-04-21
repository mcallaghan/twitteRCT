library("twitteR")

source("define_keys.R")


setup_twitter_oauth(
  CONSUMER_KEY,
  CONSUMER_SECRET,
  ACCESS_TOKEN,
  ACCESS_TOKEN_SECRET
)

retry <- 1000000

results <- searchTwitter("@realDonaldTrump",n=50000,retryOnRateLimit = retry)



users <- unique(unlist(lapply(results,screenName)))

user_table <- data.frame(username=users)

write.table(user_table,"user_table.txt",sep="\t",row.names = FALSE)