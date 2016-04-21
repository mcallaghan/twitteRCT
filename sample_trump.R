library("twitteR")

source("define_keys.R")


setup_twitter_oauth(
  CONSUMER_KEY,
  CONSUMER_SECRET,
  ACCESS_TOKEN,
  ACCESS_TOKEN_SECRET
)

trump <- readRDS("realDonaldTrump.Rda")

trump_sample <- as.numeric(trump[sample(nrow(trump),200000),])

trump_sample_large <- data.frame(id=trump_sample)

write.table(trump_sample_large,"trump_sample_raw.txt",sep="\t",row.names = FALSE)
