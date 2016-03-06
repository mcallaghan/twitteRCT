library("twitteR")
library("igraph")
source("define_keys.R")


setup_twitter_oauth(
  CONSUMER_KEY,
  CONSUMER_SECRET,
  ACCESS_TOKEN,
  ACCESS_TOKEN_SECRET
)

accounts <- c("realDonaldTrump","HillaryClinton")

retry <- 1000000

s <- Sys.time()
for (a in accounts) {
  followerIDs <- getUser(a)$getFollowerIDs(retryOnRateLimit=10)
  followers <- data.frame(followerIDs=followerIDs)
  saveRDS(followers, file=paste0(a,".Rda"))
}
e <- Sys.time()

print(e-s)

save(s,file="start.RData")
save(e,file="end.RData")


