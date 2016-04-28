# Script for graphing Twitter friends/followers
# by Kai Heinrich (kai.heinrich@mailbox.tu-dresden.de) 

# load the required packages

library("twitteR")
library("igraph")

source("define_keys.R")


setup_twitter_oauth(
  CONSUMER_KEY,
  CONSUMER_SECRET,
  ACCESS_TOKEN,
  ACCESS_TOKEN_SECRET
)

# HINT: In order for the tkplot() function to work on mac you need to install 
#       the TCL/TK build for X11 
#       (get it here: http://cran.us.r-project.org/bin/macosx/tools/)
#
# Get User Information with twitteR function getUSer(), 
#  instead of using ur name you can do this with any other username as well 

account <- "realDonaldTrump"

start<-getUser(account) 

# Get Friends and Follower names with first fetching IDs (getFollowerIDs(),getFriendIDs()) 
#and then looking up the names (lookupUsers()) 

friends.object<-lookupUsers(start$getFriendIDs())
followers.object<-lookupUsers(start$getFollowerIDs())

# Retrieve the names of your friends and followers from the friend
# and follower objects. You can limit the number of friends and followers by adjusting the 
# size of the selected data with [1:n], where n is the number of followers/friends 
# that you want to visualize. If you do not put in the expression the maximum number of 
# friends and/or followers will be visualized.

n<-20
nf <- length(friends)
friends <- sapply(friends.object[1:n],name)
nff <- length(followers)
followers <- sapply(followers.object[1:n],name)

# Create a data frame that relates friends and followers to you for expression in the graph
relations <- merge(data.frame(User=account, Follower=friends), 
                   data.frame(User=followers, Follower='YOUR_NAME'), all=T)

# Create graph from relations.
g <- graph.data.frame(relations, directed = T)

# Assign labels to the graph (=people's names)
V(g)$label <- V(g)$name

# Plot the graph using plot() or tkplot(). Remember the HINT at the 
# beginning if you are using MAC OS/X
tkplot(g)
