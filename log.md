# Initial Setup

- Turn on Raspberry pi, install raspbian OS, connect to network and find IP address

- Connect though ssh from elsewhere in the network and setup

    - Weaved (for shell access from outside the network)

    - apache (server)

    - mysql (database)
  
    - php (server scripting)
  
    - phpmyadmin (web interfacer mysql database)
  
# Sample selection

- Get list of accounts which tweet about trump (run hourly)
```
php /var/www/html/twitter/get_trump_tweeters.php
```

- Find out the number of followers and the location of each tweeter

```
php /var/www/html/twitter/get_trump_tweeter_info.php
```

- Go through the list of trump tweeters, check their last 20 likes, and include them in trump_likers if they one of their likes is about Donald Trump
```
php /var/www/html/twitter/read_tweeters_1.php
```

-  Read through the list of tweeters - record all of the followers and friends

```
php /var/www/html/twitter/connections/get_connections.php
```

- Remove those who have more than 1000 connections


```

php /var/www/html/twitter/connections/remove_large.php
```

- Iteratively count the within group connections of each group member, each time remove from the group the user with the most connections

```

php /var/www/html/twitter/connections/sort_connections.php

```

- Randomly assign users to treatment and control groups


```

php /var/www/html/twitter/groups/randomise_groups.php
```

# Sending tweets

- Add tweets to tweet database


```

php /var/www/html/twitter/tweets/add_to_tweets.php?t=[TEXT]&a=[ALTERNATIVE TEXT]&n=[TWEET NUMBER]
```


- Instruct robots to send tweets between 9am and midnight NY time


```

bash /var/www/html/twitter/tweets/send.sh
```

# Record behaviour

- Cycle through observation group and add any unrecorded likes to the likes table (Every 15 minutes)


```

bash /var/www/html/twitter/recording/record_likes.sh
```

- Cycle through observation group and add any unrecorded tweets to the likes table - categorise tweets as Trump RTs etc. (Every 30 minutes)


```

bash /var/www/html/twitter/recording/record_tweets.sh
```
