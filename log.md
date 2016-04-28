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
