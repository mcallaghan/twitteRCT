dbhost <- '192.168.250.77'
dbuser <- 'r'
dbpass <- '9YpiFW07' 
dbname <- 'trump'

library(RMySQL)
library(ggplot2)
library(dplyr)

con <- dbConnect(MySQL(),
                 user = dbuser,
                 password = dbpass,
                 host = dbhost,
                 dbname=dbname)

sql = "SELECT 
  `all_likes`.PID,`all_likes`.user_id,`all_likes`.like_id,`all_likes`.like_date,
  `observation_group`.t_group
  FROM `all_likes` 
  LEFT JOIN `observation_group` ON
  `all_likes`.user_id = `observation_group`.user_id
  WHERE `like_date` > '2016-04-01'
"

likes <- as.data.frame(dbGetQuery(con,sql)) 

sql = "SELECT user_id,t_group
  FROM observation_group
"

groups <- as.data.frame(dbGetQuery(con,sql)) 

like_users <- likes %>%
  group_by(user_id,like_date) %>%
  summarise(
    like_n = length(PID)
    ) %>%
  left_join(groups)

days <- sort(unique(like_users$like_date))
users <- unique(like_users$user_id)

user_id <- rep(users,each=length(days))
like_date <- rep(days,times=length(users))

user_days <- data.frame(user_id,like_date)


like_groups <-  likes %>% 
  group_by(t_group,like_date) %>%
  summarise(
    like_n = length(PID)
  ) %>%
  left_join(groups)

like_groups$group_n <- ifelse(like_groups$t_group=="T",
                             1000,
                             3420)

like_groups$like_freq <- like_groups$like_n / like_groups$group_n

like_users <- like_users %>%
  right_join(user_days) %>%
  select(-t_group) %>%
  left_join(groups) %>%
  mutate(
    like_n = ifelse(
      is.na(like_n),
      0,
      like_n
      )
  )

ggplot(like_groups,
       aes(like_date,like_freq,colour=t_group,group=t_group)) +
  geom_line() + 
  theme_bw()


model <- lm(like_n ~ user_id + t_group*like_date,data=like_users)
summary(model)

ggplot(like_users,
       aes(like_date,like_n,colour=t_group)
       ) + geom_point(size=0.2) + geom_jitter(size=0.1) +
  theme_bw()


