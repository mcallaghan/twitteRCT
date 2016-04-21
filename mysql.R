install.packages("RMySQL")
install.packages("RODBC")

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

like_groups <- likes %>%
  group_by(t_group,like_date) %>%
  summarise(
    like_n = length(PID)
    )


like_groups$group_n <- ifelse(like_groups$t_group=="T",
                             1000,
                             3420)

like_groups$like_freq <- like_groups$like_n / like_groups$group_n


ggplot(like_groups,
       aes(like_date,like_freq,colour=t_group,group=t_group)) +
  geom_line()


