library(RMySQL)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plm)
library(R.utils)

sourceDirectory("functions/",modifiedOnly = FALSE)


user_days <- read.csv("data/user_days.csv")

## dependent variables:

depvars <- c("like_n","tweet_n","trump_rt_n",
             "trump_mention_n","MAGA_n",
             "trump_keyword_n")


## t-test for dependent variables 2 days after the last tweet (2016-05-04)
treatment_period <- user_days %>%
  mutate(date_d = as.Date(date_d)) %>%
  filter(date_d >= "2016-04-14" & date_d <= "2016-05-04") %>%
  rename(
    `Avg tweet "Trump"` = trump_keyword_n,
    `Avg likes` = like_n,
    `Avg #MAGA` = MAGA_n,
    `Avg mentions` = trump_mention_n,
    `Avg retweets` = trump_rt_n,
    `Avg tweets` = tweet_n
  )


## make bar chart for dependent variables
treatment_period_narrow <- treatment_period %>%
  gather(var,value,`Avg likes`:`Avg tweet "Trump"`) 

averages <- data.frame(variable=unique(treatment_period_narrow$var))

for (y in unique(treatment_period_narrow$var)) {
  print(y)
  test <- t.test(
    treatment_period[treatment_period$t_group=="T",y],
    treatment_period[treatment_period$t_group=="C",y]
  )
  t
  print(test)
  averages[averages$variable==y,"control mean"] <- test$estimate[[2]]
  averages[averages$variable==y,"treatment mean"] <- test$estimate[[1]]
  averages[averages$variable==y,"p-value"] <- test$p.value
}

averages$`p-value` <- 
  ifelse(
    averages$`p-value` > 0.01,
    ifelse(
      averages$`p-value` > 0.05,
      ifelse(
        averages$`p-value` > 0.1,
        round(averages$`p-value`,2),
        paste0(round(averages$`p-value`,2)," *")
      ),
      paste0(round(averages$`p-value`,2)," **")
      ),
    paste0(round(averages$`p-value`,2), "***")
  )
    

print(
  xtable::xtable(
    averages,
    caption="A t-test on the difference in means between treatment and control groups",
    label="table:diff_means"
    ),
  file="tables/mean_t_tests.tex",
  include.rownames = FALSE
)


treatment_period_summary <- treatment_period_narrow %>%
  group_by(t_group,var) %>%
  dplyr::summarise(
    mean = mean(value),
    SEM = stderr(value),
    uci = mean+SEM,
    lci = mean-SEM
  )


ggplot() +
  geom_bar(
    data=treatment_period_summary,
    aes(x=t_group,y=mean, fill=t_group),
    stat="identity",
    position="dodge"
  ) + 
  geom_errorbar(
    data=treatment_period_summary,
    aes(x=t_group,ymin=lci,ymax=uci,group=t_group),
    position="dodge",
    width=0.5
  ) + 
  scale_fill_discrete(guide = guide_legend(title = "Treatment\nGroup")) +
  labs(x="", y="Mean (+/- SEM)") +
  facet_wrap(~var,scales="free",drop=TRUE) +
  theme_bw() +
  theme(
    #legend.position = c(0.85,0.25),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank()
  )

ggsave("plots/mean_bars.png",width=8,height=5,dpi=500)





