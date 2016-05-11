plot_tc <- function(yvar,t=0) {
  p <- ggplot() 
  p <- p +
    geom_bar(
      data=filter(group_avs,t_group=="T"),
      aes(date,average_sent,fill=average_truth),
      colour="grey",
      alpha = 1,
      stat="identity",
      show.legend = FALSE
    ) 
  if(t>0) {
    p <- p +     
      geom_text(
        data=tweet_examples,
        aes(label=broken_text,x=start_date_adj,y=t),
        hjust=0,
        vjust=0,
        angle=60,
        size=2,
        alpha=0.8
      )
  }
  p <- p +
    scale_fill_gradientn(colours=c("red","white","blue"),values=c(0,0.5,1),
                         limits=c(-2,2),name="Average truth") +
    geom_line(
      data=group_avs,
      aes_string('date',yvar,colour='t_group',group='t_group')
    ) + 
    geom_point(
      data=group_avs,
      aes_string('date',yvar,colour='t_group',group='t_group'),
      shape=4,size=3
      ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle=90,hjust = 1,vjust = 0.5),
      legend.position = c(0.07,0.75),
      text=element_text(size=8)
    ) +
    labs(y="Average tweets/likes per user per day")
  return(p)
}


plot_tc_simple <- function(yvar,t=0) {
  p <- ggplot() 
  p <- p +
    geom_bar(
      data=filter(group_avs,t_group=="T"),
      aes(date,average_sent,fill=average_truth),
      colour="grey",
      alpha = 1,
      stat="identity",
      show.legend = FALSE
    ) 
  if(t>0) {
    p <- p +     
      geom_text(
        data=tweet_examples,
        aes(label=broken_text,x=start_date_adj,y=t),
        hjust=0,
        vjust=0,
        angle=60,
        size=2,
        alpha=0.8
      )
  }
  p <- p +
    scale_fill_gradientn(colours=c("red","white","blue"),values=c(0,0.5,1),
                         limits=c(-2,2),name="Average truth") +
    geom_line(
      data=group_avs,
      aes_string('date',yvar,colour='t_group',group='t_group')
    ) + 
    geom_point(
      data=group_avs,
      aes_string('date',yvar,colour='t_group',group='t_group'),
      shape=4,size=3
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle=90,hjust = 1,vjust = 0.5),
      legend.position = c(0.07,0.75),
      text=element_text(size=8)
    ) +
    labs(y="Average tweets/likes per user per day")
  return(p)
}
