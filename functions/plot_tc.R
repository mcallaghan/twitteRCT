plot_tc <- function(yvar) {
  ggplot(group_avs,
         aes_string('date',yvar,colour='t_group',group='t_group')) +
    geom_bar(
      data=filter(group_avs,t_group=="T"),
      aes(date,average_sent,fill=average_truth),
      alpha = 1,
      stat="identity",
      show.legend = FALSE
    ) +
    scale_fill_gradientn(colours=c("red","white","blue"),values=c(0,0.5,1),
                         limits=c(-2,2)) +
    geom_line() + 
    geom_point(shape=4,size=3) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle=90,hjust = 1,vjust = 0.5)
    )
}