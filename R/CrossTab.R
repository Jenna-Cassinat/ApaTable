CrossTab <- function(data,
                     xvar,
                     crossvar,
                     gg_title,
                     gg_ylab,
                     gg_xlab="Frequency",
                     color="PuBu"){

  ggplot(data, aes(fill=data[[xvar]], y=data[[crossvar]])) +  geom_bar(position="fill")+
    labs(x = gg_xlab, y = gg_ylab)+
    ggtitle(gg_title)+
    scale_fill_brewer(palette=color)+
    theme_bw()

}
