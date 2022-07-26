MultFreq <- function(data,
                     xvar,
                     varnames,
                     gg_title,
                     gg_ylab,
                     gg_xlab="Frequency",
                     color="PuBu"){

  countab <- matrix(nrow=0, ncol=2)

  for(j in 1:length(xvar)){
    varname1 <- varnames[j]
    for(m in 1:nrow(data)){
      countab <- rbind(countab,matrix(c(varname1, data[m,xvar[j]]), ncol=2, byrow=TRUE))

    }
  }

  countab <- as.data.frame(countab)
  colnames(countab) <- c("Variables","Levels")

  countab$Variables <- unlist(countab$Variables)
  countab$Levels <- unlist(countab$Levels)
  str(countab)

  ggplot(countab, aes(fill=Levels, y=Variables)) +  geom_bar(position="fill")+
    labs(x = gg_xlab, y = gg_ylab)+
    ggtitle(gg_title)+
    scale_fill_brewer(palette=color)+
    theme_bw()

}

