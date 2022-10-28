countGraph <- function(
    data,
    varNames,
    gg_title,
    gg_ylab,
    gg_color="#1a69af",
    gg_xlab="Frequency",
    gg_fill=gg_color,
    colNames=NA
){
  if(length(colNames!=1))
    if(!all(is.na(colNames)))
      if(length(colNames)!=length(varNames))
        stop("Please supply an equal number of column and variable names.")
  if(length(colNames)==1)
    if(is.na(colNames))
      colNames <- rep(NA,length(varNames))
  for(i in 1:length(colNames)){
    if(i==1)
      df <- data.frame()
    colName <- colNames[i]
    varName <- varNames[i]
    if(is.na(colName))
      lbl <- names(attributes(data[[varName]])$labels)
    else
      lbl <- colName
    ct <- length(data[[varName]][!is.na(data[[varName]])])
    df <- rbind(
      df,
      data.frame(
        Names=lbl,
        Freq=ct
      )
    )
  }
  ggplot(df, aes(x=Freq, y = Names))+
    geom_bar(stat="identity", color=gg_color, fill=gg_fill)+
    ggtitle(gg_title)+
    labs(x = gg_xlab, y = gg_ylab)+
    theme_bw()+
    theme(legend.position = "none", text= element_text(family = "serif"))
}

