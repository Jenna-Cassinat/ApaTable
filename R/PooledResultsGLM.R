PooledResultsGLM <- function(Call){
  indexvalues <- list()
  PooledModel <- summary(mice::pool(Call))
  for(m in 1:nrow(PooledModel)){
    text <- paste0(
      "*b* = ",
      round(PooledModel$estimate[[m]],2),
      ", *SE* = ",
      round(PooledModel$std.error[[m]],2),
      " OR = ",
      round(questionr::odds.ratio(Call$analyses[[1]])[[1]][[m]], 2),
      ", *p* ",
      papaja::printp(PooledModel$p.value[[m]], add_equals = TRUE)
    )
    indexvalues <- c(indexvalues, text)
  }
  names(indexvalues) <- sapply(
    as.character(PooledModel$term),
    function(x){
      if(grepl(":",x,fixed=TRUE)){
        final <- gsub("[^:]+\\$","",x)
        final <- gsub(":","X",final,fixed=TRUE)
      }else
        final <- gsub("^.+\\$", "", x)
      return(final)
    }
  )
  return(indexvalues)
}
