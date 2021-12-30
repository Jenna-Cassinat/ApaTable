PooledResults <- function(PooledModel){
  indexvalues <- list()
  for(m in 1:nrow(PooledModel)){
    text <- paste0(
      "*b* = ",
      round(PooledModel$estimate[[m]], 2),
      ", *SE* = ",
      round(PooledModel$std.error[[m]], 2),
      ", *p* ",
      papaja::printp(PooledModel$p.value[[m]],add_equals = TRUE)
    )
    indexvalues <- c(indexvalues, text)
  }
  names(indexvalues) <- sapply(
    as.character(PooledModel$term),
    function(x){
      gsub("^.+\\$", "", x)
    }
  )
  return(indexvalues)
}
