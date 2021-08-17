BoldFunction <- function(x, y){
  y <- ifelse(x <.05, paste0("**",y, "**"), y)
  return(y)
}


doublezero <- function(x){
  pattern <- "(?<=\\-)?0\\."
  x <- gsub(pattern, ".", x, perl = TRUE)
  return(x)
}

super <- function(x){
  x <- paste0("^", x, "^")
  return(x)
}










