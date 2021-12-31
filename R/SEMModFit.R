SEMModFit <- function(ModelName){
  text <- paste0(
    "$\\chi$$^2$ (",
    round(ModelName$FIT[[3]], 2),
    ", N = ",
    ModelName$FIT[[15]], ") = ",
    round(ModelName$FIT[[3]], 2),
    ", *p* ",
    papaja::printp(fitTest$FIT[[5]]),
    ", RMESA = ",
    round(fitTest$FIT[[17]], 2),
    ", CFI = ",
    round(fitTest$FIT[[9]], 2),
    ", TLI = ",
    round(fitTest$FIT[[10]], 2),
    ", SRMR = ",
    round(fitTest$FIT[[21]], 2)
  )
  return(text)
}
