LMApaTable <- function(
  model, Bold = FALSE
){
  ModOut <- summary(model)
  Coeff <-as.data.frame(ModOut$coefficients)
  Coeff$`t value` <- NULL
  rownames(Coeff)[1] <- "Intercept"
  Header <- "| Variables | *b* | *SE* | *p* value|"
  Next <- "|:----|:----:|:----:|:----:|"
  Return <- paste(Header, Next, sep = "\n")
  for(
    x in 1:nrow(Coeff)
  ){
    pattern <- "(?<=\\-)?0\\."
    VarName <- rownames(Coeff)[x]
    EstNeg <- ifelse(Coeff$Estimate[x] <0, 1, 0)
    estimate <- round(Coeff$Estimate[x], 2)
    estimate <- ifelse(estimate == 0, ".00", estimate)
    estimate <- ifelse(EstNeg == 1 & estimate == ".00", "-.00", estimate)
    estimate <- gsub(pattern, ".", estimate, perl = TRUE)
    SE <- round(Coeff$`Std. Error`[x], 2)
    SE <- gsub(pattern, ".", SE, perl = TRUE)
    P <- round(Coeff$`Pr(>|t|)`[x], 3)
    estimate <- ifelse(P < .05, paste0(estimate,"\\*"), estimate)
    estimate <- ifelse(P < .01, paste0(estimate,"\\*"), estimate)
    estimate <- ifelse(P < .001, paste0(estimate,"\\*"), estimate)
    if(Bold == TRUE){
      estimate <- ifelse(P <.05, paste0("**",estimate, "**"), estimate)}
    P <- ifelse(P < .001, "<.001", P)
    P <- gsub(pattern, ".", P, perl = TRUE)
    Row2 <- paste("|", VarName, "|", estimate, "|", SE, "|", P, "|")
    Return <- paste(Return, Row2, sep = "\n")

  }
  return(Return)
}





