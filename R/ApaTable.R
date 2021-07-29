LMApaTable <- function(
  model, Bold = FALSE, Quiet = FALSE
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
    if (Quiet == FALSE){
      cat(Return)
    }
    Return <- paste(Return, Row2, sep = "\n")

  }
  return(Return)
}



AOVApaTable <- function(
  ..., data, Bold = FALSE, Quiet = FALSE
){
  models <- list(...)

  dvcheck <- c()

  for (p in models){
    GroupVar <- p$call$formula[[3]]
    dvcheck <- c(dvcheck,GroupVar)
  }

  uniqueVar <- length(unique(dvcheck))

  if(uniqueVar > 1){
    stop("The grouping variables in these models do not match; change models so that each have same grouping variable.")
  }

  datalength <- length(data)

  if(datalength != 1 | datalength != length(models)){
    stop("Make sure that either all data frames utilized are the same data, or the number of data frames matches the number of models provided")
  }


  return(uniqueVar)
}

AOVApaTable(test1, test2, test3)



library(haven)
data <- read_sas("/Users/Jenna/Box/Dissertation/FRP Study/Data/threefocus.sas7bdat")



test1 <- aov(osintim11 ~ GENDCOMP, data = data)
test2 <- aov(osdeide11 ~ GENDCOMP, data = data)
test3 <- aov(osmodele11 ~ GENDCOMP, data = data)
test3 <- aov(osmodele11 ~ GENDCOMP, data = data)
test3 <- aov(osmodele11 ~ GENDCOMP, data = data)
test3 <- aov(osmodele11 ~ GENDCOMP, data = data)









test3 <- aov(osmodele11 ~ GENDCOMP, data = data)summary(test)

#AOVApaTable(Nodel1, mode2, model3, OPTIONS, data = list(data1, data2, data3))

group1 <- subset(data, data$GENDCOMP == 1)
psych::describe(group1$osintim11)$sd


