LMApaTable <- function(
  model, Bold = FALSE, Quiet = FALSE, Return = FALSE
){
  ModOut <- summary(model)
  Coeff <-as.data.frame(ModOut$coefficients)
  Coeff$`t value` <- NULL
  rownames(Coeff)[1] <- "Intercept"
  Header <- "| Variables | *b* | *SE* | *p* value|"
  Next <- "|:----|:----:|:----:|:----:|"
  Table <- paste(Header, Next, sep = "\n")
  for(
    x in 1:nrow(Coeff)
  ){
    VarName <- rownames(Coeff)[x]
    EstNeg <- ifelse(Coeff$Estimate[x] <0, 1, 0)
    estimate <- round(Coeff$Estimate[x], 2)
    estimate <- ifelse(estimate == 0, ".00", estimate)
    estimate <- ifelse(EstNeg == 1 & estimate == ".00", "-.00", estimate)
    estimate <- doublezero(estimate)
    SE <- round(Coeff$`Std. Error`[x], 2)
    SE <- doublezero(SE)
    P <- round(Coeff$`Pr(>|t|)`[x], 3)
    estimate <- ifelse(P < .05, paste0(estimate,"\\*"), estimate)
    estimate <- ifelse(P < .01, paste0(estimate,"\\*"), estimate)
    estimate <- ifelse(P < .001, paste0(estimate,"\\*"), estimate)
    if(Bold == TRUE){
      estimate <- BoldFunction(P, estimate)
      P <- ifelse(P < .001, "<.001", P)
      P <- doublezero(P)
      Row2 <- paste("|", VarName, "|", estimate, "|", SE, "|", P, "|")
      Table <- paste(Table, Row2, sep = "\n")

    }
  }
  if (Quiet == FALSE){
    cat(Table,"\n")
  }
  if (Return == TRUE){
    return(Table)
  }else{
    invisible()
  }
}




AOVApaTable <- function(
  ..., Bold = FALSE, Quiet = FALSE, Return = FALSE
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

  Header <- "| Model |"
  Next <- "|:----|"

  FactorLevels <- levels(models[[1]]$model[[as.character(GroupVar)]])

  for(k in FactorLevels){
    DV1Name <- paste(k,"|")
    Header <- paste(Header, DV1Name)
    Next <- paste0(Next, ":--:|")
  }

  Table <- paste(Header, Next, sep= "\n")

  for(b in models){
    IV1 <- as.character(b$call$formula[[2]])
    sumtest <- summary(b)
    Pvalue <- sumtest[[1]][as.character(GroupVar),"Pr(>F)"]
    if(Bold ==TRUE){
      IV1 <- BoldFunction(Pvalue, IV1)}
    Data <- b$model
    RowText <- paste("|", IV1, "|")

    post <- TukeyHSD(b, as.character(GroupVar))
    post <- as.data.frame(post[[as.character(GroupVar)]])

    for(r in FactorLevels){
      Subdata <- subset(Data, Data[[as.character(GroupVar)]] == r)[[IV1]]
      average <- round(mean(Subdata), 2)
      average <- doublezero(average)
      standD <- round(sd(Subdata), 2)
      standD <- doublezero(standD)

      pattern <- paste0("(^",r,"|",r,"$)")
      rowsub <- subset(post, grepl(pattern,rownames(post)))
      form <- paste0(average, " (", standD, ")")
      supvec <- c()

      for (w in 1:nrow(rowsub)){
        rowsub2 <- subset(rowsub, w == (1:nrow(rowsub)))
        cellloc <- rownames(rowsub2)

        left <- stringr::str_match_all(cellloc, paste0("^", r))[[1]][1]
        right <- stringr::str_match_all(cellloc, paste0(r, "$"))[[1]][1]
        if(is.na(left)){
          left <- gsub(paste0("-", right), "", cellloc, fixed = TRUE)
        }
        if(is.na(right)){
          right <- gsub(paste0(left, "-"), "", cellloc, fixed = TRUE)
        }

        choose <- ifelse(left == as.character(r), right, left)

        postp <- rowsub2$`p adj`


        if(postp <.05){
          supvec <- c(supvec, choose)
        }

      }
      supvec <- paste(supvec, collapse = ", ")
      if(nchar(supvec)>0){
        supvec <- super(supvec)
        form <- paste(form, supvec)
      }
      RowText <- paste(RowText, form, "|")
    }
    Table <- paste(Table, RowText, sep = "\n")
  }

  if (Quiet == FALSE){
    cat(Table,"\n")
  }
if (Return == TRUE){
  return(Table)
}else{
  invisible()
}
}


PieChart <- function(data,x){
  data$x <- eval(substitute(x),data)
  varName <- as.character(parse(text=substitute(x)))
  FreqTab <- furniture::tableF(data,x)

  FactNum <- (length(FreqTab$x$x))
  VisTable <- data.frame(unlist(FreqTab$x$x))
  VisTable <- VisTable %>%
    rename(Class = unlist.FreqTab.x.x.)

  for (w in nrow(VisTable)){
    VisTable$n <- as.numeric(FreqTab$x$Freq)
    VisTable$prop <- (as.numeric(FreqTab$x$Freq)/as.numeric(FreqTab$x$CumFreq[[FactNum]]))*100

  }

  VisTable <- VisTable %>%
    mutate(ypos = cumsum(prop)- 0.5*prop )

  ggplot(VisTable, aes(x="", y=n, fill=Class)) +
    geom_bar(stat="identity", width=1) +
    coord_polar("y", start=0)+
    theme(legend.position="none") +
    theme_void()


}


