CorTable <- function(
  dataset, # A dataframe
  table, # A correlation table supplied as output from furniture::tableC
  labels=sapply(rownames(testtable$Table1),function(x)gsub("^\\[\\d+\\]","",x),USE.NAMES = FALSE) # A vector of labels for each variable in the correlation table
){
  if(length(labels) != nrow(table$Table1)){
    stop("The number of labels does not equal the number of variables in the table.")
  }

  table$Table1[,1] <- NULL

  FinalText <- ""
  BeginTab <- "\\begin{tabular}{ l"

  Header <- "Variable"

  NumRow <- nrow(table$Table1)
  for(r in 1:NumRow){
    Header <- paste(Header, "&", r)
    BeginTab <- paste0(BeginTab, "c")
  }
  Header <- paste(Header, "\\\\")
  BeginTab <- paste(BeginTab, "}")

  FinalText <- paste(BeginTab, "\\hline", Header, "\\hline", sep="\n")

  for(m in 1:nrow(table$Table1)){
    empty <- paste0(m, ". ", labels[[m]])
    for(n in 1:ncol(table$Table1)){
      raw <- table$Table1[m,n]
      val <- stringr::str_match(raw,"^-?\\d+\\.?\\d+")[1,1]
      pVal <- stringr::str_match(raw,"(?<=\\().+(?=\\))")[1,1]
      if(!is.na(pVal)){
        if(pVal=="<.001")
          astrs <- "***"
        else if(as.numeric(pVal)<0.05)
          astrs <- "*"
        else if(as.numeric(pVal)<0.01)
          astrs <- "**"
        else
          astrs <- ""
        val <- paste0(val,astrs)
      }
      if(!is.na(val)){
        if(val== "1.00"){
          val <-"-"
        }
      }
      if(is.na(val)){
        val <- ""
      }
      empty <- paste(empty, "&", val)
    }
    empty <- paste(empty, "\\\\")
    FinalText <- paste(FinalText, empty,  sep = "\n")
  }

  FinalText <- paste(FinalText, "\\hline", sep = "\n")

  meanline <- "\\emph{M}"
  sdline <- "\\emph{SD}"

  varnames <- sapply(rownames(testtable$Table1),function(x)gsub("^\\[\\d+\\]","",x),USE.NAMES = FALSE)

  for(y in 1:ncol(table$Table1)){
    descrip <- psych::describe(dataset[,varnames[[y]]])
    avg <- round(descrip["mean"], 2)
    std <- round(descrip["sd"], 2)
    meanline <- paste(meanline, "&", avg)
    sdline <- paste(sdline, "&", std)
  }

  meanline <- paste(meanline, "\\\\")
  sdline <- paste(sdline, "\\\\")

  FinalText <- paste(FinalText, meanline, sdline, sep = "\n")

  pvaltext <- paste("*\\emph{p} \\textless .05, **\\emph{p} \\textless .01, ***\\emph{p} \\textless .001")
  pvalline <- paste0("\\multicolumn{", ncol(table$Table1)+1, "}{l}{", pvaltext, "}\\\\")
  FinalText <- paste(FinalText, "\\hline", sep = "\n")
  FinalText <- paste(FinalText, pvalline, sep = "\n")

  EndTab <- "\\end{tabular}"

  FinalText <- paste(FinalText, EndTab, sep = "\n")

  cat(FinalText)
}



