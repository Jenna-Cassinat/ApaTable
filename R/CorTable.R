CorTable <- function(
  dataset, # A dataframe
  table, # A correlation table supplied as output from furniture::tableC(dataset)
  caption = "Bivariate correlations and descriptive statistics of study variables", # The caption that shows up for the correlation table
  labels = NA, # A vector of labels for each variable in the correlation table
  Align = "S", # "c" to center column values, "S" to align values by decimal (requires latex package "siunitx")
  italicizeCaption = FALSE,
  rotate=FALSE # You need \usepackage{rotating} to do this
){

  # Validate labels argument ----
  if(!is.na(labels))
    if(length(labels) != nrow(table$Table1)){
      stop("The number of labels does not equal the number of variables in the table.")
    }

  # Isolate dataframe from correlation table object ----
  d <- table$Table1

  # Derive variable names ----
  varnames <- sapply(
    rownames(d),
    function(x)
      gsub("^\\[\\d+\\]","",x),
    USE.NAMES = FALSE
  )

  # Subset source data for variables in use ----
  datasub <- dataset[,varnames]
  n <- sum(complete.cases(datasub))

  # Add complete case count to table caption ----
  caption <- paste0(
    caption,
    " (N = ",
    n,
    ")"
  )

  # Italicize table caption if specified ----
  if(italicizeCaption)
    caption <- paste0("\\emph{",caption,"}")

  # Fix column names ----
  colnames(d) <- c(
    "Variable",
    1:(ncol(d)-1)
  )

  # Set labels to variable names if not specified ----
  if(is.na(labels))
    labels <- varnames

  # Number labels and drop in for surrogate row names ----
  labels <- sapply(
    1:length(labels),
    function(i)
      paste0(i,". ",labels[i])
  )
  d[,1] <- labels

  # Convert columns to type character ----
  for(cn in 2:ncol(d))
    d[,cn] <- as.character(d[,cn])

  # Format dataframe values ----
  for(m in 1:nrow(d)){
    for(n in 2:ncol(d)){
      raw <- d[m,n]
      val <- stringr::str_match(raw,"^-?\\d+\\.?\\d+")[1,1]
      pVal <- stringr::str_match(raw,"(?<=\\().+(?=\\))")[1,1]
      if(!is.na(val)){
        val <- round(as.numeric(val), 2)
      }
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
        if(val== 1.00){
          val <- "\\textemdash" #TEMP
        }
      }
      if(is.na(val)){
        val <- ""
      }
      d[m,n] <- val
    }
  }

  # Typeset table ----
  final <- knitr::kable(
    d,
    "latex",
    row.names=FALSE,
    align=c(
      "l",
      rep(Align,ncol(d)-1)
    ),
    caption=caption,
    escape=FALSE
  )
  final <- kableExtra::kable_styling(
    final,
    latex_options = "hold_position"
  )

  # Rotate table if specified ----
  if(rotate)
    final <- gsub(
      "((?<=\\\\begin\\{)table(?=\\})|(?<=\\\\end\\{)table(?=\\}))",
      "sidewaystable",
      final,
      perl=TRUE
    )

  # Remove all vertical lines ----
  beginTab <- stringr::str_match(
    final,
    "\\\\begin\\{tabular\\}(\\[[A-z]\\])?\\{([A-z]\\|)+[A-z]\\}"
  )[1,1]
  beginTabNoPipes <- gsub("|","",beginTab,fixed = TRUE)
  final <- sub(beginTab,beginTabNoPipes,final,fixed = TRUE)

  # Remove all inner horizontal lines in output ----
  placeholder <- uuid::UUIDgenerate()
  hlineOld <- "\\\\hline"
  hlineNew <- "\\hline"
  nLinesToKeep <- 2
  for(i in 1:nLinesToKeep)
    final <- sub(
      hlineOld,
      placeholder,
      final
    )
  final <- gsub(
    hlineOld,
    "",
    final
  )
  final <- gsub(
    placeholder,
    hlineNew,
    final,
    fixed = TRUE
  )

  # Add mean and standard deviation per column and p-value key ----
  meanline <- "\\emph{M}"
  sdline <- "\\emph{SD}"
  for(i in 1:length(varnames)){
    avg <- round(mean(dataset[,varnames[[i]]],na.rm = TRUE),2)
    std <- round(sd(dataset[,varnames[[i]]],na.rm = TRUE),2)
    meanline <- paste(meanline, "&", avg)
    sdline <- paste(sdline, "&", std)
  }
  meanline <- paste(meanline, "\\\\")
  sdline <- paste(sdline, "\\\\")
  pvalText <- "*\\emph{p} \\textless .05, **\\emph{p} \\textless .01, ***\\emph{p} \\textless .001"
  pvalLine <- paste0("\\multicolumn{", ncol(d), "}{l}{", pvalText, "}\\\\")
  footer <- paste(hlineNew,meanline,sdline,hlineNew,pvalLine,sep="\n")
  final <- kableExtra::row_spec(
    final,
    nrow(d),
    extra_latex_after = footer
  )

  # Cat results ----
  cat(final)
}
