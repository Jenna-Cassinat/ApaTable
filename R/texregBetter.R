# Be sure that your document includes \usepackage{multirow} !

texregBetter <- function(
  l,
  modelNames=sapply(1:length(l),function(i)paste("Model",i)),
  labels=NA, # Custom labels for coefficients
  includeStandardBeta=TRUE,
  caption="Statistical models",
  Align = "c", # "c" to center column values, "S" to align values by decimal (requires latex package "siunitx")
){

  # Warn that the S align option currently isn't working (because it currently isn't with knitr) ----
  if(Align=="S")
    stop(
      paste(
        "The \"S\" align option currently isn't working for texregBetter.",
        "It's a knitr problem.",
        "... I'm so sorry."
      )
    )

  # Validate modelNames
  if(length(l)!=length(modelNames))
    stop("Please supply a single name for each model.")

  # Run texreg for matrix data ----
  m <- texreg::matrixreg(
    l,
    ...
  )

  # Convert to dataframe for kablization ----
  variableColName <- "Variables"
  d <- as.data.frame(m)
  colnames(d) <- d[1,]
  colnames(d)[1] <- variableColName
  d <- d[2:nrow(d),]
  for(rw in 1:nrow(d))
    for(cn in 1:ncol(d))
      d[rw,cn] <- trimws(d[rw,cn])

  # Count models ----
  nModels <- ncol(d)-1

  # Rearrange data in dataframe for standard error and eventual standard beta ----
  for(rw in 1:nrow(d))
    for(cn in 1:ncol(d))
      if(!nchar(d[rw,cn]))
        d[rw,cn] <- NA
  prefixSe <- paste0(uuid::UUIDgenerate(),"_")
  for(i in 1:nModels){
    d[[paste0(prefixSe,i)]] <- NA
  }
  for(rw in which(is.na(d[,1])))
    for(cn in 2:(nModels+1)){
      if(!is.na(d[rw,cn]))
        if(nchar(d[rw,cn]))
          d[rw-1,paste0(prefixSe,cn-1)] <- as.numeric(
            gsub(
              "[\\(\\)]",
              "",
              d[rw,cn]
            )
          )
      d[rw,cn] <- NA
    }
  d <- d[!is.na(d[,1]),]
  rownames(d) <- NULL

  # Count variables ----
  nVar <- which(d[,1]=="nimp")-1

  # Drop in custom variable names if specified ----
  if(!(all(is.na(labels))&length(labels)==1)){
    if(length(labels)!=nVar)
      stop(
        paste0(
          "If you're using the labels argument, it must be a vector of length ",
          nVar,
          ". You need a variable name for each of the following:\n",
          paste(
            paste0("\t",d[1:nVar,1]),
            collapse="\n"
          )
        )
      )
    d[1:nVar,1] <- labels
  }

  # Reorder columns for formatting ----
  colOrder <- variableColName
  for(i in 1:nModels)
    colOrder <- c(
      colOrder,
      colnames(d)[i+1],
      paste0(prefixSe,i)
    )
  d <- d[,colOrder]

  # Rename all columns ----
  colnames(d) <- 1:ncol(d)

  # Remove NAs ----
  for(rw in 1:nrow(d))
    for(cn in 1:ncol(d))
      if(is.na(d[rw,cn]))
        d[rw,cn] <- ""

  # Kablize table ----
  final <- knitr::kable(
    d,
    "latex",
    caption=caption
  )

  # Drop in custom header ----
  headerPattern <- "(?<=\\\\hline\\n)[^\\\\\\\\]+\\\\\\\\"
  customHeaderVarCell <- paste0("\\\\multirow{2}{*}{",variableColName,"} \\\\\\\\")
  customHeaderModelNamesLine <- paste(
    sapply(
      modelNames,
      function(x)
        paste0("& \\\\multicolumn{2}{c}{",x,"}")
    ),
    collapse=" "
  )
  customHeaderModelNamesLine <- paste(
    customHeaderModelNamesLine,
    "\\\\\\\\"
  )
  customHeaderDetailsLine <- paste(
    sapply(
      1:nModels,
      function(i){
        if(Align=="S")
          "& $$B$$ & $$SE$$"
        else
          "& $B$ & $SE$"
      }
    ),
    collapse=" "
  )
  customHeaderDetailsLine <- paste(
    customHeaderDetailsLine,
    "\\\\\\\\"
  )
  customHeader <- paste(
    customHeaderVarCell,
    customHeaderModelNamesLine,
    paste0("\\\\cline{2-",ncol(d),"}"),
    customHeaderDetailsLine,
    collapse="\n"
  )
  final <- sub(headerPattern,customHeader,final,perl = TRUE)

  # Isolate begin tabular ----
  beginTab <- stringr::str_match(
    final,
    "\\\\begin\\{tabular\\}(\\[[A-z]\\])?\\{([A-z]\\|)+[A-z]\\}"
  )[1,1]

  # Remove all vertical lines ----
  beginTabNoPipes <- gsub("|","",beginTab,fixed = TRUE)

  # Drop in custom alignment character ----
  alignStringPattern <- "(?<=\\{)[^\\}]+(?=\\}$)"
  alignString <- trimws(stringr::str_match(beginTabNoPipes,alignStringPattern)[1,1])
  alignStringNew <- paste0("l",paste(rep(Align,nchar(alignString)-1),collapse=""))
  beginTabNoPipes <- gsub(alignStringPattern,alignStringNew,beginTabNoPipes,perl=TRUE)

  # Drop in reformed begintab directive ----
  final <- sub(beginTab,beginTabNoPipes,final,fixed = TRUE)

  # Cat final ----
  cat(final)

}

# TEST #TEMP ----

walkingImp <- mice::mice(
  mice::walking,
  m=5,
  maxit=5,
  meth='pmm',
  print=FALSE
)
m1 <- with(walkingImp,lm(walkingImp$data$age~walkingImp$data$sex+walkingImp$data$YA))
m2 <- with(walkingImp,lm(walkingImp$data$age~walkingImp$data$sex+walkingImp$data$YB))
mList <- list(
  m1,
  m2
)
coefNamesSquare <- sapply(
  mList,
  function(m){
    sapply(
      1:length(m$analyses),
      function(i)
        names(m$analyses[[i]]$coefficients)
    )
  }
)
coefNames <- c()
for(i in 1:ncol(coefNamesSquare))
  coefNames <- c(
    coefNames,
    coefNamesSquare[,i]
  )
coefNames <- gsub("^.+\\$","",unique(coefNames))
texregBetter(
  l = lapply(mList,mice::pool)
)
