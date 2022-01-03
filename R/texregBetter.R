# Be sure that your document includes \usepackage{multirow} !

texregBetter <- function(
  l,
  modelNames=sapply(1:length(l),function(i)paste("Model",i)),
  labels=NA, # Custom labels for coefficients
  includeStandardBeta=TRUE, # Not currently used
  caption="Statistical models",
  hlineAfterVars=TRUE,
  Align = "c", # "c" to center column values, "S" to align values by decimal (requires latex package "siunitx")
  includeOddsRatio = rep(FALSE,length(l)) # This is the primary argument in the texregBetter shorthand; no need to mess with it here.
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

  # Validate includeOddsRatio ----
  if(length(includeOddsRatio)!=length(l))
    stop("includeOddsRatio should be a T/F vector with a value for each model.")

  # Run texreg for matrix data ----
  m <- texreg::matrixreg(l)

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

  # Tag model columns in dataframe for now ----
  prefixM <- paste0(uuid::UUIDgenerate(),"_")
  colnames(d)[2:ncol(d)] <- sapply(
    1:nModels,
    function(i)
      paste0(prefixM,i)
  )

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

  # Add in odds ratios if specified ----
  prefixOr <- paste0(uuid::UUIDgenerate(),"_")
  for(i in 1:nModels)
    if(includeOddsRatio[i]){
      d[,paste0(prefixOr,i)] <- sapply(
        1:nrow(d),
        function(j){
          x <- d[,i+1][[j]]
          if(j>nVar)
            return(NA)
          if(d[j,1]=="(Intercept)")
            return(intToUtf8(8212))
          if(is.na(x))
            return(x)
          beta <- as.numeric(
            trimws(
              gsub(
                "\\*",
                "",
                x
              )
            )
          )
          final <- round(exp(beta),2)
          return(final)
        }
      )
    }

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
  for(i in 1:nModels){
    colOrder <- c(
      colOrder,
      paste0(prefixM,i),
      paste0(prefixSe,i)
    )
    if(includeOddsRatio[i])
      colOrder <- c(
        colOrder,
        paste0(prefixOr,i)
      )
  }
  d <- d[,colOrder]

  # Rename nimps and nobs rows for later ----
  placeholdNimp <- uuid::UUIDgenerate()
  placeholdNobs <- uuid::UUIDgenerate()
  d[d[,1]=="nimp",1] <- placeholdNimp
  d[d[,1]=="nobs",1] <- placeholdNobs

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

  # Drop in actual names for imps and nobs rows ----
  nimpsNobsPlaceholders <- c(placeholdNimp,placeholdNobs)
  nimpsNobsNames <- c("$N$ Imputations","$N$ Observations")
  for(i in 1:length(nimpsNobsPlaceholders))
    final <- gsub(
      nimpsNobsPlaceholders[i],
      nimpsNobsNames[i],
      final,
      fixed=TRUE
    )

  # Drop in custom header ----
  headerPattern <- "(?<=\\\\hline\n)[^\\n]+\\\\\\\\"
  customHeaderVarCell <- paste0("\\\\multirow{2}{*}{",variableColName,"} \\\\\\\\")
  customHeaderModelNamesLine <- paste(
    sapply(
      1:nModels,
      function(i){
        modelName <- modelNames[i]
        nColsToSpan <- length(grep(paste0(i,"$"),colnames(d)))
        final <- paste0("& \\\\multicolumn{",nColsToSpan,"}{c}{",modelName,"}")
        return(final)
      }
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
        dSign <- uuid::UUIDgenerate()
        baseString <- glue::glue("& {dSign}B{dSign} & {dSign}SE{dSign}")
        if(includeOddsRatio[i])
          baseString <- paste(
            baseString,
            glue::glue("& OR")
          )
        if(Align=="S")
          dSignTrue <- "$$"
        else
          dSignTrue <- "$"
        final <- gsub(dSign,dSignTrue,baseString,fixed=TRUE)
        return(final)
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

  # Remove horizontal lines ----
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

  # Add horizontal line after variables if specified ----
  if(hlineAfterVars)
    final <- kableExtra::row_spec(
      final,
      nVar,
      hline_after=TRUE
    )

  # Add horizontal line after data ----
  final <- kableExtra::row_spec(
    final,
    nrow(d),
    hline_after=TRUE
  )

  # Add p-value reference ----
  pvalText <- "*\\emph{p} \\textless .05, **\\emph{p} \\textless .01, ***\\emph{p} \\textless .001"
  pvalLine <- paste0("\\multicolumn{", ncol(d), "}{l}{", pvalText, "}\\\\")
  endTab <- "\\end{tabular}"
  final <- gsub(
    endTab,
    paste(
      pvalLine,
      endTab,
      collapse="\n"
    ),
    final,
    fixed=TRUE
  )

  # Cat final ----
  cat(final)

}
