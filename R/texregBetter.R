texregBetter <- function(
  l,
  ... # Additional parameters to matrixreg
){

  # Run texreg for matrix data ----
  m <- texreg::matrixreg(
    l,
    ...
  )

  # Convert to dataframe for kablization ----
  d <- as.data.frame(m)
  colnames(d) <- d[1,]
  colnames(d)[1] <- "Variables"
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
  prefixSb <- paste0(uuid::UUIDgenerate(),"_")
  for(i in 1:nModels){
    d[[paste0(prefixSe,i)]] <- NA
    d[[paste0(prefixSb,i)]] <- NA
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

  View(d) #TEMP
  return(d) #TEMP

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
