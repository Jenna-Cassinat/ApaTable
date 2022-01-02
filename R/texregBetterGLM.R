texregBetterGLM <- function(
  l,
  modelNames=sapply(1:length(l),function(i)paste("Model",i)),
  labels=NA, # Custom labels for coefficients
  includeStandardBeta=TRUE, # Not currently used
  caption="Statistical models",
  hlineAfterVars=TRUE,
  Align = "c", # "c" to center column values, "S" to align values by decimal (requires latex package "siunitx")
  includeOddsRatio = rep(TRUE,length(l)) # This is the primary argument in the texregBetter shorthand; no need to mess with it here.
)
  texregBetter(
    l,
    modelNames=modelNames,
    labels=labels, # Custom labels for coefficients
    includeStandardBeta=includeStandardBeta, # Not currently used
    caption=caption,
    hlineAfterVars=hlineAfterVars,
    Align=Align,
    includeOddsRatio=includeOddsRatio
  )
