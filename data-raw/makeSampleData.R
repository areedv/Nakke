#' makeSampleData creates and (optionally) stores samle data in the package
#'
#' To be used to create and/or update sample data in the package
#'
#' @param realRegData Data frame to make sample data from
#' @param saveSampleDataInPackage Logical if TRUE will overwrite any existing
#' @return sampleRegData Data frame of sample data

makeSampleData <- function(realRegData, saveSampleDataInPackage) {

  # synpop or whatever method to flip and turn orignal data
  sampleRegData <- realRegData

  # make fake hospital/dept names consistent with reshIDs
  sampleRegData$Avdeling <- NULL
  sampleRegData$Avdeling <- rep("navnløs", length(sampleRegData$AVD_RESH))
  depts <- levels(factor(sampleRegData$AVD_RESH))
  baseName <- rep("shusAvd", length(depts))
  ownName <- paste0(baseName, LETTERS[1:length(depts)])
  for (i in 1:length(depts)) {
    ind <- which(sampleRegData$AVD_RESH == depts[i])
    sampleRegData$Avdeling[ind] <- ownName[i]
  }

  # save data if requested
  if (saveSampleDataInPackage) {
    RegData <- sampleRegData
    devtools::use_data(RegData, RegData, overwrite = saveSampleDataInPackage)
  }

  return(sampleRegData)
}
