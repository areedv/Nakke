#' Preprosesser data fra Degenerativ Nakke
#'
#' Denne funksjonen definerer og formaterer variabler 
#'
#' @inheritParams FigAndeler
#'
#' @return RegData En dataramme med det preprosesserte datasettet
#'
#' @export
NGERPreprosess <- function(RegData=RegData)
{
  #Kun ferdigstilte registreringer:
	RegData <- RegData[which(RegData$LegeskjemaStatus == 1), ]  #Vi ønsker kun ferdigstilte legeskjema
	#Kjønnsvariabel:
	RegData$erMann <- 0
	RegData$erMann[RegData$Kjonn=='Mann'] <- 1
	#Riktig datoformat og hoveddato
	RegData$InnDato <- as.POSIXlt(RegData$OprDato, format="%Y-%m-%d")	
	RegData$Aar <- 1900 + strptime(RegData$InnDato, format="%Y")$year
	#Variabel som identifiserer avdelingas resh
	names(RegData)[which(names(RegData) == 'AVD_RESH')] <- 'ReshId'
	class(RegData$ReshId) <- 'numeric'


  return(invisible(RegData))
}
