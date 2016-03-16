#' Funksjon som gjør utvalg i datagrunnlaget til registreringene for Nakke
#'
#' Funksjon som gjør utvalg av dataene, returnerer det reduserte datasettet og utvalgsteksten.
#' 
#' @inheritParams FigAndeler
#' @param fargepalett: Velge fargepalett, standard:BlaaOff ("offentliggjøringsfargene")
#'
#' @export

NakkeLibUtvalg <- function(RegData, datoFra, datoTil, minald=0, maxald=130, erMann='',
		tidlOp='', fargepalett='BlaaOff')	#insttype, 
{


Ninn <- dim(RegData)[1]
indAld <- which(RegData$Alder >= minald & RegData$Alder <= maxald)
indDato <- which(RegData$InnDato >= as.POSIXlt(datoFra) & RegData$InnDato <= as.POSIXlt(datoTil))
indKj <- if (erMann %in% 0:1) {which(RegData$erMann == erMann)} else {indKj <- 1:Ninn}
#indTidlOp <- if (tidlOp %in% 1:4) {which(RegData$TidlOpr==tidlOp)} else {indTidlOp <- 1:Ninn}
indMed <- intersect(indAld, intersect(indDato, indKj))
RegData <- RegData[indMed,]


TidlOprtxt <-	c('Tidl. operert samme nivå', 'Tidl. operert annet nivå', 'Tidl. operert annet og sm. nivå', 'Primæroperasjon')

N <- dim(RegData)[1]

utvalgTxt <- c(paste('Operasjonsdato: ', if (N>0) {min(RegData$InnDato, na.rm=T)} else {datoFra}, 
			' til ', if (N>0) {max(RegData$InnDato, na.rm=T)} else {datoTil}, sep='' ),
	if ((minald>0) | (maxald<130)) {paste('Pasienter fra ', if (N>0) {min(RegData$Alder, na.rm=T)} else {minald}, 
						' til ', if (N>0) {max(RegData$Alder, na.rm=T)} else {maxald}, ' år', sep='')},
	if (erMann %in% 0:1) {paste('Kjønn: ', c('Kvinner', 'Menn')[erMann+1], sep='')}
#	if (tidlOp %in% 1:4) {TidlOprtxt[tidlOp]}
	)
	

UtData <- list(RegData=RegData, utvalgTxt=utvalgTxt, fargepalett=fargepalett) #GronnHNpms624,
return(invisible(UtData)) 
}