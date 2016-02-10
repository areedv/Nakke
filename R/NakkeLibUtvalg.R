#' Funksjon som gjør utvalg av registreringene for Nakke
#'
#'Funksjon som gjør utvalg av dataene, returnerer det reduserte datasettet og utvalgsteksten.
#' Inndata:
#' @param erMann - kjønn, 1-menn, 0-kvinner, standard: '' (alt annet enn 0 og 1), dvs. begge
#' @param minald - alder, fra og med
#' @param maxald - alder, til og med
#' @param datoFra - Operasjonsdato, fra og med. Standard: '2012-01-01'   
#' @param datoTil - Operasjonsdato, til og med. Standard: '3000-01-01' (siste registreringsdato)

NakkeLibUtvalg <- function(RegData, datoFra, datoTil, minald=0, maxald=130, erMann='',
		tidlOp='', fargepalett='BlaaOff')	#insttype, 
{

#Definerer registerspesifikke variable. Dette må gjøres i funksjonen som henter data
RegData$erMann <- 0
RegData$erMann[RegData$Kjonn=='Mann'] <- 1
RegData$InnDato <- as.POSIXlt(RegData$OprDato, format="%Y-%m-%d")	#"%Y-%m-%d") # %H:%M:%S" )	#"%d.%m.%Y"	"%Y-%m-%d"
names(RegData)[which(names(RegData) == 'AVD_RESH')] <- 'ReshId'
class(RegData$ReshId) <- 'numeric'
RegData$Aar <- 1900 + strptime(RegData$InnDato, format="%Y")$year


#Utvalg..........
RegData <- RegData[which(RegData$LegeskjemaStatus == 1), ]  #Vi ønsker kun ferdigstilte legeskjema

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