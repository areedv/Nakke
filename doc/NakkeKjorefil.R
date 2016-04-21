

#--------------------------------------SAMLERAPPORT-----------------------------------

	rm(list=ls())
	library(knitr)
	setwd('C:/Registre/.../trunk/RSamleDok')
	InfarktDataDum <- read.table('C:/Registre/Nakke/data/NakkeAlleVar2015-05-20.csv', sep=';', header=T)
	InfarktData <- InfarktDataDum	#[3000:6000, ]
#InfarktData <- read.table('C:/Registre/Hjerteinfarkt/data/Aarsrapportdata2013.csv', sep=';', header=T)
	reshID <- 104284	#106581 (Orkdal)	#104284 StOlav

	libkat <- 'C:/Registre/Rlib/trunk/'		#del av sti til bibliotekkatalog, før /lib/r/<funksjon.R>
	libkatTex <- libkat

	source("../RAndeler/InfarktFigAndeler.R", encoding="UTF-8")
	source("../RMeanMed/InfarktFigMeanMed.R", encoding="UTF-8")

	knit('InfarktSamleDok.Rnw')

#--------------------------------------------------------
#Variable som kan være greit å ha definert i grunnlagsfila:

#SlagData$TidSymptInnlegg <- as.numeric(difftime(SlagData$Innleggelsestidspunkt, SlagData$Symptomdebut,
#			units='hours'))
#SlagData <- SlagData[which(SlagData$Trombolyse %in% c(1,3)), ]
#SlagData$TidSymptTrombolyse <- as.numeric(difftime(SlagData$TrombolyseStarttid, SlagData$Symptomdebut,
#			units='hours'))
#SlagData <- SlagData[which(SlagData$Trombolyse %in% c(1,3)), ]
#SlagData$TidInnleggTrombolyse <- as.numeric(difftime(SlagData$TrombolyseStarttid,
#			SlagData$Innleggelsestidspunkt, units='hours'))
#SlagData$erMann

#------------------------------ Andeler flere var --------------------------
#------------------------------ (Fordelinger) --------------------------
rm(list=ls())
library(Nakke)
NakkeData <- read.table('C:/Registre/Nakke/data/AlleVarNum2016-01-18Staging.csv', sep=';', header=T) #Nakke18012016, AlleVarNum2016-01-04Num
RegData <- NakkeData
# Inndata til funksjon:
#...NB: SkjemaID
reshID <- 601161 #De tre med flest reg:
minald <- 0	#alder, fra og med
maxald <- 130	#alder, til og med
datoFra <- '2012-01-01'	 # min og max dato i utvalget vises alltid i figuren.
datoTil <- '2016-05-01'
erMann <- 0			#kjønn, 1-menn, 0-kvinner, standard: '' (alt annet enn 0 og 1), dvs. begge
tittel=1
enhetsUtvalg <- 0	#1-Eget sykehus mot resten (standard), 0-Hele landet, 2-Eget sykehus
valgtVar <- 'SmertestillBrukPreOp'	#Må velge... Alder, AntallNivaaOpr, Antibiotika, ArbeidstausPreOp
        #Arbeidstaus3mnd, Arbeidstaus12mnd, ASAgrad, BMI, EqAngstPreOp, ErstatningPreOp,FornoydBeh3mnd,FornoydBeh12mnd
      #Komorbiditet,Kompl3mnd, KomplOpr,LiggeDognPostop, LiggeDognTotalt, Morsmal, NytteOpr3mnd, NytteOpr12mnd,
      #OperasjonsKategori,
      #OprIndik, OprIndikPareseGrad, OprIndikMyelopati, OprIndikSmerter,Radiologi,Roker, Snuser,
      #SivilStatus, Saardren,SmertestillBrukPreOp, SymptVarighetArmer, SymptVarighetNakkeHode,
      #TidlOpr, TidlOprAntall, UforetrygdPreOp,Utdanning

outfile <- paste(valgtVar, '_ford.png', sep='')	#''	#Navn angis av Jasper
setwd("C:/ResultattjenesteGIT/Nakke/")

FigAndeler(RegData=NakkeData, datoFra=datoFra, valgtVar=valgtVar,
           datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann,
           reshID=reshID, enhetsUtvalg=enhetsUtvalg, hentData=0, outfile=outfile)



variable <- c('Alder', 'AntallNivaaOpr', 'Antibiotika', 'ArbeidstausPreOp',
              'Arbeidstaus3mnd', 'Arbeidstaus12mnd', 'ASAgrad', 'BMI', 'EqAngstPreOp', 'ErstatningPreOp',
              'FornoydBeh3mnd','FornoydBeh12mnd', 'Komorbiditet', 'Kompl3mnd', 'KomplOpr', 'LiggeDognPostop',
               'LiggeDognTotalt', 'Morsmal', 'NytteOpr3mnd', 'NytteOpr12mnd', 'OperasjonsKategori',
               'OprIndik', 'OprIndikPareseGrad', 'OprIndikMyelopati', 'OprIndikSmerter', 'Radiologi',
               'Roker', 'Snuser',
              'SivilStatus', 'Saardren', 'SmertestillBrukPreOp', 'SymptVarighetArmer', 'SymptVarighetNakkeHode',
              'TidlOpr', 'TidlOprAntall', 'UforetrygdPreOp', 'Utdanning')

for (valgtVar in variable) {
     outfile <- paste(valgtVar, '.png', sep='')
     FigAndeler(RegData=NakkeData, datoFra=datoFra, valgtVar=valgtVar,
                datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann,
                reshID=reshID, enhetsUtvalg=enhetsUtvalg, libkat=libkat, outfile=outfile)
     }


#------------------------------ Andel, utvikling over tid --------------------------
#-----------------------------------------------------------------------------------
rm(list=ls())
NakkeData <- read.table('C:/Registre/Nakke/data/AlleVarNum2016-04-13.csv', sep=';', header=T) #Nakke18012016, AlleVarNum2016-01-04Num
RegData <- NakkeData
setwd("C:/ResultattjenesteGIT//Nakke")

# Inndata til funksjon:
#...NB: SkjemaID
reshID <- 601161 #De tre med flest reg:
minald <- 0	#alder, fra og med
maxald <- 130	#alder, til og med
datoFra <- '2012-01-01'	 # min og max dato i utvalget vises alltid i figuren.
datoTil <- '2016-04-01'
erMann <- ''			#kjønn, 1-menn, 0-kvinner, standard: '' (alt annet enn 0 og 1), dvs. begge
tittel=1
enhetsUtvalg <- 1	#1-Eget sykehus mot resten (standard), 0-Hele landet, 2-Eget sykehus
valgtVar <- 'SmertestillPreOp'	#Må velge... Alder, AndreRelSykdommer, Antibiotika,
          #ArbeidstausPreOp', 'Arbeidstaus3mnd', 'Arbeidstaus12mnd, ASAgrad, BMI, ErstatningPreOp,
		  #Fornoyd12mnd, FornoydBeh3mnd,FornoydBeh12mnd, Misfor3mnd,Misfor12mnd, KomplinfekDyp3mnd,
		  #KomplinfekOverfl3mnd, KomplStemme3mnd, KomplSvelging3mnd, NytteOpr3mnd, NytteOpr12mnd
		  #Verre3mnd, Verre12mnd, OprIndikMyelopati, OprIndikSmerter, PerOpEnhverKompl, Roker, Saardren,
		  #SmertestillPreOp, SymptVarighetNakkeHode, SymptVarighetSmerterUker, UforetrygdPreOp, Utdanning

outfile <- paste(valgtVar, '.png', sep='')	#''	#Navn angis av Jasper
FigAndelTid(RegData=NakkeData, datoFra=datoFra, valgtVar=valgtVar,
           datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann,
           reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile)

variable <- c('Alder', 'AndreRelSykdommer', 'Antibiotika',
          'ArbeidstausPreOp', 'Arbeidstaus3mnd', 'Arbeidstaus12mnd', 'ASAgrad', 'BMI', 'ErstatningPreOp',
		  'FornoydBeh3mnd', 'FornoydBeh12mnd', 'Misfor3mnd', 'Misfor12mnd', 'KomplinfekDyp3mnd',
		  'KomplinfekOverfl3mnd', 'KomplStemme3mnd', 'KomplSvelging3mnd', 'NytteOpr3mnd', 'NytteOpr12mnd',
		  'Verre3mnd', 'Verre12mnd', 'OprIndikMyelopati', 'OprIndikSmerter', 'PerOpEnhverKompl', 'Roker',
		  'Saardren', 'SmertestillPreOp', 'SymptVarighetNakkeHode', 'SymptVarighetSmerterUker',
		  'UforetrygdPreOp', 'Utdanning')

for (valgtVar in variable) {
     outfile <- paste(valgtVar, '.png', sep='')
     FigAndelTid(RegData=NakkeData, datoFra=datoFra, valgtVar=valgtVar,
                datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann,
                reshID=reshID, libkat=libkat, outfile=outfile)
}


#------------------------------ Andel, per enhet --------------------------
#-----------------------------------------------------------------------------------
rm(list=ls())
NakkeData <- read.table('C:/Registre/Nakke/data/AlleVarNum2016-04-13.csv', sep=';', header=T) #Nakke18012016, AlleVarNum2016-01-04Num
RegData <- NakkeData
setwd("C:/ResultattjenesteGIT/Nakke/")

# Inndata til funksjon:
#...NB: SkjemaID
reshID <- 601161 #De tre med flest reg:
minald <- 0	#alder, fra og med
maxald <- 130	#alder, til og med
datoFra <- '2012-01-01'	 # min og max dato i utvalget vises alltid i figuren.
datoTil <- '2016-04-01'
erMann <- ''			#kjønn, 1-menn, 0-kvinner, standard: '' (alt annet enn 0 og 1), dvs. begge
libkat <- 'C:/Registre/Rlib/trunk/'		#del av sti til bibliotekkatalog, før /lib/r/<funksjon.R>
tittel=1
enhetsUtvalg <- 1	#1-Eget sykehus mot resten (standard), 0-Hele landet, 2-Eget sykehus
valgtVar <- 'NRSsmerteArmEndr12mnd'	#Må velge... Alder, AndreRelSykdommer, Antibiotika,
          #ArbeidstausPreOp', 'Arbeidstaus3mnd', 'Arbeidstaus12mnd, ASAgrad, BMI, EnhverKompl3mnd
		  #ErstatningPreOp,
		  #FornoydBeh3mnd,FornoydBeh12mnd, Misfor3mnd,Misfor12mnd, KomplinfekDyp3mnd,
		  #KomplinfekOverfl3mnd, KomplStemme3mnd, KomplSvelging3mnd, NDIendr12mnd, NytteOpr3mnd, NytteOpr12mnd
		  #NRSsmerteArmEndr12mnd,Verre3mnd, Verre12mnd, OprIndikMyelopati, Roker, Saardren,
		  #SmertestillPreOp, SymptVarighetNakkeHode, SymptVarighetSmerterUker, UforetrygdPreOp, Utdanning
setwd()
outfile <- paste(valgtVar, '_Shus.png', sep='')	#''	#Navn angis av Jasper
FigAndelerGrVar(RegData=NakkeData, datoFra=datoFra, valgtVar=valgtVar,
           datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann,
           reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile)

variable <- c('Alder', 'AndreRelSykdommer', 'Antibiotika',
          'ArbeidstausPreOp', 'Arbeidstaus3mnd', 'Arbeidstaus12mnd', 'ASAgrad', 'BMI', 'EnhverKompl3mnd', 'ErstatningPreOp',
             'FornoydBeh3mnd', 'FornoydBeh12mnd', 'Misfor3mnd', 'Misfor12mnd', 'KomplinfekDyp3mnd',
             'KomplinfekOverfl3mnd', 'KomplStemme3mnd', 'KomplSvelging3mnd', 'NDIendr12mnd', 'NytteOpr3mnd', 'NytteOpr12mnd',
             'NRSsmerteArmEndr12mnd','Verre3mnd', 'Verre12mnd', 'OprIndikMyelopati', 'Roker', 'Saardren',
             'SmertestillPreOp', 'SymptVarighetNakkeHode', 'SymptVarighetSmerterUker', 'UforetrygdPreOp', 'Utdanning')

for (valgtVar in variable) {
     outfile <- paste(valgtVar, '.png', sep='')
     FigAndelerGrVar(RegData=NakkeData, datoFra=datoFra, valgtVar=valgtVar,
                datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann,
                reshID=reshID, libkat=libkat, outfile=outfile)
}
#------------------------------ Gjennomsnitt/Median per år --------------------------
#-----------------------------------------------------------------------------------
rm(list=ls())
NakkeData <- read.table('C:/Registre/Nakke/data/AlleVarNum2016-04-13.csv', sep=';', header=T) #Nakke18012016, AlleVarNum2016-01-04Num
RegData <- NakkeData
setwd("C:/Registre/Nakke/trunk/GjsnTid")

# Inndata til funksjon:
#...NB: SkjemaID
reshID <- 601161 #De tre med flest reg:
minald <- 0	#alder, fra og med
maxald <- 130	#alder, til og med
datoFra <- '2012-01-01'	 # min og max dato i utvalget vises alltid i figuren.
datoTil <- '2016-04-01'
erMann <- ''			#kjønn, 1-menn, 0-kvinner, standard: '' (alt annet enn 0 og 1), dvs. begge
libkat <- 'C:/Registre/Rlib/trunk/'		#del av sti til bibliotekkatalog, før /lib/r/<funksjon.R>
tittel=1
enhetsUtvalg <- 1	#1-Eget sykehus mot resten (standard), 0-Hele landet, 2-Eget sykehus
valgtVar <- 'KnivtidTotalMin'	#Må velges: EMSendr12mnd, EMSendr3mnd, EQ5Dendr12mnd, EQ5Dendr3mnd, Eq5DScorePreOp,
               #KnivtidTotalMin, LiggeDognPostop, LiggeDognTotalt
               #NDIendr12mnd, NDIendr3mnd, NDIscorePreOp

outfile <- paste(valgtVar, '.png', sep='')	#''	#Navn angis av Jasper
utdata <- FigGjsnTid(RegData=NakkeData, datoFra=datoFra, valgtVar=valgtVar, valgtMaal='',
           datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann,
           reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile)

variable <- c('EMSendr12mnd', 'EMSendr3mnd', 'EQ5Dendr12mnd', 'EQ5Dendr3mnd', 'Eq5DScorePreOp',
              'KnivtidTotalMin', 'LiggeDognPostop', 'LiggeDognTotalt',
              'NDIendr12mnd', 'NDIendr3mnd', 'NDIscorePreOp')
for (valgtVar in variable) {
     outfile <- paste(valgtVar, '_GjsnTid.png', sep='')
     FigGjsnTid(RegData=NakkeData, datoFra=datoFra, valgtVar=valgtVar, valgtMaal='',
                                datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann,
                                reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile)
}



#------------------------------ Gjsn/med per enhet --------------------------
#-----------------------------------------------------------------------------------
rm(list=ls())
NakkeData <- read.table('C:/Registre/Nakke/data/AlleVarNum2016-04-13.csv', sep=';', header=T) #Nakke18012016, AlleVarNum2016-01-04Num
RegData <- NakkeData
# Inndata til funksjon:
#...NB: SkjemaID
reshID <- 601161 #De tre med flest reg:
minald <- 0	#alder, fra og med
maxald <- 130	#alder, til og med
datoFra <- '2012-01-01'	 # min og max dato i utvalget vises alltid i figuren.
datoTil <- '2016-06-01'
erMann <- ''			#kjønn, 1-menn, 0-kvinner, standard: '' (alt annet enn 0 og 1), dvs. begge
tittel=1
valgtVar <- 'NRSsmerteArmPreOp'	#Må velge... Alder, EMSscorePreOp, LiggeDognPostop,KnivtidTotalMin, LiggeDognTotalt,
          #NDIscorePreOp, NRSsmerteArmPreOp, NRSsmerteNakkePreOp

outfile <- paste(valgtVar, '_res72.pdf', sep='')	#''	#Navn angis av Jasper

FigGjsnGrVar(RegData=NakkeData, datoFra=datoFra, valgtVar=valgtVar,
            datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann,
            reshID=reshID, outfile=outfile)


variable <- c('Alder', 'EMSscorePreOp', 'LiggeDognPostop','KnivtidTotalMin', 'LiggeDognTotalt',
          'NDIscorePreOp', 'NRSsmerteArmPreOp', 'NRSsmerteNakkePreOp')
for (valgtVar in variable) {
     outfile <- paste(valgtVar, '.png', sep='')
     FigGjsnGrVar(RegData=NakkeData, datoFra=datoFra, valgtVar=valgtVar,
                datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann,
                reshID=reshID, outfile=outfile)
}













