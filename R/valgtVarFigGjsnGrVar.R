#' valgtVarFigGjsnGrVar provides a list of registry vars for the report
#'
#' The list of possible variables to be used in the report type
#' \emph{figGjsnGrVar}. Placed as its own function to trim the shiny app for
#' Nakke
#'
#' @return valgtVarListFigGjsnGrVar list of names and values
#' @export

valgtVarFigGjsnGrVar <- function() {

  valgtVarListFigGjsnGrVar <-
    list("Aldersfordeling"="Alder",
         "Antallnivåoperert"="AntallNivaaOpr",
         "Erdetgittantibiotikaprofylakse"="Antibiotika",
         "Arbeidsstatus12mndetteroperasjon"="Arbeidstaus12mnd",
         "Arbeidsstatus3mndetteroperasjon"="Arbeidstaus3mnd",
         "Arbeidsstatusføroperasjon"="ArbeidstausPreOp",
         "ASAgrad"="ASAgrad",
         "PasientenesBMIBodyMassIndex"="BMI",
         "HelsetilstandAngst"="EqAngstPreOp",
         "Søkterstatning"="ErstatningPreOp",
         "Fornøydhetmedbehandlingapåsykehuset12mnd"="FornoydBeh12mnd",
         "Fornøydhetmedbehandlingapåsykehuset3mnd"="FornoydBeh3mnd",
         "Hastegrad"="OperasjonsKategori",
         "Antallliggedøgnpostoperativt"="LiggeDognPostop",
         "Totaltantallliggedøgn"="LiggeDognTotalt",
         "Morsmål"="Morsmal",
         "Nytteavoperasjon12mnd"="NytteOpr12mnd",
         "Nytteavoperasjon3mnd"="NytteOpr3mnd",
         "Paresegradføroperasjon"="OprIndikPareseGrad",
         "Røykerpasienten"="Roker",
         "Harpasientenfåttsårdren"="Saardren",
         "Sivilstatus"="SivilStatus",
         "Hyppighetavsmertestillendeføroperasjonen"="SmertestillBrukPreOp",
         "Snuserpasienten"="Snuser",
         "Varighetavutstrålendearmsmerter"="SymptVarighetArmer",
         "Varighetavnakkehodesmerter"="SymptVarighetNakkeHode",
         "Erpasiententidligereoperert"="TidlOpr",
         "Antalltidligereoperasjoner"="TidlOprAntall",
         "Søktuføretrygd"="UforetrygdPreOp",
         "Utdanningsnivå"="Utdanning")

  return(valgtVarListFigGjsnGrVar)
}
