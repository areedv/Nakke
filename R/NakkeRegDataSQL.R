#' Henter data registrert for Degenerativ Nakke
#'
#' Henter data for Degenerativ Nakke fra "staging"
#'
#' @inheritParams FigAndeler
#'
#' @return Henter dataramma RegData for Degenerativ Nakke
#'
#' @export
#'
NakkeRegDataSQL <- function(datoFra = '2012-01-01', datoTil = '2099-01-01') {

  registryName <- "Nakke"
  dbType <- "mysql"

  query <- paste0('SELECT
	Alder,
	AndreRelSykdommer,
	AntallNivaaOpr,
	AntBarn,
	AntBarnPas,
	Antibiotika,
	Arbeidstaus12mnd,
	Arbeidstaus3mnd,
	ArbeidstausPreOp,
	ASAgrad,
	AVD_RESH,
	Avdeling,
	BMI,
	BMIkategori,
	CentreID12mnd,
	CentreID3mnd,
	CentreIdPas,
	Dagkirurgi,
	DelvisSykemeldtPros12mnd,
	DelvisSykemeldtPros3mnd,
	DelvisSykemeldtProsPreOp,
	EMSscore12mnd,
	EMSscore3mnd,
	EMSscorePreOp,
	EnhverKompl12mnd,
	EnhverKompl3mnd,
	Eq5DScore12mnd,
	Eq5DScore3mnd,
	Eq5DScorePreOp,
	Erstatning12mnd,
	Erstatning3mnd,
	ErstatningPreOp,
	ForlopsID,
	Helsetilst12mnd,
	Helsetilst3mnd,
	HelsetilstPreOp,
	HelsetilstPreOpMissing,
	Hoyde,
	HoydeMissing,
	InngrepType,
	Kjonn,
	KnivtidSluttMin,
	KnivtidSluttTimer,
	KnivtidStartMin,
	KnivtidStartTimer,
	KnivtidTotalMin,
	KomplDVT12mnd,
	KomplDVT3mnd,
	KomplinfekDyp12mnd,
	KomplinfekDyp3mnd,
	KomplinfekOverfl12mnd,
	KomplinfekOverfl3mnd,
	KomplKraftsvikt12mnd,
	KomplKraftsvikt3mnd,
	KomplLungeEmboli12mnd,
	KomplLungeEmboli3mnd,
	KomplPneumoni12mnd,
	KomplPneumoni3mnd,
	KomplStemme12mnd,
	KomplStemme3mnd,
	KomplSvelging12mnd,
	KomplSvelging3mnd,
	KomplUVI12mnd,
	KomplUVI3mnd,
	LegeskjemaStatus,
	LiggeDognPostop,
	LiggeDognTotalt,
	MceCentreID,
	Morsmal,
	NDIscore12mnd,
	NDIscore3mnd,
	NDIscorePreOp,
	NytteOpr12mnd,
	NytteOpr3mnd,
	OperasjonsKategori,
	OppFolgStatus12mnd,
	OppFolgStatus3mnd,
	OprDato AS InnDato,
	OprIndikAnnet,
	OprIndikasjon,
	OprIndikasjonUtfylt,
	OprIndikMyelopati,
	OprIndikParese,
	OprIndikPareseGrad,
	OprIndikSmerteLokArm,
	OprIndikSmerteLokNakke,
	OprIndikSmerter,
	OprKode,
	ORG_RESH,
	Organisasjon,
	Parese12mnd,
	Parese3mnd,
	ParesePreOp,
	PareseVarighet,
	PasientID,
	PasientSkjemaStatus,
	PerOpEnhverKompl,
	RadiologiCt,
	RadiologiMr,
	RadiologiMyelografi,
	RadiologiRtgCcol,
	RadiologiRtgCcolFunkOpptak,
	RadiologiUndersokelseUtfylt,
	Reopr90d,
	RHF,
	RHF_RESH,
	Roker,
	RokerPas,
	RtgFunnANNET,
	Saardren,
	SivilStatus,
	SivilStatusPas,
	Snuser,
	SnuserPas,
	StatusKtr12mnd,
	StatusKtr3mnd,
	SykehusNavn,
	SymptVarighetArmer,
	SymptVarighetNakkeHode,
	SymptVarighetSmerterUker,
	TidlOpr,
	TidlOprAnnetNiv,
	TidlOprAntall,
	TidlOprNei,
	TidlOprSammeNiv,
	Uforetrygd12mnd,
	Uforetrygd3mnd,
	UforetrygdPreOp,
	UforeTrygdPros3mnd,
	UforeTrygdProsPreOp,
	Utdanning,
	UtdanningPas,
	UtDato,
	VarighetSykeMeld12mnd,
	VarighetSykeMeld3mnd,
	Vekt,
	VektMissing
                  WHERE InnDato >= \'', datoFra, '\' AND InnDato <= \'', datoTil, '\'')

RegData <- rapbase::LoadRegData(registryName, query, dbType)

#FROM AlleVarNum INNER JOIN ForlopsOversikt ON AlleVarNum.MCEID = ForlopsOversikt.ForlopsID


#	AntibiotikaAntDogn
#	AntibiotikaDose
#	AntibiotikaDoseAntall
#	AntibiotikaIntEvtAntDogn
#	AntibiotikaIntKunOprDag
#	AntibiotikaMedikament
#	BakreFusjonDistaltNiv
#	BakreFusjonProximaltNiv
#	BakreFusjonSkruer
#	BakreFusjonStag
#	BakreFusjonWire
#	BenGraftAutograft
#	BenGraftBankben
#	BenGraftBensubstitutt
#	DodsDato
#	DodsfallOpphold
#	DodsfallOppholdAnnetSpes
#	EMSblareTarm12mnd
#	EMSblareTarm3mnd
#	EMSblareTarmPreOp
#	EMSgangeFunk12mnd
#	EMSgangeFunk3mnd
#	EMSgangeFunkPreOp
#	EMShandFunk12mnd
#	EMShandFunk3mnd
#	EMShandFunkPreOp
#	EMSkoordinasjon12mnd
#	EMSkoordinasjon3mnd
#	EMSkoordinasjonPreOp
#	EMSnummenhet12mnd
#	EMSnummenhet3mnd
#	EMSnummenhetPreOp
#	EqAngst12mnd
#	EqAngst3mnd
#	EqAngstPreOp
#	EqGange12mnd
#	EqGange3mnd
#	EqGangePreOp
#	EqPersStell12mnd
#	EqPersStell3mnd
#	EqPersStellPreOp
#	EqSmerte12mnd
#	EqSmerte3mnd
#	EqSmertePreOp
#	EqVanlGjMaal12mnd
#	EqVanlGjMaal3mnd
#	EqVanlGjMaalPreOp
#	ForlopsLaget
#	ForlopsLagetAv
#	ForlopsOppdaterAv
#	ForlopsOppdatert
#	FornoydBeh12mnd
#	FornoydBeh3mnd
#	FriskmeldtDato12mnd
#	FriskmeldtDato3mnd
#	NDIarbeid12mnd
#	NDIarbeid3mnd
#	NDIarbeidPreOp
#	NDIbilkjoring12mnd
#	NDIbilkjoring3mnd
#	NDIbilkjoringPreOp
#	NDIfritid12mnd
#	NDIfritid3mnd
#	NDIfritidPreOp
#	NDIhodepineE12mnd
#	NDIhodepineE3mnd
#	NDIhodepinePreOp
#	NDIkonsentrasjon12mnd
#	NDIkonsentrasjon3mnd
#	NDIkonsentrasjonPreOp
#	NDIlesing12mnd
#	NDIlesing3mnd
#	NDIlesingPreOp
#	NDIlofting12mnd
#	NDIlofting3mnd
#	NDIloftingPreOp
#	NDIpersStell12mnd
#	NDIpersStell3mnd
#	NDIpersStellPreOp
#	NDIsmerte12mnd
#	NDIsmerte3mnd
#	NDIsmertePreOp
#	NDIsovn12mnd
#	NDIsovn3mnd
#	NDIsovnPreOp
#	NRSsmerteArm12mnd
#	NRSsmerteArm3mnd
#	NRSsmerteArmMissingPreOp
#	NRSsmerteArmPreOp
#	NRSsmerteHodet12mnd
#	NRSsmerteHodet3mnd
#	NRSsmerteHodetMissingPreOp
#	NRSsmerteHodetPreOp
#	NRSsmerteNakke12mnd
#	NRSsmerteNakke3mnd
#	NRSsmerteNakkeMissingPreOp
#	NRSsmerteNakkePreOp
#	OppFolgLaget12mnd
#	OppFolgLaget3mnd
#	OppFolgLagetAv12mnd
#	OppFolgLagetAv3mnd
#	OppFolgOppdatert12mnd
#	OppFolgOppdatert3mnd
#	OppFolgOppdatertAv12mnd
#	OppFolgOppdatertAv3mnd
#	OprIndikMyelopatiMotorisk
#	OprIndikMyelopatiSensorisk
#	OprMetodeAndre
#	OprMetodeAnnenBakreDekompr
#	OprMetodeBakreFusjon
#	OprMetodeDiskektomi
#	OprMetodeDiskektomiBenblokk
#	OprMetodeDiskektomiCage
#	OprMetodeDiskektomiPlate
#	OprMetodeDiskektomiSkiveprotese
#	OprMetodeForamenotomiBakreBiLat
#	OprMetodeForamenotomiBakreUniLat
#	OprMetodeKirDekompresjon
#	OprMetodeKorpektomi
#	OprMetodeKorpektomiBenblokk
#	OprMetodeKorpektomiBur
#	OprMetodeKorpektomiPlate
#	OprMetodeMikroMakroEndo
#	OprMetodeTilgangBakre
#	OprMetodeTilgangFremre
#	OprMetodeTilgangFremreH
#	OprMetodeTilgangFremreV
#	PareseDager
#	PareseTimer
#	PareseUker
#	PasientDod
#	PasLaget
#	PasLagetAv
#	PasOppdatert
#	PasOppdatertAv
#	PatSkjemaLaget
#	PatSkjemaLagetAv
#	PatSkjemaOppdatert
#	PatSkjemaOppdatertAv
#	PerOpKomplAnafylaksiI
#	PerOpKomplAnnenNerveskade
#	PerOpKomplAnnet
#	PerOpKomplBlodning
#	PerOpKomplDurarift
#	PerOpKomplFeilplasseringImplant
#	PerOpKomplKardioVaskulare
#	PerOpKomplMedullaskade
#	PerOpKomplNerverotSkade
#	PerOpKomplOpFeilNivaa
#	PerOpKomplOsofagusSkade
#	PerOpKomplRespiratorisk
#	PerOpKomplSkadeStoreBlodkar
#	PostnrPas
#	PoststedPas
#	RanawatKlassifikasjon
#	ReoprInnen90DagerUfylt
#	RtgFunnCervicalSpStenose
#	RtgFunnDegnerasjonNakke
#	RtgFunnIntrMedHoysingnalMR
#	RtgFunnNormal
#	RtgFunnProlaps
#	RtgFunnRotkanalstenose
#	RtgFunnSpondylolistese
#	RtgFunnUtfylt
#	SamtykkeStatus
#	SideNivaaC0_C1
#	SideNivaaC0C1H
#	SideNivaaC0C1V
#	SideNivaaC1C2
#	SideNivaaC1C2H
#	SideNivaaC1C2V
#	SideNivaaC2C3
#	SideNivaaC2C3H
#	SideNivaaC2C3V
#	SideNivaaC3C4
#	SideNivaaC3C4H
#	SideNivaaC3C4V
#	SideNivaaC4C5
#	SideNivaaC4C5H
#	SideNivaaC4C5V
#	SideNivaaC5C6
#	SideNivaaC5C6H
#	SideNivaaC5C6V
#	SideNivaaC6C7
#	SideNivaaC6C7H
#	SideNivaaC6C7V
#	SideNivaaC7TH1
#	SideNivaaC7TH1H
#	SideNivaaC7TH1V
#	SmerteLokOverEks12mnd
#	SmerteLokOverEks3mnd
#	SmerteLokOverEksPreOp
#	SmerteLokSide12mnd
#	SmerteLokSide3mnd
#	SmerteLokSidePreOp
#	Smertestill12mnd
#	Smertestill3mnd
#	SmertestillBruk12mnd
#	SmertestillBruk3mnd
#	SmertestillBrukPreOp
#	SmertestillPreOp
#	SykdAnnenendokrin
#	SykdAnnenreumatisk
#	SykdAnnet
#	SykdBechtrew
#	SykdCarpalTunnelSyndr
#	SykdCerebrovaskular
#	SykdDepresjonAngst
#	SykdHjertekar
#	SykdHodepine
#	SykdHypertensjon
#	SykDiabetesMellitus
#	SykdImmunSuprBeh
#	SykdKreft
#	SykdKroniskLunge
#	SykdKroniskNevrologisk
#	SykdKrSmerterMuskelSkjelSyst
#	SykdOsteoporose
#	SykdReumatoidartritt
#	SykdSkulderImpigment
#	SykdVaskularClaudicatio
#	SykdWhiplashNakke
#	TidlSkulderPlager12mnd
#	TidlSkulderPlager3mnd
#	TidlSkulderPlagerPreOp
#	UforeTrygdPros12mnd
#	UsEmgNevrografi
#	UsRotblokkade
#	UtfyltDato12mnd
#	UtfyltDato3mnd
#	UtfyltDatoLegeskjema
#	UtfyltDatoPas
#	YrkeFysiskeKrav



return(RegData)
}



