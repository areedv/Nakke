
# This is the user-interface definition for the 'report' Shiny web application
# for 'Nakke'

shinyUI(fluidPage(tabsetPanel(

  tabPanel("Andeler",
           pageWithSidebar(
             headerPanel("Brukervalg"),
             sidebarPanel(
               nakkeStandardInput("figAndeler"),
               selectInput("andelerValgtVar", "Variabel:",
                           c("Aldersfordeling"="Alder",
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
                             "Utdanningsnivå"="Utdanning"),
                           selected = "Alder")
             ),
             mainPanel(
               plotOutput("report1Plot")
             )
           )),
  tabPanel("Report2",
           pageWithSidebar(
             headerPanel("UI"),
             sidebarPanel(
               nakkeStandardInput("report2")
             ),
             mainPanel(
               plotOutput("report2Plot")
             )
           )),
  tabPanel("Report3",
           pageWithSidebar(
             headerPanel("UI"),
             sidebarPanel(
               selectInput("erMann",
                           "Kjønn:",
                           c("Begge"=2, "Menn"=1, "Kvinner"=0)
               )
             ),
             mainPanel(
               textOutput("r3Text")
             )
           )),
  tabPanel("Report4",
           pageWithSidebar(
             headerPanel("UI"),
             sidebarPanel(
               selectInput("erMann",
                           "Kjønn:",
                           c("Begge"=2, "Menn"=1, "Kvinner"=0)
               )
             ),
             mainPanel(
               textOutput("r4Text")
             )
           )),
  tabPanel("Report5",
           pageWithSidebar(
             headerPanel("UI"),
             sidebarPanel(
               selectInput("erMann",
                           "Kjønn:",
                           c("Begge"=2, "Menn"=1, "Kvinner"=0)
               )
             ),
             mainPanel(
               textOutput("r5Text")
             )
           ))
)
)
)
