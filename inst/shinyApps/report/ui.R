
# This is the user-interface definition for the 'report' Shiny web application
# for 'Nakke'

require(highcharter)
data("RegData")

# not implemented just yet, wait until sample data without preprocessing
if (1==0) {
  RegData <- NakkePreprosess(RegData=RegData)
}

shinyUI(navbarPage(title = "NKR NAKKE", theme = "bootstrap.css",

  tabPanel("Andeler",
             sidebarLayout(
             sidebarPanel(
               selectInput("andelerValgtVar", "Variabel:",
                           valgtVarFigAndeler(),
                           selected = "Alder"),
               nakkeStandardInput("figAndeler"),
               downloadButton("downloadData", label = "Last ned data")
#                selectInput("andelerEnhetsUtvalg", "Enhetsutvalg:",
#                            c("Hele landet" = 0,
#                              "Egen enhet mot resten av landet" = 1,
#                              "Egen enhet" = 2),
#                            selected = 1)
             ),
             mainPanel(tabsetPanel(
               tabPanel("Figur",
                        highchartOutput("andelerPlot")
               ),
               tabPanel("Data",
                        DT::dataTableOutput("andelerTable")
               )
             ))
           )),
  tabPanel("AndelerGrVar",
           pageWithSidebar(
             headerPanel(title = h3("Brukervalg")),
             sidebarPanel(
               nakkeStandardInput("figAndelerGrVar"),
               selectInput("andelerGrVarValgtVar", "Variabel:",
                           valgtVarFigAndelerGrVar(),
                           selected = "Alder"),
               selectInput("andelerGrVarEnhetsUtvalg", "Enhetsutvalg:",
                           c("Hele landet" = 0,
                             "Egen enhet mot resten av landet" = 1,
                             "Egen enhet" = 2),
                           selected = 1)
             ),
             mainPanel(
               highchartOutput("andelerGrVarPlot")
             )
           )),
  tabPanel("AndelTid",
           pageWithSidebar(
             headerPanel(title=h3("Brukervalg")),
             sidebarPanel(
               nakkeStandardInput("figAndelTid"),
               selectInput("andelTidValgtVar", "Variabel:",
                           valgtVarFigAndelTid(),
                           selected = "Alder"),
               selectInput("andelTidEnhetsUtvalg", "Enhetsutvalg:",
                           c("Hele landet" = 0,
                             "Egen enhet mot resten av landet" = 1,
                             "Egen enhet" = 2),
                           selected = 1)
             ),
             mainPanel(
               # default output:
               #plotOutput("andelTidPlot")
               # Highcharts output:
               highchartOutput("andelTidPlot")
             )
           )),
  tabPanel("GjsnGrVar",
           pageWithSidebar(
             headerPanel(title = h3("Brukervalg")),
             sidebarPanel(
               nakkeStandardInput("figGjsnGrVar"),
               selectInput("gjsnGrVarValgtVar", "Variabel:",
                           valgtVarFigGjsnGrVar(),
                           selected = "Alder"),
               selectInput("gjsnGrVarValgtMaal", "Sentralmål:",
                           c("Gjennomsnitt" = "Gjsn",
                             "Median" = "Med"),
                           selected = "Gjsn")
             ),
             mainPanel(
               highchartOutput("gjsnGrVarPlot")
             )
           ))

)
)
