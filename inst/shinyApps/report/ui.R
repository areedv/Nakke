
# This is the user-interface definition for the 'report' Shiny web application
# for 'Nakke'

shinyUI(fluidPage(tabsetPanel(

  tabPanel("Andeler",
           pageWithSidebar(
             headerPanel(title=h3("Brukervalg")),
             sidebarPanel(
               nakkeStandardInput("figAndeler"),
               selectInput("andelerValgtVar", "Variabel:",
                           valgtVarFigAndeler(),
                           selected = "Alder"),
               selectInput("andelerEnhetsUtvalg", "Enhetsutvalg:",
                           c("Hele landet" = 0,
                             "Egen enhet mot resten av landet" = 1,
                             "Egen enhet" = 2),
                           selected = 1)
             ),
             mainPanel(
               plotOutput("andelerPlot")
             )
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
               plotOutput("andelerGrVarPlot")
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
               plotOutput("andelTidPlot")
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
