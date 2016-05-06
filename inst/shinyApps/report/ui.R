
# This is the user-interface definition for the 'report' Shiny web application
# for 'Nakke'

shinyUI(fluidPage(tabsetPanel(

     tabPanel("Report1",
              pageWithSidebar(
                   headerPanel("UI"),
                   sidebarPanel(
                        nakkeStandardInput("uc1")
                    ),
                   mainPanel(
                        textOutput("r1Text")
                   )
              )),
     tabPanel("Report2",
              pageWithSidebar(
                   headerPanel("UI"),
                   sidebarPanel(
                        nakkeStandardInput("uc2")
                   ),
                   mainPanel(
                        textOutput("r2Text")
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
