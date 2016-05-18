# This is the server logic for the 'report' Shiny web application for 'Nakke'

# load libs, scripts and data here, once

shinyServer(function(input, output) {

     nakkeStandard1 <- callModule(nakkeStandard, "uc1", reportName="report1")
     nakkeStandard2 <- callModule(nakkeStandard, "uc2", reportName="report2")

     output$report1Plot <- renderPlot({
          nakkeStandard1()
     })

     output$report2Plot <- renderPlot({
          nakkeStandard2()
     })

     output$r3Text <- renderText({
          input$erMann
     })

     output$r4Text <- renderText({
          input$erMann
     })

     output$r5Text <- renderText({
          input$erMann
     })
})
