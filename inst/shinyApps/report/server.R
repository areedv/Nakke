# This is the server logic for the 'report' Shiny web application for 'Nakke'

# load libs, scripts and data here, once

shinyServer(function(input, output) {

  # reuse server module, but with different namespaces
  nakkeStandardFigAndeler <- callModule(nakkeStandard, "figAndeler",
                                        reactive(input$andelerValgtVar),
                                        reactive(input$enhetsUtvalg))
  nakkeStandard2 <- callModule(nakkeStandard, "report2")

  output$report1Plot <- renderPlot({
    nakkeStandardFigAndeler()
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
