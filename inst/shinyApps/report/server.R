# This is the server logic for the 'report' Shiny web application for 'Nakke'

# load libs, scripts and data here, once

shinyServer(function(input, output) {

  # reuse server module, but with different namespaces
  nakkeStandardFigAndeler <- callModule(nakkeStandard, "figAndeler",
                                        reactive(input$andelerValgtVar),
                                        reactive(input$enhetsUtvalg))
  nakkeStandardFigAndelerGrVar <- callModule(nakkeStandard, "figAndelerGrVar")

  output$andelerPlot <- renderPlot({
    nakkeStandardFigAndeler()
  })

  output$andelerGrVarPlot <- renderPlot({
    nakkeStandardFigAndelerGrVar()
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
