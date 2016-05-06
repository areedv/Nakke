# This is the server logic for the 'report' Shiny web application for 'Nakke'

# load libs, scripts and data here, once

shinyServer(function(input, output) {

     ucValue <- callModule(nakkeStandard, "uc")

     output$r1Text <- renderText({
          ucValue()
     })

     output$r2Text <- renderText({
          ucValue()
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
