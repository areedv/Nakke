# This is the server logic for the 'report' Shiny web application for 'Nakke'

# load libs, scripts and data here, once

shinyServer(function(input, output) {

     output$r1Text <- renderText({
          "Report1"
     })

     output$r12Text <- renderText({
          "Report2"
     })

     output$r3Text <- renderText({
          "Report3"
     })

     output$r4Text <- renderText({
          "Report4"
     })

     output$r5Text <- renderText({
          "Report5"
     })
})
