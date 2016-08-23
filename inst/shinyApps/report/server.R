# This is the server logic for the 'report' Shiny web application for 'Nakke'

# load libs, scripts and data here, once
require(rCharts)
require(highcharter)
shinyServer(function(input, output) {

  # reuse server module, but with different namespaces and per report user
  # controls outside namespace (if any)
  nakkeStandardFigAndeler <-
    callModule(nakkeStandard, "figAndeler", session = getDefaultReactiveDomain(),
               valgtVar=reactive(input$andelerValgtVar),
               enhetsUtvalg=reactive(input$andelerEnhetsUtvalg))

  nakkeStandardFigAndelerGrVar <-
    callModule(nakkeStandard, "figAndelerGrVar",
               valgtVar=reactive(input$andelerGrVarValgtVar),
               enhetsUtvalg=reactive(input$andelerGrVarEnhetsUtvalg))

  nakkeStandardFigAndelTid <-
    callModule(nakkeStandard, "figAndelTid",
               valgtVar=reactive(input$andelTidValgtVar),
               enhetsUtvalg=reactive(input$andelTidEnhetsUtvalg))

  nakkeStandardFigGjsnGrVar <-
    callModule(nakkeStandard, "figGjsnGrVar",
               valgtVar=reactive(input$gjsnGrVarValgtVar),
               valgtMaal=reactive(input$gjsnGrVarValgtMaal))


  output$andelerPlot <- renderHighchart({
    h1 <- nakkeStandardFigAndeler()
    return(h1)
  })

  output$andelerGrVarPlot <- renderHighchart({
    h1 <- nakkeStandardFigAndelerGrVar()
    return(h1)
  })

  # default function statement:
  #output$andelTidPlot <- renderPlot({
  # Highchart function statement:
  output$andelTidPlot <- renderHighchart({
    # default call:
    #nakkeStandardFigAndelTid()
    # Highcahrts call:
    h1 <- nakkeStandardFigAndelTid()
    return(h1)
  })

  output$gjsnGrVarPlot <- renderPlot({
    nakkeStandardFigGjsnGrVar()
  })
})
