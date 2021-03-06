# This is the server logic for the 'report' Shiny web application for 'Nakke'

# load libs, scripts and data here, once
require(Nakke)
require(highcharter)

shinyServer(function(input, output) {

  # reuse server module, but with different namespaces and per report user
  # controls outside namespace (if any)
  nakkeStandardFigAndeler <-
    callModule(nakkeStandard, "figAndeler", session = getDefaultReactiveDomain(),
               valgtVar=reactive(input$andelerValgtVar)
               #enhetsUtvalg=reactive(input$andelerEnhetsUtvalg)
               )

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
    out <- nakkeStandardFigAndeler()
    return(out$plotObj)
  })
  
  output$andelerTable <- DT::renderDataTable(DT::datatable({
    out <- nakkeStandardFigAndeler()
    out$tableObj
  }, container = AndelerTableContainer(groupText = names(out$tableObj)[1],
                                       deptName = names(out$tableObj)[2]),
  rownames = FALSE,
  options = list(processing = FALSE,
                 paging = FALSE,
                 searching = FALSE)))
  
  output$downloadData <- downloadHandler(
    filename = "test.csv",
    content = function(file) {
      out <- nakkeStandardFigAndeler()
      write.table(out$tableObj, file)
    }
  )

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

  output$gjsnGrVarPlot <- renderHighchart({
    h1 <- nakkeStandardFigGjsnGrVar()
    return(h1)
  })
})
