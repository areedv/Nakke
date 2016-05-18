#' Server logic function for skiny Nakke
#'
#' nakkeStandard is the module server function for shiny Nakke
#'
#' @param input
#' @param output
#' @param session
#' @export

nakkeStandard <- function(input, output, session, reportName) {
     #erMann <- reactive({
     #     input$erMann
     #})
     #alder <- reactive({
     #     input$alder
     #})
     #periode <- reactive({
     #     input$periode
     #})
     # for checking which report, can session be used to grab id?
     # test with explicit naming first:
     if (reportName == "report1") {
          #get_uc_outside_namespace_from_additional_args
          plotObj <- reactive({
               FigAndeler(RegData=RegData,
                          valgtVar = "Alder",
                          erMann=as.numeric(input$erMann),
                          minald = as.numeric(input$alder[1]),
                          maxald = as.numeric(input$alder[2]),
                          reshID = 103469)
          })
     }
     if (reportName == "report2") {
          #get_uc_outside_namespace_from_additional_args
          plotObj <- reactive({
               FigAndeler(RegData=RegData,
                          valgtVar = "Alder",
                          erMann=as.numeric(input$erMann),
                          minald = as.numeric(input$alder[1]),
                          maxald = as.numeric(input$alder[2]),
                          reshID = 103469)
          })
     }
     return(plotObj)
}
