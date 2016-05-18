#' Server logic function for skiny Nakke
#'
#' nakkeStandard is the module server function for shiny Nakke
#'
#' @param input
#' @param output
#' @param session
#' @export

nakkeStandard <- function(input, output, session, reportName, ...) {
     erMann <- reactive({
          input$erMann
     })
     alder <- reactive({
          input$alder
     })
     periode <- reactive({
          input$periode
     })
     # for checking which report, can session be used to grab id?
     # test with explicit naming first:
     if (reportName == "report1") {
          get_uc_outside_namespace_from_additional_args
          output$reportPlot <- renderPlot({
               call_fig_function
          })

     }

}
