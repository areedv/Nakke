#' Server logic function for skiny Nakke
#'
#' nakkeStandard is the module server function for shiny Nakke
#'
#' @param input
#' @param output
#' @param session
#' @export

nakkeStandard <- function(input, output, session) {
     erMann <- reactive({
          input$erMann
     })
     alder <- reactive({
          input$alder
     })
     periode <- reactive({
          input$periode
     })
     ucValues <- list(erMann=erMann, alder=alder, periode=periode)
     return(erMann)
}
