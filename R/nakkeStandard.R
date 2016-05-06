#' Server logic function for skiny Nakke
#'
#' nakkeStandard is the module server function for shiny Nakke
#'
#' @param input
#' @param output
#' @param session
#' @export

nakkeStandard <- function(input, output, session) {
     ucValue <- reactive({
          input$erMann
     })
     return(ucValue)
}
