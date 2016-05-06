#' Modulised UI for shiny
#'
#' nakkeStandardInput provides common ui uc for shiny Nakke
#'
#'  @param id namespace for this module
#'  @param label Label for this module
#'  @export

nakkeStandardInput <- function(id, label = "Brukervalg") {

     # create namespace
     ns <- NS(id)

     tagList(
          selectInput(ns("erMann"),
                         "Kjønn:",
                         c("Begge"=2, "Menn"=1, "Kvinner"=0)
          )
     )
}
