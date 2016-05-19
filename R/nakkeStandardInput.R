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
                label=h3("Kjønn:"),
                c("Begge"=2, "Menn"=1, "Kvinner"=0)
    ),
    sliderInput(ns("alder"), label = h3("Alder"), min = 0,
                max = 130, value = c(0, 130)
    ),
    dateRangeInput(ns("periode"), label = h3("Periode"),
                   separator="til", language="nb")
  )
}
