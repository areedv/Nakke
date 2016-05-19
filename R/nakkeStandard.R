#' Server logic function for skiny Nakke
#'
#' nakkeStandard is the module server function for shiny Nakke
#'
#' @param input
#' @param output
#' @param session
#' @export

nakkeStandard <- function(input, output, session) {
     sessionName <- session$ns("name")
     # namespace id comes with an extra '-name'. Remove it
     sessionName <- gsub("-name", "", sessionName)
     if (sessionName == "figAndeler") {
          #get_uc_outside_namespace_from_additional_args
          plotObj <- reactive({
               FigAndeler(RegData=RegData,
                          valgtVar = "Alder",
                          erMann=as.numeric(input$erMann),
                          minald = as.numeric(input$alder[1]),
                          maxald = as.numeric(input$alder[2]),
                          reshID = as.numeric(input$avdeling)
               )
          })
     }
     if (sessionName == "report2") {
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
