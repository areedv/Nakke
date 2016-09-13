#' Make html table container for DT used in Shiny
#' 
#' Yes, make it!
#' 
#' @param groupText String General name of grouping
#' @param deptName String Name of department where data belongs
#' @export 

AndelerTableContainer <- function(groupText = "Gruppe", deptName = "ShusAvd") {
  
  andelerTableContainer <- htmltools::withTags(table(
    class = "display",
    thead(
      tr(
        th(rowspan = 2, style = "text-align:left", groupText),
        th(colspan = 2, style = "text-align:center", deptName),
        th(colspan = 2, style = "text-align:center", "Landet forøvrig")
      ),
      tr(
        lapply(rep(c("Andel (%)", "N"), 2), th) 
      )
    )
  ))
  return(andelerTableContainer)
}