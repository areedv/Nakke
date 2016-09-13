

AndelerTableContainer <- function(groupText = "Gruppe", deptName = "ShusAvd") {
  
  andelerTableContainer <- htmltools::withTags(table(
    class <- "display",
    thead(
      tr(
        th(rowspan = 2, groupText),
        th(rowspan = 2, deptName),
        th(rowspan = 2, "Landet forøvrig")
      ),
      tr(
        lapply(rep(c("Andel (%)", "N"), 2), th) 
      )
    )
  ))
  print(andelerTableContainer)
}