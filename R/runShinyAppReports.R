#' runShinyAppReports starts shiny reports
#'
#' runShinyAppReports presents figs and stuff from the package in shiny
#'

runShinyAppReports <- function() {

     appName <- "report"
     appsDirectoryName <- "shinyApps"
     packageName <- "Nakke"

     rapbase::runShinyApp(appName, appsDirectoryName, packageName)
}
