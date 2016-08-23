#' Provider of Highchart object
#' 
#' Provide Highchart object for Andeler
#' 
#' @inheritParams FigAndeler
#' @return h1 Highchart object
#' @export

AndelerHighchart <- function(hoved, AntHoved, NHoved, rest, AntRest, NRest,
                             retn, subtxt, grtxt, shtxt, fargeHoved, fargeRest,
                             medSml, Tittel) {
 
  if (retn == 'H') {
    chartType <- "bar"
  } else {
    chartType <- "column"
  }
  # to use extra data in tooltips, make a data series from data frame
  df <- data.frame(y = hoved, N = as.vector(AntHoved),
                   stringsAsFactors = FALSE)
  ds <- rlist::list.parse(df)
  names(ds) <- NULL
  
  h1 <- highcharter::highchart() %>%
    hc_title(text = Tittel) %>%
    hc_xAxis(title = list(text=subtxt), categories=grtxt) %>%
    hc_yAxis(title = list(text='Andel pasienter (%)')) %>%
    hc_add_series(name = paste0(shtxt, " (N=", NHoved, ")"),
                  data = ds,
                  type = chartType, color = fargeHoved) %>%
    hc_tooltip(formatter = JS("function() { return '<b>' + this.series.name +
                              '</b><br>' +
                              'Andel = ' + this.y + '<br>' +
                              'N (' + this.x + ') = ' + this.point.N; }")) %>%
    hc_exporting(enabled = TRUE)
  if (medSml == 1) {
    df <- data.frame(y = rest, N = as.vector(AntRest),
                     stringsAsFactors = FALSE)
    ds <- rlist::list.parse(df)
    names(ds) <- NULL
    h1 <- hc_add_series(h1, name = paste0("Landet forøvrig (N=", NRest, ")"),
                        data = ds,
                        type = "scatter", color = fargeRest,
                        marker = list(symbol = "diamond", radius = 5))
  }
  
  return(h1)
}


#' Provider of Highchart object
#' 
#' Provide Highchart object for AndelerGrVar
#' 
#' @inheritParams FigAndelerGrVar
#' @return h1 Highchart object
#' @export

AndelerGrVarHighchart <- function(AndelerGrSort, Ngrtxt, AndelHele, N, Tittel,
                                  utvalgTxt, GrNavnSort, farger) {
  
  # to use extra data in tooltips, make a data series from data frame
  df <- data.frame(y = as.vector(AndelerGrSort), N = Ngrtxt,
                   stringsAsFactors = FALSE)
  ds <- rlist::list.parse(df)
  names(ds) <- NULL
  
  h1 <- highcharter::highchart() %>%
    hc_title(text = Tittel) %>%
    hc_subtitle(text = utvalgTxt) %>%
    hc_xAxis(categories=GrNavnSort) %>%
    hc_yAxis(title = list(text='Andel (%)')) %>%
    hc_add_series(name = "Andeler",
                  data = ds,
                  type = "bar", color = farger[3]) %>%
    hc_tooltip(formatter = JS("function() { return '<b>' + this.series.name +
                              '</b><br>' +
                              'Andel = ' + this.y + '<br>' +
                              this.point.N; }")) %>%
    hc_exporting(enabled = TRUE)
  
  # add global ratio
  h1 <- hc_add_series(h1, name = paste0("Hele landet (", sprintf('%.1f',
                                                                 AndelHele),
                                        " %), N=", N),
                      data = rep(AndelHele, length(GrNavnSort)),
                      type = "line",
                      color = farger[2],
                      marker = list(enabled=FALSE),
                      enableMouseTracking = FALSE
  )

  return(h1)
}

#' Provider of Highchart object
#' 
#' Provide Highchart object for AndelTid
#' 
#' @inheritParams FigAndelTid
#' @return h1 Highchart object
#' @export

AndelTidHighchart <- function(Aartxt, AndelHoved, NAarHendHoved, NHovedRes,
                              AndelRest,
                              NAarHendRest, NSmlRes, Tittel, utvalgTxt, shtxt,
                              smltxt, fargeHoved, fargeRest) {
  
  # to use extra data in tooltips, make a data series from data frame
  df <- data.frame(y = as.vector(AndelHoved),
                   N = as.vector(NAarHendHoved),
                   stringsAsFactors = FALSE)
  ds <- rlist::list.parse(df)
  names(ds) <- NULL
  
  h1 <- highcharter::highchart() %>%
    hc_title(text = Tittel) %>%
    hc_subtitle(text = utvalgTxt) %>%
    hc_xAxis(title = list(text="Innleggelsesår"), categories=as.character(Aartxt)) %>%
    hc_yAxis(title = list(text='Andel (%)')) %>%
    hc_add_series(name = paste0(shtxt, " N=", NHovedRes),
                  data = ds,
                  type = "line", color = fargeHoved) %>%
    hc_tooltip(formatter = JS("function() { return '<b>' + this.series.name +
                              '</b><br>' +
                              'Andel = ' + this.y + '<br>' +
                              'N = ' + this.point.N; }")) %>%
    hc_exporting(enabled = TRUE)
  
  # add global ratio
  df <- data.frame(y = as.vector(AndelRest),
                   N = as.vector(NAarHendRest),
                   stringsAsFactors = FALSE)
  ds <- rlist::list.parse(df)
  names(ds) <- NULL
  h1 <- hc_add_series(h1, name = paste0(smltxt, " (N=", NSmlRes, ")"),
                      data = ds,
                      type = "line",
                      color = fargeRest)
  
  return(h1)
}
