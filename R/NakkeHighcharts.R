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

#' Provider of Highchart object
#' 
#' Provide Highchart object for GjsnGrVar
#' 
#' @inheritParams FigGjsnGrVar
#' @return h1 Highchart object
#' @export

GjsnGrVarHighchart <- function(Midt, N, Ngrtxt, tittel, utvalgTxt, GrNavnSort,
                               xaksetxt, KIHele, KIopp, KIned, MidtHele,
                               farger, deltittel) {
  
  # make data series
  df <- data.frame(y = Midt,
                   N = Ngrtxt,
                   stringsAsFactors = FALSE)
  ds <- rlist::list.parse(df)
  names(ds) <- NULL
  
  h1 <- highcharter::highchart() %>%
    hc_title(text = paste(tittel, "med 95% konfidensintervall")) %>%
    hc_subtitle(text = utvalgTxt) %>%
    hc_xAxis(categories = as.character(GrNavnSort),
             reversed = TRUE) %>%
    hc_yAxis(title = list(text=xaksetxt),
             min = -0.01,
             startOnTick = FALSE,
             plotBands = list(from=KIHele[1],
                              to=KIHele[2],
                              color=farger[4])) %>%
    hc_add_series(name = deltittel,
                  data = ds,
                  type = "bar",
                  color = farger[3],
                  tooltip = list(pointFormat='<b>{series.name}</b>:
                               {point.y:.1f}<br><b>N:</b>
                               {point.N} <br>')) %>%
    hc_tooltip(shared = TRUE)
  
  
  # add groups ci
  df <- data.frame(low = KIned,
                   high = KIopp,
                   stringsAsFactors = FALSE)
  ds <- rlist::list.parse(df)
  names(ds) <- NULL
  
  h1 <- hc_add_series(h1, name = "KI",
                      data = ds,
                      type = "errorbar",
                      color = farger[1],
                      tooltip = list(pointFormat='<b>KI:</b> {point.low:.1f} - {point.high:.1f} <br/>'))
  
  # add global score, ci as band defined i yAxis above
  h1 <- hc_add_series(h1, name = paste0(tittel, ", alle: ",
                                        sprintf('%.1f',MidtHele),
                                        ", N: ", N, ", KI: ",
                                        sprintf('%.1f', KIHele[1]), " - ",
                                        sprintf('%.1f', KIHele[2])),
                      data = rep(MidtHele, length(GrNavnSort)),
                      type = "line",
                      color = farger[2],
                      marker = list(enabled=FALSE),
                      enableMouseTracking = FALSE)
  
  h1 <- hc_exporting(h1, enabled = TRUE)
  
  return(h1)
}