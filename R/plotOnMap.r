# This function takes a data frame consisting of one column of municipality names and one column of data for each municipality,
# and adds a row for each municipality that is not present, with NA in the data column. It also strips out any row which does
# not have the name of a municipality in the first column. It is called by plotOnMap on all of its inputs, so it doesn't really
# need to be exported for use by package users -- the only usecase is to make plotting easier, but now that is not necessary.
# (This function was previously exported.)

fillOutMapPlotFrame <- function(dat) {
  allMunicipalities <- levels(swe_kommuner_allpoints$knnamn)

  present_muns <- unique(dat[, 1])
  nonpresent <- allMunicipalities[!(allMunicipalities %in% present_muns)]
  for (mun in nonpresent) {
    row <- nrow(dat) + 1
    dat[row, 1] <- mun
    dat[row, 2] <- NA
  }
  return(dat[dat[, 1] %in% allMunicipalities, ])
}

#' Create interactive plot of data on map of Sweden
#'
#' This function takes in data on Swedish municipalities, and creates a plot of said data on an interactive
#' map of Sweden.
#'
#' @param dat Either a data frame of two columns, the first of which contains names of municipalities and the second
#' the data, or a named list, where the names are names of municipalities. Allowed to be partial data,
#' municipalities without data have their data automatically set to NA.
#' @param tooltips A data frame with two columns, the first of which contains the name of a municipality and the second
#' the tooltip that should be shown for that municipality. If omitted, tooltips are generated automatically from the data.
#' @param mainTitle Main title of the plot, displayed at the top. If omitted, plot has no title.
#' @param subTitle Subtitle of the plot, displayed right below the title. If omitted, plot has no subtitle.
#' @param legendTitle Title of the legend of the plot. If omitted, legend has no title - so don't omit this.
#'
#' @examples
#' library(ggplot2)
#' library(ggiraph)
#' exampledata <- data.frame(municipality = c("Stockholm", "Lund", "Uppsala"),
#'                           students = c("some", "many", "loads"))
#' girafe(ggobj = plotOnMap(exampledata))
#' @returns A ggplot object that can be turned into an interactive graphic with the girafe function, see the example.
#'
#' @export

plotOnMap <- function(dat, tooltips = NA, mainTitle = NA, subTitle = NA, legendTitle = "") {
  if (is.null(ncol(dat))) {
    if (!is.null(names(dat))) {
      muns <- names(dat)
      dat <- data.frame(muns, dat)
    }
  }
  dat <- fillOutMapPlotFrame(dat)
  colnames(dat) <- c("knnamn", "PlotVar")
  plotData <- dplyr::left_join(swe_kommuner_allpoints, dat, by = "knnamn")
  if (identical(NA, tooltips)) {
    if (is.numeric(plotData$PlotVar)) {
      roundedPlotVar <- round(plotData$PlotVar, 2)
      tooltips <- paste(plotData$knnamn, rep(": ", length(plotData$knnamn)), roundedPlotVar)
    } else {
      tooltips <- paste(plotData$knnamn, rep(": ", length(plotData$knnamn)), plotData$PlotVar)
    }
    plotData$ttip <- tooltips
  } else if (ncol(tooltips) == 2) {
    plotData <- dplyr::left_join(plotData, tooltips, by = "knnamn")
  }

  # This doesn't work, the tooltip just displays as NA instead of not showing at all:
  # if (hideNAtooltips) {
  #  plotData$ttip[is.na(plotData$PlotVar)] <- NA
  # }

  p <- ggplot(plotData, aes(ggplot_long, ggplot_lat)) +
    geom_polygon_interactive(aes(
      fill = PlotVar, group = knkod,
      tooltip = ttip, data_id = knkod
    )) +
    coord_equal() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank()
    ) +
    theme(
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_blank()
    )

  if (!is.na(mainTitle)) {
    if (is.na(subTitle)) {
      p <- p + ggtitle(mainTitle)
    } else {
      p <- p + ggtitle(mainTitle, subtitle = subTitle)
    }
  }
  if (!is.na(legendTitle)) {
    p <- p + labs(fill = legendTitle) + theme(legend.title = element_text(size = 8))
  }

  return(p)
}

#' Create an example plot of forests in Sweden
#'
#' This function is an example of how one can integrate the \code{SCB()} function's handling of data with the
#' plotting utilities, to create a plot of how large a proportion of the area of each municipality is
#' forests.
#'
#' @examples
#' library(ggplot2)
#' library(ggiraph)
#' girafe(ggobj = exampleForestPlot())
#' @export

exampleForestPlot <- function() {
  prcSkogsbruk <- sapply(municipalityNames, function(mun) {
    100 * SCB(Municipality = mun, LandUseType = c("skogsmark, produktiv", "skogsmark, improduktiv")) / SCB(Municipality = mun, LandUseType = "Total area")
  })
  ttip <- paste(municipalityNames, ": ", round(prcSkogsbruk, 1), "%", sep = "")
  tooltips <- data.frame(knnamn = municipalityNames, ttip = ttip)
  prcSkogsbrukFrame <- data.frame(Municipality = names(prcSkogsbruk), PlotVar = prcSkogsbruk)
  # We need to Unicode escape the åäö here, because CRAN gets unhappy if non-ASCII characters are included in code, even if it is just a hardcoded string:
  return(plotOnMap(prcSkogsbrukFrame, tooltips = tooltips, mainTitle = "Procent av Sveriges kommuners yta som \u00E4r skog", subTitle = "Data fr\u00E5n SCB", legendTitle = "Pct."))
}
