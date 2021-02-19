#' Fill out partial municipality data with NAs
#'
#' This function takes a data frame consisting of one column of municipality names and one column of data for each municipality,
#' and adds a row for each municipality that is not present, with NA in the data column. It also strips out any row which does
#' not have the name of a municipality in the first column.
#'
#' @param dat Data frame of partial data on municipalities
#'
#' @examples
#' fill_out_mapplotframe(data.frame(municipality = c("Stockholm", "Lund", "Uppsala"), students = c(23000, 32000, 33500)))
#' @return A data frame with one row for each municipality

fill_out_mapplotframe <- function(dat) {
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
#' municipalities without data are set to NA.
#' @param tooltips A data frame with two columns, the first of which contains the name of a municipality and the second
#' the tooltip that should be shown for that municipality. If omitted, tooltips are generated automatically from the data.
#' @param mainTitle Main title of the plot, displayed at the top. If omitted, plot has no title.
#' @param subTitle Subtitle of the plot, displayed right below the title. If omitted, plot has no subtitle.
#' @param legendTitle Title of the legend of the plot. If omitted, legend has no title - so don't omit this.
#'
#' @examples
#' exampledata <- data.frame(municipality = c("Stockholm", "Lund", "Uppsala"), students = c("some", "many", "loads"))
#' girafe(ggobj = plot_on_map(exampledata))
#' @returns A ggplot object that can be turned into an interactive graphic with the girafe function, see the example.
#'
#' @export

plot_on_map <- function(dat, tooltips = NA, mainTitle = NA, subTitle = NA, legendTitle = "") {
  if (is.null(ncol(dat))) {
    if (!is.null(names(dat))) {
      muns <- names(dat)
      dat <- data.frame(muns, dat)
    }
  }
  dat <- fill_out_mapplotframe(dat)
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
#' girafe(ggobj = example_forest_plot())
#' @export

example_forest_plot <- function() {
  prcSkogsbruk <- sapply(levels(SCBdata$markanvandning$region), function(mun) {
    100 * SCB(Municipality = mun, LandUseClass = c("skogsmark, produktiv", "skogsmark, improduktiv")) / SCB(Municipality = mun, TotalArea = TRUE)
  })
  ttip <- paste(levels(SCBdata$markanvandning$region), ": ", round(prcSkogsbruk, 1), "%", sep = "")
  tooltips <- data.frame(knnamn = levels(SCBdata$markanvandning$region), ttip = ttip)
  prcSkogsbrukFrame <- data.frame(Municipality = names(prcSkogsbruk), PlotVar = prcSkogsbruk)
  return(plot_on_map(prcSkogsbrukFrame, tooltips = tooltips, mainTitle = "Procent av Sveriges kommuners yta som är skog", subTitle = "Data från SCB", legendTitle = "Pct."))
}
