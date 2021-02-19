# ggiraph provides the interactive plot element:
#' @import ggiraph
# ggplot2 provides the general plotting that'll be made interactive by ggiraph:
#' @import ggplot2
#'
# Load map of Swedish municipalities: (Taken from the package swemaps, cf. github.com/reinholdsson/swemaps, which
# in turn gets it from SCB.)
swe_kommuner_allpoints <- read.csv("map_kn.csv", fileEncoding = "UTF8", stringsAsFactors = TRUE)

fill_out_mapplotframe <- function(dat) {
  # Tar en dataframe av data för plottning på kartan,
  # och lägger till en rad med kommunnamn och NA för
  # varje kommun som det inte anges data för. Förenklar
  # att plotta data från enkäten på kartan. Antar att
  # den får en data frame med första kolumn kommunnamn
  # och andra data.
  # Tar också bort alla rader vars värde i första kolumnen
  # inte är namnet på en av Sveriges kommuner.

  allMunicipalities <- levels(swe_kommuner_allpoints$knnamn)

  present_muns <- unique(dat[,1])
  nonpresent <- allMunicipalities[!(allMunicipalities %in% present_muns)]
  for (mun in nonpresent) {
    row <- nrow(dat)+1
    dat[row,1] <- mun
    dat[row,2] <- NA
  }
  return(dat[dat[,1]%in% allMunicipalities,])
}

plot_on_map <- function(dat, tooltips = NA, hideNAtooltips = TRUE, mainTitle = NA, subTitle = NA, legendTitle = NA) {
  # Ritar upp en karta över Sveriges kommuner, färglagd efter värdet
  # på någon variabel.
  # Förväntar sig som input en data frame med första kolumn som innehåller
  # namn på kommuner (alla kommuner måste vara representerade), en andra
  # som innehåller datan som skall plottas.
  if (is.null(ncol(dat))) {
    if (!is.null(names(dat))) {
      muns <- names(dat)
      dat <- data.frame(muns,dat)
    }
  }
  dat <- fill_out_mapplotframe(dat)
  colnames(dat) <- c("knnamn","PlotVar")
  plotData <- dplyr::left_join(swe_kommuner_allpoints, dat, by="knnamn")
  if (identical(NA,tooltips)) {
    if (is.numeric(plotData$PlotVar)) {
      roundedPlotVar <- round(plotData$PlotVar, 2)
      tooltips <- paste(plotData$knnamn, rep(": ", length(plotData$knnamn)), roundedPlotVar)
    } else {
      tooltips <- paste(plotData$knnamn, rep(": ", length(plotData$knnamn)), plotData$PlotVar)
    }
    plotData$ttip <- tooltips
  } else if (ncol(tooltips) == 2) {
    plotData <- dplyr::left_join(plotData, tooltips, by="knnamn")
  }

  if (hideNAtooltips) {
    plotData$ttip[is.na(plotData$PlotVar)] <- NA
  }

  p <- ggplot(plotData, aes(ggplot_long, ggplot_lat)) +
    geom_polygon_interactive(aes(fill = PlotVar, group = knkod,
                                 tooltip = ttip, data_id = knkod)) +
    coord_equal() +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank()) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_blank())

  if(!is.na(mainTitle)) {
    if(is.na(subTitle)) {
      p <- p + ggtitle(mainTitle)
    } else {
      p <- p + ggtitle(mainTitle, subtitle = subTitle)
    }

  }
  if(!is.na(legendTitle)) {
    p <- p + labs(fill = legendTitle) + theme(legend.title = element_text(size=8))
  }

  return(p)
}

example_forest_plot <- function() {
  # Ett exempel på hur plot_on_map kan användas.
  # Ritar upp en karta över hur stor del av ytan hos varje kommun i Sverige
  # som används som produktiv eller improduktiv skogsmark.
  prcSkogsbruk <- sapply(levels(SCBdata$markanvandning$region), function(mun)
                        100*SCB(Municipality = mun, LandUseClass = c("skogsmark, produktiv","skogsmark, improduktiv"))/SCB(Municipality = mun, TotalArea = TRUE))
  ttip <- paste(levels(SCBdata$markanvandning$region), ": ",round(prcSkogsbruk, 1), "%", sep="")
  tooltips <- data.frame(knnamn = levels(SCBdata$markanvandning$region), ttip=ttip)
  prcSkogsbrukFrame <- data.frame(Municipality = names(prcSkogsbruk), PlotVar = prcSkogsbruk)
  return(plot_on_map(prcSkogsbrukFrame, tooltips = tooltips, mainTitle = "Procent av Sveriges kommuners yta som är skog", subTitle = "Data från SCB", legendTitle = "Pct."))
}
