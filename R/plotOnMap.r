# There are two libraries we need to import for this code to work (we could write ggplot2:: one hundred times instead,
# but this is cleaner), and for some reason a package in Depends: in DESCRIPTION is not added to NAMESPACE by roxygen,
# so here we go:
# ggiraph provides the interactive plot element:
#' @import ggiraph
# ggplot2 provides the general plotting that'll be made interactive by ggiraph:
#' @import ggplot2


# This function takes a data frame consisting of one column of municipality names and one column of data for each municipality,
# and adds a row for each municipality that is not present, with NA in the data column. It also strips out any row which does
# not have the name of a municipality in the first column. Finally, it resets the row names of the data frame to just numbering them.
# (This last bit is mostly useful to make the testing easier -- the row names should never be getting used anyway.)

# It is called by plotOnMap on all of its inputs, so it doesn't really need to be exported for use by package users -- the only
# usecase is to make plotting easier, but now that is not necessary. (This function was previously exported.)

fillOutMapPlotFrame <- function(dat) {
  # First, we need to validate our input: Is it a data frame with at least two columns, the first of which is of type
  # character?
  if (!is.data.frame(dat)) {
    stop("The argument to fillOutMapPlotFrame must be a data frame.")
  } else if (ncol(dat) < 2) {
    stop("The data frame passed to fillOutMapPlotFrame must have at least two columns.")
  } else if (!is.character(dat[, 1])) {
    stop("The first column of the data frame passed to fillOutMapPlotFrame must be of type character.")
  }

  # First, what are the unique values of the first column? These should ideally be names of municipalities.
  presentMunicipalities <- unique(dat[, 1])
  # Which of the municipalities of Sweden are not present in the list?
  nonPresentMunicipalities <- municipalityNames[!(municipalityNames %in% presentMunicipalities)]
  # Add one row per municipality that is not present, containing the name of that municipality in the first
  # column and NA in every other column:
  for (municipality in nonPresentMunicipalities) {
    row <- nrow(dat) + 1
    dat[row, ] <- NA_real_ # Still uncertain what type of NA to put here to be honest... It appears that the NAs get coerced to the right type of NA anyway?
    dat[row, 1] <- municipality
  }
  # Finally, we strip out every row whose first element is not the name of a municipality.
  # So we are guaranteed to be returning a data frame that contains at least one row per municipality, and no rows that
  # do not correspond to a municipality.
  dat <- dat[dat[, 1] %in% municipalityNames, ]
  # And reset the row names:
  rownames(dat) <-  c(1:nrow(dat))
  return(dat)
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
  # First thing to check is if the data we got is in the format of a named vector of things to plot,
  # or is a data frame, and then for each case check that it has the requisite information.
  # It is a little tricky to know precisely what type we want to require the vector to be, since
  # the only real requirement is that it be plottable by ggplot2. We choose to be permissive and
  # say any atomic type is okay, though some might result in ugly plots:
  if (is.atomic(dat)) {
    # So we have the vector case. Then, we need the vector to have names:
    if (!is.null(names(dat))) {
      # So it has names, and we can transform this input into a data frame, which is what the rest of the code will want:
      presentMunicipalityNames <- names(dat)
      dat <- data.frame(presentMunicipalityNames, dat)
    } else {
      # No name, no game. We throw an error. Note that we aren't checking that the names are names of municipalities --
      # any irregularities will get sorted out by fillOutMapPlotFrame. But passing something without names entirely is almost
      # surely a mistake, since it could never work, so it's best to tell the user that it is an error:
      stop("If supplying a vector to plotOnMap, that vector must be named, with names corresponding to municipalities.")
    }
  } else if (is.data.frame(dat)) {
    # The other acceptable case. Here, the first column should be of type character (and contain municipality names), and there
    # should be at least two columns, since we plot the second column:
    if (ncol(dat) < 2) {
      stop("When passing a data frame, it must have at least two columns -- the first should contain municipality names, the second the data to plot, and further columns are ignored.")
    }
    if(!(is.character(dat[,1]))) {
      stop("The first column must be of type character, because it should contain names of municipalities.")
    }
    if (ncol(dat) > 2) {
      warning("You passed a data frame with more than two columns to be plotted. The columns after the first two will be ignored.")
    }
  } else {
    # The user has passed an invalid data type:
    stop("Input to plot must be either a named vector or a data frame.")
  }

  # We also check that our title parameters are given correctly -- they should either be missing
  # or be character of length 1:
  if (!(missing(mainTitle) || (is.character(mainTitle) && length(mainTitle) == 1))) {
    stop("mainTitle, if not omitted, must be a single object of type character")
  }
  if (!(missing(subTitle) || (is.character(subTitle) && length(subTitle) == 1))) {
    stop("subTitle, if not omitted, must be a single object of type character")
  }
  if (!(missing(legendTitle) || (is.character(legendTitle) && length(legendTitle) == 1))) {
    stop("legendTitle, if not omitted, must be a single object of type character")
  }
  # A subtitle won't be shown if the main title is missing, so if this is the case we give a warning:
  if (missing(mainTitle) && !missing(subTitle)) {
    warning("A subtitle won't be shown if the main title is missing")
  }

  # Now we at least know that we have a data frame of the right form. Time to make sure all its rows are
  # municipalities, and every municipality has at least one row:
  dat <- fillOutMapPlotFrame(dat)

  # Next, we need to make sure that no municipality has multiple rows:
  if (length(unique(dat[,1])) != length(dat[,1])) {
    # Multiple rows for one municipality means we don't know which to plot:
    stop("You must not supply more than one data point per municipality, otherwise we can't know which to use in the plot.")
  }

  # Having made sure we have data of the right format to plot, it is time to attach it to the map, so it can be plotted.
  # To do this we must know what the column names are, so we pick "knnamn" (since that is what the municipality name column
  # is called in the map data) and "PlotVar", for "variable we want to plot":
  colnames(dat) <- c(1:ncol(dat)) # Just making sure there isn't already a column with one of those names
  colnames(dat)[1:2] <- c("knnamn", "PlotVar")
  # Then, we join the data frame of data to plot to the map. Note that "swe_kommuner_allpoints" is stored in the internal data
  # of the library, and contains the map of the municipalities of Sweden.
  plotData <- dplyr::left_join(swe_kommuner_allpoints, dat, by = "knnamn")

  # Having created the data frame we will be passing to ggplot, it is time to consider what tooltips we want. We can auto-generate
  # tooltips if the parameter is missing, and if it is not missing, we check that they are in the right format and use those instead.
  # Passing tooltips works precisely like passing the data itself -- either a named vector or a data frame.
  if (missing(tooltips)) {
    # So we need to create tooltips. This is pretty easily done: In the case where the data to plot is numeric,
    # we round it to two decimal places first and then paste it together, otherwise we just paste immediately:
    if (is.numeric(plotData$PlotVar)) {
      roundedPlotVar <- round(plotData$PlotVar, 2)
      tooltips <- paste(plotData$knnamn, rep(": ", length(plotData$knnamn)), roundedPlotVar, sep="")
    } else {
      tooltips <- paste(plotData$knnamn, rep(": ", length(plotData$knnamn)), plotData$PlotVar, sep="")
    }
    plotData$tooltips <- tooltips
  } else if (is.data.frame(tooltips)) {
    # In the case where we have been passed a data frame, we check the column types and numbers, and then
    # pass it to fillOutMapPlotFrame to add any missing tooltips, and finally check that we have one tooltip
    # per municipality, before adding the tooltips to the plotData.
    if (ncol(tooltips) < 2) {
      stop("When passing tooltips as a data frame, we need at least two columns: the first with the name of the municipality to which the tooltip belongs, the second with the tooltip itself. Further columns are ignored.")
    }
    if (!is.character(tooltips[,1])) {
      stop("When passing tooltips as a data frame, the first column must be of type character, containing the names of municipalities to which the tooltips belong.")
    }
    if (!is.character(tooltips[,2])) {
      stop("When passing tooltips as a data frame, the second column must be of type character, containing the tooltips themselves.")
    }
    if (ncol(tooltips) > 2) {
      warning("You passed a data frame with more than two columns as tooltips. The columns after the first two will be ignored.")
    }

    # Now we make sure to have one tooltip per municipality and no extra rows that do not correspond to municipalities:
    tooltips <- fillOutMapPlotFrame(tooltips)
    if (length(unique(tooltips[,1]))!=length(tooltips[,1])) {
      stop("You must only provide one tooltip per municipality. Found a repeated municipality in first column of tooltips.")
    }

    # Finally, we can add the tooltips into plotData. First we need to make sure they have the right column names, though:
    colnames(tooltips) <- c(1:ncol(tooltips)) # Just making sure there isn't already a column with one of those names
    colnames(tooltips)[1:2] <- c("knnamn", "tooltip")
    plotData <- dplyr::left_join(plotData, tooltips, by = "knnamn")
  } else if (is.character(tooltips)) {
    # So we are in the case where we've been passed a vector of tooltips. First, of course, we check that it has names:
    if (is.null(names(tooltips))) {
      stop("When passing tooltips as a vector of the tooltips, each must be named with the municipality it corresponds to. Found no names of tooltips at all.")
    }
    # Then, that the names are precisely one per municipality:
    if (!(all(names(tooltips) %in% municipalityNames) && all(municipalityNames %in% names(tooltips)) && (length(names(tooltips)) == length(municipalityNames)))) {
      #TODO: This behaviour is inconsistent with the data.frame case, where it is okay to be missing some municipalities, and they are filled in with NA tooltips.
      stop("When passing tooltips as a named vector of characters, each municipality must have exactly one tooltip.")
    }
    # So we have data of the right form. Time to add it to the plotting data:
    tooltips <- data.frame(knnamn = names(tooltips), tooltip = tooltips)
    plotData <- dplyr::left_join(plotData, tooltips, by = "knnamn")
  } else {
    # So tooltips is of the wrong type. We throw an error:
    stop("Invalid specification of tooltips. Either omit the parameter to get automatically generated tooltips, or pass them as a data frame or a named vector.")
  }

  #TODO: Figure out how to display only some tooltips. Supplying NA for the tooltip text does not hide it, it just displays NA.

  # Having finally gotten all the data we need in the right format, we create the ggplot object:
  p <- ggplot(plotData,
              aes(.data$ggplot_long, .data$ggplot_lat)) + # The coordinates of the map are in ggplot_long and ggplot_lat, for longitude and latitude
    geom_polygon_interactive(aes( # geom_polygon_interactive is what enables the tooltips and highlight on hover
      fill = .data$PlotVar, group = .data$knkod,
      tooltip = .data$tooltip, data_id = .data$knkod
    )) +
    coord_equal() + # Needed for the right aspect ratio of the map
    theme( # Minimal theme, since the data we are presenting is not the latitudes and longitudes;
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank()
    ) +
    theme(
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_blank()
    )

  # If we have been given those by the user, we now add main- and subtitles as well as legend titles. Assuming they are of the
  # correct format, of course.
  if (!missing(mainTitle)) {
    if (missing(subTitle)) {
      p <- p + ggtitle(mainTitle)
    } else {
      p <- p + ggtitle(mainTitle, subtitle = subTitle)
    }
  }
  if (!missing(legendTitle)) {
    p <- p + labs(fill = legendTitle) + theme(legend.title = element_text(size = 8))
  }

  # Finally, we return the ggplot/ggiraph object. The user can plot it with girafe(ggobj=p)
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
  tooltipTexts <- paste(municipalityNames, ": ", round(prcSkogsbruk, 1), "%", sep = "")
  tooltips <- data.frame(knnamn = municipalityNames, tooltipText = tooltipTexts)
  prcSkogsbrukFrame <- data.frame(Municipality = names(prcSkogsbruk), PlotVar = prcSkogsbruk)
  # We need to Unicode escape the åäö here, because CRAN gets unhappy if non-ASCII characters are included in code, even if it is just a hardcoded string:
  return(plotOnMap(prcSkogsbrukFrame, tooltips = tooltips, mainTitle = "Procent av Sveriges kommuners yta som \u00E4r skog", subTitle = "Data fr\u00E5n SCB", legendTitle = "Pct."))
}
