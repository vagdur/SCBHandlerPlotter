#' Vectorise a call to SCB
#'
#' This function provides vectorisation of the SCB function -- it is effectively equivalent
#' to just doing it yourself with an sapply call, but makes for slightly neater code. Plus, there
#' are some cases where this is not easy to do with a sapply -- implementing this function in full
#' generality requires a bit of metaprogramming, which one probably doesn't want to do when writing
#' a script...
#' It is called precisely like \code{SCB()} for the query, but its other parameters are different.
#' The central one is vectorColumn which should be set to the name of a column. The function then looks at if you
#' provided a list of values for that column: If you did, the function runs SCB() on each of those values, otherwise,
#' if you omitted that column, it looks up what all of its possible values are, and vectorises over
#' those.
#' The verbose parameter still exists, with the same effect, but failIfUnable and errorHandlingMode are not supported.
#'
#' @param vectorColumn Which column should be vectorised over. This argument should not be quoted, just write it straight up. See examples.
#' @param verbose Causes some extra messages to be printed explaining what is happening in the calls to downstream functions.
#' @param forceTable Directs the query to a specific table, in case your query does not uniquely determine a table.
#' @param ... Your query, see documentation of SCB() for syntax for this.
#'
#' @seealso SCB
#'
#' @examples
#' # How much forest is there in each municipality in Sweden?:
#' vectorSCB(LandUseType = "Total forest", vectorColumn = Municipality)
#' # How much land is used for each type of use in Ludvika municipality?
#' vectorSCB(Municipality = "Ludvika", vectorColumn = LandUseType)
#' # In order to use a column whose name is stored in a variable, use !! like this: (!! is provided by the library rlang.)
#' columnToVector <- "Municipality"
#' vectorSCB(Gender = "Male", forceTable = "AgeGender", vectorColumn = !!columnToVector)
#'
#' @returns A named vector whose elements are results from SCB() and names are the level of the column we vectorised over
#'
#' @export

vectorSCB <- function(vectorColumn, verbose=FALSE, forceTable = NA_character_, ...) {
  # First off, let us validate the input types:
  if (!missing(verbose)) {
    if (!is.logical(verbose)) {
      stop(paste("Argument verbose is expected to be TRUE/FALSE, not",typeof(verbose)))
    }
    if (length(verbose) != 1) {
      stop("Argument verbose is expected to be a single value, not a vector.")
    }
  }
  if (!missing(forceTable)) {
    if (!is.character(forceTable)) {
      stop(paste("Argument verbose is expected to be character, not",typeof(verbose)))
    }
    if (length(forceTable) != 1) {
      stop("Argument forceTable is expected to be a single value, not a vector.")
    }
  }

  # Some metaprogramming magic extracts the name of the column over which we will vectorise:
  vectorColumnName <- as.character(rlang::get_expr(rlang::enquo(vectorColumn)))

  # If this results in a vector of length greater than one, the user passed us some more complicated
  # expression than just a name, which we can't handle: (And if we get length 0, they passed NULL)
  if (length(vectorColumnName) != 1) {
    stop("You appear to have passed a complicated expression into vectorColumn. This is not evaluated -- if you want your arguments evaluated, use the !! operator provided by library rlang.")
  }

  # Next up, we need to figure out what levels of this column we are to vectorise over. There are
  # two cases -- either the user provided this list, or they expect to vectorise over all possible
  # values:
  levelsToVector <- list()
  if (vectorColumnName %in% names(list(...))) {
    # So the user provided this parameter, and our life is easy:
    levelsToVector <- list(...)[[vectorColumnName]]

    # We could do some error handling here, checking whether the column and levels the user specified actually exist. But that is handled
    # by SCB() when we call it, so we let it handle that check.
  } else {
    # The user did not provide an explicit list of values to vector over. Again, there are two cases:
    # If the user provided a forceTable, then we know which table to look for the levels of the column in.
    # If not, we need to figure out which tables can handle our query.
    if (!missing(forceTable)) {
      # First, is the forceTable a real table?
      if (!(forceTable %in% loadedTableNames)) {
        stop(paste("Value ",forceTable," of forceTable does not match any existing table.",sep=""))
      }
      # Okay, so it is real... Does it actually have the requested column?
      dealiasedVectorColumnName <- columnDealias(allLoadedTables[[which(loadedTableNames == forceTable)]], vectorColumnName)
      if (is.null(dealiasedVectorColumnName)) {
        stop(paste("vectorColumn set to ",vectorColumnName,", which is not a column in table ",forceTable,", which query was forced to via forceTable.",sep=""))
      }
      # So it has the requested column. Time to fetch the levels of said column:
      levelsToVector <- sort(unique(csvData(allLoadedTables[[which(loadedTableNames == forceTable)]])[, dealiasedVectorColumnName]))
    } else {
      # So we don't get a forceTable, and have to figure it out ourselves. The way to do this is of course to ask each table if it is able
      # to handle a request involving both the values in ... and vectorColumn.
      tablesAbleToRespond <- list()
      for (candidateTable in allLoadedTables) {
        # First, we need to set up the arguments we'll want to use:
        callArguments <- list(...)
        # When we supply an empty list, we still get a check that the column is present, but no checks that we supplied the right type of empty list:
        callArguments[[vectorColumnName]] <- character()
        # We also need to supply the table itself as an argument to canHandleQuery:
        callArguments[["x"]] <- candidateTable
        # Now we check if the table can handle the query. Unfortunately we can't write this as just a call to canHandleQuery with argument callArguments,
        # we need to use do.call:
        if(do.call("canHandleQuery", callArguments)) {
          tablesAbleToRespond[[length(tablesAbleToRespond) + 1]] <- candidateTable
        }
      }
      # If more than one table can handle the query, we throw an error, since we haven't implemented collision handling for this function.
      #TODO: Implement collision handling for this function.
      if (length(tablesAbleToRespond) > 1) {
        # Just to be nice, we give the user a list of which tables were able to handle the query:
        namesOfRespondingTables <- sapply(tablesAbleToRespond, name)
        stop(paste("More than one table was able to handle your query, specifically the following:\n",
                   paste(namesOfRespondingTables, collapse=", "),".\n"
                   ,"Please use forceTable to specify which table to query."),sep="")
      }
      # Likewise, if no table is able to respond, we throw an error:
      #TODO: Implement failIfUnable parameter for this function.
      if (length(tablesAbleToRespond) == 0) {
        stop("No table was able to handle your query. Have you misspelled a column or level name?")
      }

      # So there is exactly one table able to handle this query, and now we can fetch the levels of the sought-after column:
      dealiasedVectorColumnName <- columnDealias(tablesAbleToRespond[[1]], vectorColumnName)
      levelsToVector <- sort(unique(csvData(tablesAbleToRespond[[1]])[, dealiasedVectorColumnName]))

      # Since we know which table is supposed to be handling the query, we can just set forceTable now to simplify the job
      # for SCB():
      forceTable <- name(tablesAbleToRespond[[1]])
    }
  }

  # So we finally have the list of levels over which to vector. Time to actually do the vectoring -- at this point we can
  # actually just do a sapply, although with a slightly more complicated function inside the sapply than usual:
  queryResults <- sapply(levelsToVector, function(level) {
    # What we need to do is construct our query to SCB() and then call it using do.call:
    callArguments <- list(...)
    callArguments[[vectorColumnName]] <- level
    callArguments$forceTable <- forceTable
    callArguments$verbose <- verbose
    do.call("SCB", callArguments)
  })
  # Then we attach the right names to the result and hand it back to the user:
  names(queryResults) <- levelsToVector
  return(queryResults)
}
