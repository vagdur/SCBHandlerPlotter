# The way it works is similar to normal queries -- each table implements vectorised queries against itself, and has a method
# to check if a vectorised query can be run against it. The function vectorSCB itself just figures out which table to run
# the query against and passes it to the right table.

# Define canHandleQuery:
setGeneric("canHandleVectorQuery", function(x, vectorColumn, ...) standardGeneric("canHandleVectorQuery"))
setMethod("canHandleVectorQuery", "DocumentedTable", function(x, vectorColumn, ...) {
  # First we pull out the names of the arguments we got: (Note that "list(...)" evaluates all the arguments. Right now
  # we don't care about this, but this does prevent us from doing metaprogramming dark magics to the stuff in list(...))
  argumentNames = names(list(...))

  # If it wasn't already on it, we append the vectorColumn to the list of argument names:
  if (!(vectorColumn %in% argumentNames)) {
    # We need to record that we didn't receive a list of values for the vectorColumn:
    vectorColumnValuesGiven <- FALSE
    argumentNames[[length(argumentNames) + 1]] <- vectorColumn
  } else {
    vectorColumnValuesGiven <- TRUE
  }

  # Then we check that we can dealias each of these to a proper column name:
  dealiasedArgumentNames <- columnDealias(x, argumentNames)
  if (length(dealiasedArgumentNames) != length(argumentNames)) {
    # At least one argument name failed to dealias, so we don't know what to do with this, and
    # we return FALSE to indicate the table cannot handle this query:
    return(FALSE)
  }

  # So we know all the column names we were given are real. If we weren't given a list of values for the vectorColumn,
  # we need to explicitly add the empty list there:
  argumentValues <- list(...)
  if (!vectorColumnValuesGiven) {
    argumentValues[[vectorColumn]] <- numeric(0)
  }

  # Time to check that each argument value corresponds to a (potential) level of that column:

  for (argumentName in argumentNames) {
    #TODO: This is a really bad algorithm, since it double loops, once over argumentNames and once over the columns
    # per argumentName. Should figure out a way to do this with a single loop? Might not be a huge issue as long as
    # we only have like ten columns, though.
    argumentValue <- argumentValues[[argumentName]]
    for (column in x@tableColumns) {
      # If this column is the one that the argument name refers to...
      if (name(column) == columnDealias(x,argumentName)) {
        # ... we try to dealias the value of the argument against this column...
        dealiasedArgumentValue <- levelDealias(column, argumentValue)
        if (length(dealiasedArgumentValue) != length(argumentValue)) {
          # ... and if at least one of the elements of argumentValue failed to dealias against
          # this column, we conclude that while the column name matches the parameter we were
          # passed, we are nonetheless not in the right table, since an invalid level was asked for.
          # Perhaps there is some other table with the same column name that can handle this? We,
          # however, return FALSE:
          return(FALSE)
        }
      }
    }
  }

  return(TRUE)
})

# Define the vectorQuery method for a single table:
setGeneric("vectorQuery", function(x, vectorColumn, verbose=NA, checkHandleable=NA, failIfUnable=NA, ...) standardGeneric("vectorQuery"))
setMethod("vectorQuery", "DocumentedTable", function(x, vectorColumn, verbose=FALSE, checkHandleable=TRUE, failIfUnable=FALSE, ...) {
  # First we might need to check that we can handle this query:
  if (checkHandleable) {
    if (!canHandleVectorQuery(x, vectorColumn = vectorColumn, ...)) {
      if (failIfUnable) {
        stop("Table is unable to handle your query.")
      } else {
        warning("Table is unable to handle your query. Returning NA.")
        return(NA)
      }
    }
  }

  # Let's store our user input in a better variable than ...: (And also force them all to be evaluated.)
  passedParameters <- list(...)

  # And dealias the vectorColumn the user gave us:
  vectorColumnObject <- columnDealias(x, vectorColumn, getColumn = TRUE)[[1]]
  dealiasedVectorColumn <- name(vectorColumnObject)

  # Second, we need to determine what levels of the vectorColumn to vector over:
  if (vectorColumn %in% names(passedParameters)) {
    # The user passed us a list of things to vector over in the "...". We move that over to its own special variable,
    # and remove it from passedParameters:
    levelsToVectorOver <- passedParameters[[vectorColumn]]
    passedParameters[[vectorColumn]] <- NULL
  } else {
    # We did not get an explicit list of things to vector over, so we assume we should vector over all levels of the column.
    # First dealias the name of the vectorColumn:
    levelsToVectorOver <- sort(unique(x@csvData[, dealiasedVectorColumn]))
  }

  # Having determined what to vector over, it is time to actually find the data the user wants:

  # We do this in two steps:
  #   1.  We subset our table's CSV data down to only the rows which we are interested in, as in
  #       a normal query()
  #   2.  Instead of just summing the valueColumn -- which would give an unvectorised result -- we
  #       create a list in which to store our output, and loop over the remaining row. For each row,
  #       we check the value of the vectorColumn, and add the value of the valueColumn to the corresponding
  #       entry of the output list. This evades the need to run subset() once per level we want to vector over,
  #       as the naive algorithm of just doing a sapply would -- so this should be much faster.


  # The first thing to do in step one is grab our list of columns and levels from ... and dealias them
  # into something our CSV can handle:
  columnNames <- names(list(...))
  columnLevels <- list(...)
  # Now we use the getColumn=TRUE mode of columnDealias to get the column objects themselves, so we can
  # immediately dealias the levels we were passed against them:
  queryColumns <- columnDealias(x, columnNames, getColumn=TRUE)
  for (i in c(1:length(columnLevels))) {
    columnLevels[[i]] <- levelDealias(queryColumns[[i]], columnLevels[[i]])
  }
  # So now we have dealiased all columns and all levels. Time to create the subset of data specified by our columns
  # other than the vectorColumn: (Or actually including the vectorColumn, if we are only vectoring over a subset of
  # its levels.) We do this one column at a time.
  runningSubset <- csvData(x)
  for (i in c(1:length(queryColumns))) {
    rowsToKeep <- runningSubset[, name(queryColumns[[i]])] %in% columnLevels[[i]]
    runningSubset <- runningSubset[rowsToKeep,]
  }

  # Having created this subset, we are ready to proceed to step two.
  queryResults <- rep(0, length(levelsToVectorOver))
  names(queryResults) <- levelDealias(vectorColumnObject, levelsToVectorOver)

  for (i in c(1:nrow(runningSubset))) {
    # First, let's figure out which level the current row is:
    currentRowLevel <- as.character(runningSubset[i, dealiasedVectorColumn])
    # Then, to the corresponding entry of queryResults, we add in the value of the valueColumn
    # for this row
    queryResults[currentRowLevel] <- queryResults[currentRowLevel] + runningSubset[i, valueColumn(x)]
  }

  # Finally, we set the names of the output to the names the user used, instead of the dealiased names:
  # (This only has an effect if the user supplied us a list of vectors to level over.)
  names(queryResults) <- levelsToVectorOver
  return(queryResults)
})

#' Vectorise a call to SCB
#'
#' This function provides vectorisation of the SCB function -- it is effectively equivalent
#' to just doing it yourself with an sapply call, but it is considerably faster, since it doesn't have
#' to run query() once per level you're vectorising over.
#' It is called precisely like \code{SCB()} for the query, but its other parameters are different.
#' The central one is vectorColumn which should be set to the name of a column. The function then looks at whether you
#' provided a list of values for that column: If you did, the function runs your query on each of those values, otherwise,
#' if you omitted that column, it looks up what all of its possible values are, and vectorises over those.
#' The verbose and failIfUnable parameters still exist, with the same effect, but collisionHandlingMode is not supported.
#'
#' @param vectorColumn Which column should be vectorised over. This argument should not be quoted, just write it straight up. See examples.
#' @param verbose Causes some extra messages to be printed explaining what is happening. If you are getting unexpected results, try setting this to TRUE first.
#' @param forceTable Directs the query to a specific table, in case your query does not uniquely determine a table.
#' @param failIfUnable If the query cannot be handled by our tables, should we throw an error, or just a warning and return NA? TRUE gives errors, FALSE gives warnings.
#' @param ... Your query, see documentation of SCB() for syntax for this.
#'
#' @seealso SCB
#'
#' @examples
#' # How much forest is there in each municipality in Sweden?:
#' vectorSCB(LandUseType = "Total forest", vectorColumn = Municipality)
#' # How much land is used for each type of use in Ludvika municipality?
#' vectorSCB(Municipality = "Ludvika", vectorColumn = LandUseType)
#' # In order to use a column whose name is stored in a variable, use !! like this:
#' # (!! is provided by the library rlang.)

#' columnToVector <- "Municipality"
#' vectorSCB(Gender = "Male", forceTable = "AgeGender", vectorColumn = !!columnToVector)
#'
#' @returns A named vector whose elements are results from SCB() and names are the level of the column we vectorised over
#'
#' @export

vectorSCB <- function(vectorColumn, verbose=FALSE, forceTable = NA_character_, failIfUnable = FALSE, ...) {
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

  # Okay, so now we have the name of the column we want to vectorise over, so we are ready to figure out which table(s) can handle this query:

  if (!missing(forceTable)) {
    # First, we check that the value of forceTable is actually the name of a table:
    if(!(forceTable %in% loadedTableNames)) {
      stop(paste(forceTable,"is not the name of a table."))
    }
    # We check that the table asked for can actually handle the query:
    if(!canHandleVectorQuery(allLoadedTables[[which(loadedTableNames == forceTable)]], vectorColumn = vectorColumnName, ...)) {
      # If it can't, we either throw an error or return NA with a warning, depending on the value of failIfUnable:
      if (failIfUnable) {
        stop("The table that the query was forced to via forceTable is unable to handle the query.")
      } else {
        warning("The table that the query was forced to via forceTable is unable to handle the query. Returning NA.")
        return(NA_real_)
      }
    } else {
      # So it can handle the query, and we just pass this off straight to that table. It doesn't need to recheck
      # if it can handle the query, since we already did that for it.
      return(vectorQuery(allLoadedTables[[which(loadedTableNames == forceTable)]],
                         vectorColumn = vectorColumnName, checkHandleable = FALSE, verbose = verbose, ...))
    }
  } else { #forceTable is missing:
    # This should be the more common case -- we first need to figure out what table to send the query to, and then
    # do it. The first part can be done in one line with a judicious use of sapply:
    tablesAbleToHandleQuery <- which(sapply(allLoadedTables, function(tab) canHandleVectorQuery(tab, vectorColumn = vectorColumnName, ...)))

    # Now we have a trilemma: Are zero, one, or more tables able to handle the query?
    if (length(tablesAbleToHandleQuery) == 0) {
      # So we can't handle the query. What to do depends on failIfUnable:
      if (failIfUnable) {
        stop("No table is able to handle your query -- have you misspelled a column or level?")
      } else {
        warning("No table is able to handle your query -- have you misspelled a column or level? Returning NA.")
        return(NA_real_)
      }
    } else if (length(tablesAbleToHandleQuery) == 1) {
      # This is the easiest case -- there's precisely one table able to handle the request.
      if (verbose) {
        # When verbose, we tell the user which table the data comes from:
        message(paste("Query matched table ",loadedTableNames[tablesAbleToHandleQuery],". Returning data from said table.",sep=""))
      }
      # we run the query against the table and return the result:
      return(vectorQuery(allLoadedTables[[tablesAbleToHandleQuery]], vectorColumn = vectorColumnName, checkHandleable = FALSE, verbose = verbose, ...))
    } else if (length(tablesAbleToHandleQuery) > 1) {
      # Unlike plain SCB(), our job here is simple, since we have no collision handling modes. We just throw an error.
      stop(paste("Multiple tables are able to handle your request: ",paste(loadedTableNames[tablesAbleToHandleQuery], collapse=", "),".\n
                   You can get around this error either by using forceTable to specify which of these tables to query, or by choosing a
                   different collisionHandlingMode than \"error\".",sep=""))
    }
  }
}
