# The tip of the iceberg, the function that actually lets users query from
# the tables in a simple and intuitive way.
# Note that this function can unfortunately not be documented by just writing
# some Roxygen tags with documentation before the function -- the documentation
# should contain links to the documentation of all tables we have, which is liable
# to change. Thus, documentation of this function is written by the same script as
# the one that writes documentation of the tables themselves.

# So we have just one Roxygen tag:
#' @export
SCB <- function(verbose = FALSE, forceTable = NA_character_, failIfUnable = FALSE, collisionHandlingMode = "error", ...) { # All the juice is of course in the innocuous-looking "..."
  # So we have received a query. There are two cases: Either forceTable is set, in which case we just funnel the query
  # on to that specific table, or it is not, in which case we need to figure out which table to send the query to.
  # First, however, we check that our parameters are valid:
  if (!(collisionHandlingMode %in% c("error","namedVector","consensus","arbitrary"))) {
    stop(paste('"',collisionHandlingMode,'" is not a valid collision handling mode. It must be one of "error", "namedVector", "consensus", or "arbitrary".'))
  }
  if (!is.logical(verbose)) {
    stop("verbose must be TRUE or FALSE.")
  }
  if (!is.logical(failIfUnable)) {
    stop("failIfUnable must be TRUE or FALSE.")
  }
  if (!is.character(forceTable)) {
    stop("forceTable must be character.")
  }
  if (!missing(forceTable)) {
    # First, we check that the value of forceTable is actually the name of a table:
    if(!(forceTable %in% loadedTableNames)) {
      stop(paste(forceTable,"is not the name of a table."))
    }
    # We check that the table asked for can actually handle the query:
    if(!canHandleQuery(allLoadedTables[[which(loadedTableNames == forceTable)]])) {
      # If it can't, we either throw an error or return NA with a warning, depending on the value of failIfUnable:
      if (failIfUnable) {
        stop("Table that query was forced to via forceTable is unable to handle the query.")
      } else {
        warning("Table that query was forced to via forceTable is unable to handle the query. Returning NA.")
        return(NA_real_)
      }
    } else {
      # So it can handle the query, and we just pass this off straight to that table. It doesn't need to recheck
      # if it can handle the query, since we already did that for it.
      return(query(allLoadedTables[[which(loadedTableNames == forceTable)]], checkHandleable = FALSE, verbose = verbose, ...))
    }
  } else {
    # This should be the more common case -- we first need to figure out what table to send the query to, and then
    # do it. The first part can be done in one line with a judicious use of sapply:
    tablesAbleToHandleQuery <- which(sapply(allLoadedTables, function(tab) canHandleQuery(tab,...)))

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
      return(query(allLoadedTables[[tablesAbleToHandleQuery]], checkHandleable = FALSE, verbose = verbose, ...))
    } else if (length(tablesAbleToHandleQuery) > 1) {
      # This is a tricky case -- there is more than one possible table to send the query to.
      # There are several ways we could handle this, depending on collisionHandlingMode:
      if (collisionHandlingMode == "error") {
        # Simplest, and default mode: We just throw an error.
        stop(paste("Multiple tables are able to handle your request: ",paste(loadedTableNames[tablesAbleToHandleQuery], collapse=", "),".\n
                   You can get around this error either by using forceTable to specify which of these tables to query, or by choosing a
                   different collisionHandlingMode than \"error\".",sep=""))
      } else if (collisionHandlingMode == "namedVector") {
        # Second simplest method: We run the query by every table that can handle it, and return a named vector of the results from each query.
        # If verbose, we warn about this behaviour:
        if (verbose) {
          message(paste("Multiple tables are able to handle your request: ",paste(loadedTableNames[tablesAbleToHandleQuery], collapse=", "),".\n
                   Returning a named vector of the results of running your query by each of these tables.\n
                   You can get around this behaviour either by using forceTable to specify which of these tables to query, or by choosing a
                   different collisionHandlingMode than \"namedVector\".",sep=""))
        }
        queryResults <- sapply(tablesAbleToHandleQuery, function(tableIndex) query(allLoadedTables[[tableIndex]], checkHandleable = FALSE, verbose=verbose, ...))
        names(queryResults) <- loadedTableNames[tablesAbleToHandleQuery]
        return(queryResults)
      } else if (collisionHandlingMode == "arbitrary") {
        # Another simple method, though probably usually ill advised: We just run it by the first table in alphabetical order that can handle the query.
        # If verbose, we inform of this behaviour:
        if (verbose) {
          message(paste("Multiple tables are able to handle your request: ",paste(loadedTableNames[tablesAbleToHandleQuery], collapse=", "),".\n
                   Arbitrarily choosing table ",loadedTableNames[tablesAbleToHandleQuery[1]]," for your query.\n
                   You can get around this behaviour either by using forceTable to specify which of these tables to query, or by choosing a
                   different collisionHandlingMode than \"arbitrary\".",sep=""))
        }
        return(query(allLoadedTables[[ tablesAbleToHandleQuery[1] ]], verbose = verbose, checkHandleable = FALSE, ...))
      } else if (collisionHandlingMode == "consensus") {
        # This mode is the one that is a bit less simple: We run the query by all tables, and check if they agree on the result. If they do, we return
        # that consensus, otherwise, we throw an error.
        queryResults <- sapply(tablesAbleToHandleQuery, function(tableIndex) query(allLoadedTables[[tableIndex]], checkHandleable = FALSE, verbose=verbose, ...))
        if (length(unique(queryResults)) == 1) {
          # There was a consensus! If verbose, we still warn about this:
          if (verbose) {
            message(paste("Multiple tables are able to handle your request: ",paste(loadedTableNames[tablesAbleToHandleQuery], collapse=", "),".\n
                   Ran your query against all these tables, and they agreed on the result, so we are returning that result. (This may have significantly slowed your query down.)\n
                   You can get around this behaviour either by using forceTable to specify which of these tables to query, or by choosing a
                   different collisionHandlingMode than \"consensus\".",sep=""))
          }
          return(unique(queryResults))
        } else {
          # No consensus, so we throw an error:
          stop(paste("Multiple tables are able to handle your request: ",paste(loadedTableNames[tablesAbleToHandleQuery], collapse=", "),".\n
                   Ran your query against all these tables, and they disagreed on the result, so we are throwing an error. (This may have significantly slowed your query down.)\n
                   You can get around this behaviour either by using forceTable to specify which of these tables to query, or by choosing a
                   different collisionHandlingMode than \"consensus\".",sep=""))
        }
      }
    }
  }
}
