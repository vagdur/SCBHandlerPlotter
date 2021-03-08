#' Query the collection of SCB tables for data
#'
#' This function is the main meat of the library, allowing you to query a large collection of statistical tables from Statistics Sweden (SCB) and other sources
#' of official statistics. The syntax is very simple -- you pass as parameters the names of columns that are present in a table (e.g. EducationLevel), and set them
#' to values of that column you want (e.g. "Postgraduate education"), and then \code{SCB(EducationLevel = "Postgraduate education")} tells you how many people in Sweden
#' have a postgraduate education (94718). Adding more parameters lets you narrow your query down further --
#' \code{SCB(EducationLevel = "Postgraduate education", Gender="Women")} tells you how many of those are women (37906), and this can be even further narrowed down to
#' \code{SCB(EducationLevel = "Postgraduate education", Gender="Women", Municipality="Uppsala")} if you want to know how many of those women live in Uppsala (3679).
#'
#' To be specific, when \code{SCB(...)} is called, it looks at the parameters it has been given, and checks if there is some table which has all the columns specified,
#' and all of those columns have all the levels specified. If so, it looks at that table, subsets it to just the entries where all the given columns have one of the
#' given values of that column, and then it sums the values in the "value column" of the rows in that subset. That is, all tables are pivoted to the longest possible
#' form, so that all the actual data is stored in one column, which is the one we sum over -- see the following vignette for more details on this concept:
#' \code{vignette("pivot", package="tidyr")} (requires that you have tidyr installed.).
#'
#' For a full list of the tables you can query, and what columns they have with what levels, see the "See also" section of this documentation.
#'
#' There are a couple of named parameters, which don't query, but instead give some settings for how the query works, like how much output you expect and what
#' to do if more than one table can handle your query. They also permit you to force your query to be sent to a specific table. See the "arguments" section.
#'
#' @param ... All the columns you want to specify. See the description.
#' @param verbose Should extra messages explaining what is going on be printed? If you are getting unexpected results, setting this to TRUE might make things clearer.
#' @param forceTable Force the query to go to a specific table. Useful if your query could be answered by multiple tables, but you know which you want data from. See examples.
#' @param failIfUnable Should an error be thrown if there is no table that can handle your query? The default, FALSE, only returns NA and gives a warning.
#' @param collisionHandlingMode What should happen if more than one table can handle your query? There are four options: "error", the default, simply throws an
#' error if this happens. "namedVector" queries all of the tables that can handle your query, and returns a vector of the results, with names of the table they
#' came from. "consensus" tries the query against all tables that can handle it -- if they all give the same answer, that is what is returned, otherwise, it throws
#' an error. "arbitrary" arbitrarily chooses a table that can handle your query and returns what it says.
#'
#' @examples
#' # How many divorced 24-year-old women were there in Uppsala municipality in 2019?
#' SCB(Municipality = "Uppsala", Age=24, Gender="Women", MaritalStatus = "Divorced")
#' # 14
#' # How much forest was there in Lomma municipality in 2015?
#' SCB(Municipality = "Lomma", LandUseType = "Total forest")
#' @returns A numeric value. What this value represents can vary greatly by your specific query -- it could be anything from an amount of people to a tax rate to an area of land.
#'
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
    if(!canHandleQuery(allLoadedTables[[which(loadedTableNames == forceTable)]], ...)) {
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
      return(query(allLoadedTables[[which(loadedTableNames == forceTable)]], checkHandleable = FALSE, verbose = verbose, ...))
    }
  } else { #forceTable is missing:
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
