# This file defines a generic class for handling SCB tables --
# its constructor just reads in an XML file documenting the table,
# and then loads everything in.
#
# The class implements querying against the table in the syntax of
# the SCB function -- effectively the SCB function is just a wrapper
# for figuring out which table object to run the query against.
#
# Specialised table handlers, for tables that work differently in
# some regard than the default implementation, should inherit from
# this class and only change the queryer method, not any of the
# other methods.

# In order to do this, we actually need several layers of class:
# A table consists of some data, a method to query said data, and
# some information on what the columns of the data are. We implement
# this using a Column class.
# A column has a name and some aliases, and information on what levels it
# can have. In turn, we implement the level information using a Level class,
# an object of which has a name and some aliases.
# This design allows us to just have a dealias() method that takes a potential
# name of a column or level and returns either the real name of it in the underlying
# csv table, or NULL if it is not present. This seems to be the most elegant way
# of handling the possibility of many different names for the same thing.

# We start by defining the generics we will want in several classes:
setGeneric("name", function(x) standardGeneric("name"))
setGeneric("name<-", function(x, value) standardGeneric("name<-"))
setGeneric("aliases", function(x) standardGeneric("aliases"))
setGeneric("aliases<-", function(x,value) standardGeneric("aliases<-"))
setGeneric("dealias", function(x, toDealias) standardGeneric("dealias"))

##########################################################################################
# Definition of the Level class

# Create the class:
setClass("Level",
         slots = c(
           name = "character",
           aliases = "character"
         ),
         prototype = c(
           name = NA_character_,
           aliases = NA_character_
         ))
# Define a constructor: (A very simple thing indeed.)
Level <- function(name, aliases=character(0)) {
  new("Level", name = name, aliases = aliases)
}
# Define a validator:
setValidity("Level", function(object) {
  # We only check that the level name is just a single string. We should perhaps check that it is valid
  # as an entry in a csv file, but that is much more complicated, so we "test" that by throwing an error
  # at some later point if the csv handling breaks or something. The easiest way to test if a value will
  # work is to try to use it and see what happens, after all.
  if (length(object@name) != 1) {
    return("Name of level should be a single string, not a vector")
  }
  return(TRUE)
})
# Define setter and getter for name and aliases:
setMethod("name", "Level", function(x) x@name)
setMethod("name<-", "Level", function(x,value) {
  x@name <- value
  validObject(x)
  x
})
setMethod("aliases", "Level", function(x) x@aliases)
setMethod("aliases<-","Level", function(x, value) {
  x@aliases <- value
  validObject(x)
  x
})
# Define the dealias method. This method takes a string, and checks if it is either the name or the level or one
# of the defined aliases. If it is, it returns the name of the level. Otherwise, it returns NULL. Actually, it does
# this in a vectorised way, so calling dealias(aLevel,c("Men","Women")) returns a list of the results of applying this
# procedure in each argument.
setMethod("dealias","Level", function(x, toDealias) {
  # Not the prettiest code ever, but it does precisely what was described:
  return(unname(unlist(sapply(toDealias, function(str) {
    if (str == x@name) {
      x@name
    } else if (str %in% x@aliases) {
      x@name
    } else {
      NULL
    }
  }))))
})

####################################################################################
# Definition of the Column class:

# Create the class:
setClass("Column",
         slots = c(
           name = "character",
           aliases = "character",
           colLevels = "list",
           levelsType = "character",
           maxLevel = "numeric",
           minLevel = "numeric"
         ),
         prototype = c(
           name = NA_character_,
           aliases = NA_character_,
           colLevels = list(),        # This is named colLevels instead of just levels because "levels" is reserved by the language.
           levelsType = NA_character_,
           maxLevel = NA_real_,
           minLevel = NA_real_
         ))

# Define a constructor:
# Note that this code does absolutely no checking of object validity. That is
# done by the validator, which is called automatically by new()
Column <- function(name, aliases=NA_character_, levelsType, colLevels=list(), maxLevel=NA_real_, minLevel=NA_real_) {
  new("Column", name=name, aliases=aliases, levelsType=levelsType, colLevels=colLevels, maxLevel=maxLevel, minLevel=minLevel)
}
# This wants to know the name and aliases of the column to be created,
# and either a list of Level objects, a pair of numbers, or NA, depending on the levelsType.
# levelsType should be one of "Municipalities", "NumericRange", or "Character", just as in
# the documentation format.
setValidity("Column", function(object) {
  # It should have exactly one name and one levelsType:
  if (length(object@name) != 1) {
    return("Name of column should be a single string, not a vector")
  }
  if (length(object@levelsType) != 1) {
    return("levelsType of column should be a single string, not a vector")
  } else if (!(object@levelsType %in% c("Municipalities", "NumericRange", "Character"))) {
    return("levelsType of column must be one of Municipalities, NumericRange, or Character.")
  }

  if (object@levelsType != "Character") {
    # Only a column with levelsType Character is permitted to have its "colLevels" be not-NA.
    # If a "colLevels" was nonetheless supplied, we ignore it with a warning:
    if (!(length(object@colLevels) == 0)) {
      warning('A "colLevels" was set in Column despite levelsType not being "Character". This will be ignored.')
    }
  }
  if (object@levelsType != "NumericRange") {
    # Only a column with levelsType NumericRange is permitted to have maxLevel or minLevel set.
    # If either is nevertheless supplied, we ignore it with a warning:
    if (!is.na(object@maxLevel)||!is.na(object@minLevel)) {
      warning("A maxLevel or minLevel was set in Column despite levelsType not being NumericRange. This will be ignored.")
    }
  }

  # The preceding checks are actually enough for non-Character columns. For Character columns, we need
  # some more checks:
  if (object@levelsType=="Character") {
    if (length(object@colLevels) == 0) {
      # A column of levelsType character needs to have colLevels specified:
      return("A Column of levelsType Character needs to have colLevels specified.")
    }
    # Next, we check that each object in colLevels really is a level, and is furthermore
    # a valid object of that class:
    for (level in object@colLevels) {
      if (!is(level)=="Level") {
        return("List of colLevels should consist of Level objects")
      }
      validObject(level)
    }
  }
  # If everything worked, we return TRUE:
  return(TRUE)
})

# Define setters and getters for all slots of the Column class: (This entire part is a *huge* DRY violation, but that's just R, I guess?)
setMethod("name","Column", function(x) x@name)
setMethod("name<-", "Column", function(x,value) {
  x@name <- value
  validObject(x)
  x
})
setMethod("aliases", "Column", function(x) x@aliases)
setMethod("aliases<-","Column", function(x, value) {
  x@aliases <- value
  validObject(x)
  x
})

setGeneric("levelsType", function(x) standardGeneric("levelsType"))
setGeneric("levelsType<-", function(x, value) standardGeneric("levelsType<-"))
setMethod("levelsType","Column", function(x) x$levelsType)
setMethod("levelsType<-","Column", function(x, value) {
  x@levelsType <- value
  validObject(x)
  x
})

setGeneric("colLevels", function(x) standardGeneric("colLevels"))
setGeneric("colLevels<-", function(x, value) standardGeneric("colLevels<-"))
setMethod("colLevels","Column", function(x) x@colLevels)
setMethod("colLevels<-","Column", function(x, value) {
  x@colLevels <- value
  validObject(x)
  x
})

setGeneric("minLevel", function(x) standardGeneric("minLevel"))
setGeneric("minLevel<-", function(x, value) standardGeneric("minLevel<-"))
setMethod("minLevel","Column", function(x) x@minLevel)
setMethod("minLevel<-","Column", function(x, value) {
  x@minLevel <- value
  validObject(x)
  x
})

setGeneric("maxLevel", function(x) standardGeneric("maxLevel"))
setGeneric("maxLevel<-", function(x, value) standardGeneric("maxLevel<-"))
setMethod("maxLevel","Column", function(x) x@maxLevel)
setMethod("maxLevel<-","Column", function(x, value) {
  x@maxLevel <- value
  validObject(x)
  x
})

# Finally, we define both a dealias and a levelDealias method for a Column. "dealias" works
# precisely as for a Level -- it looks at the name and aliases of the column, and sends any
# input that matches the name or the aliases to the name, and any other input to NULL.
# levelDealias runs its input against the dealias methods of the levels of the column, trying
# to find if any succeed in dealiasing the input. If any did, it returns that success, otherwise
# it returns NULL. If multiple succeed, it throws an error. (This should be prevented by the data
# documentation validation if the column was constructed from XML documentation, however.)
# levelDealias also works for NumericRange or Municipality columns, where it just checks that the
# input has a valid value and then returns either the input or NULL depending on if it was valid
# or not.
# Both functions are of course vectorised.
setMethod("dealias", "Column", function(x, toDealias) {
  # We can actually reuse the code for dealiasing in a Level verbatim.
  # Normally this might be cause to consider having both inherit from some shared
  # parent class, but the way this might require multiple inheritance seems like
  # more headache than it's worth to not repeat eight lines of code twice.
  return(unname(unlist(sapply(toDealias, function(str) {
    if (str == x@name) {
      x@name
    } else if (str %in% x@aliases) {
      x@name
    } else {
      NULL
    }
  }))))
})

setGeneric("levelDealias", function(x,toDealias) standardGeneric("levelDealias"))
setMethod("levelDealias", "Column", function(x, toDealias) {
  # Again, we use a sapply with a unname(unlist()) wrapped around it, because that makes
  # everything except that part much easier to read.
  return(unname(unlist(sapply(toDealias, function(str) {
    # We actually want to do slightly different things depending on the levelsType here.
    # If it is of type Municipality, we return str if str is a municipality name, otherwise NULL:
    if (x@levelsType == "Municipalities") {
      if (str %in% municipalityNames) {
        str
      } else {
        NULL
      }
    } else if (x@levelsType == "NumericRange") {
      # If it is of type NumericRange, we return NULL if str is not a number. If it is a number, and maxLevel
      # or minLevel is set, we check if str is in the right range. If it is, we return it, otherwise NULL.
      if (!is.numeric(str)) {
        NULL
      } else {
        if (!is.na(x@maxLevel) && str > x@maxLevel) {
          NULL
        } else if (!is.na(x@minLevel) && str < x@minLevel) {
          NULL
        } else {
          str
        }
      }
    } else if (x@levelsType == "Character") {
      # If it is of type Character, we try to dealias str against every level, and check that this succeeds either
      # zero times (in which case we return NULL) or once (in which case we return the successful value). If it succeeds
      # more than once, we throw an error, since there is no unambiguous thing to dealias to.

      dealiasingResults <- unname(unlist(sapply(x@colLevels, function(lev) {
        dealias(lev, str)
      })))
      if (is.null(dealiasingResults)) {
        # So none of the levels could dealias it, so we return NULL:
        NULL
      } else if (length(dealiasingResults) == 1) {
        # WARNING: For some reason, if we return dealiasingResults, it pops out as an n x 1 matrix, but inside the actual function,
        # it is as expected just a vector. This is very surprising, but borne out by inserting a browser() right after the sapply.
        # I wonder why R does that to the return value?
        dealiasingResults[1]
      } else if (length(dealiasingResults) > 1) {
        # More than one level succeeded in dealiasing! This should not happen -- in a table constructed from an XML file
        # that has been tested, this is impossible. We throw an error.
        stop("More than one level succeeded in dealiasing. Are you sure your column has been constructed correctly?")
      } else {
        # This really should not be possible. Since there should be no way to reach this point, we throw an error here
        # to be alerted if our assumptions that this is unreachable are violated:
        stop("Reached case in conditional that should not be reachable. This should never run, so something weird has happened.")
      }
    } else {
      # This should be impossible -- levelsType has to be one of those three values
      stop("Column has invalid levelsType.")
    }
  }))))
})

###################################################################################################################################
# Definition of the DocumentedTable class
# The name is chosen to specify what kind of table we're dealing with.
# A DocumentedTable contains a bunch of things:
#   1.  A name of the table
#   2.  A description of the table
#   3.  A source for the table
#   4.  A year for when the data is from
#   5.  A data.frame containing the actual data -- this should not be accessed or modified by the user.
#   6.  A list of Column objects documenting the columns, each of which contains more information
#   7.  A name of the column in the data.frame containing the numeric data
#
# There are a few methods that work on a DocumentedTable:
#   1.  Getters and setters for the first four points on the list above
#   2.  columnDealias: Implements dealiasing of columns. See the comment before that method for more detail.
#   3.  query: Implements querying against the table. See the comment before that function, or the documentation of the library for the
#       general format of querying.
#   4.  canHandleQuery: Checks whether a query makes sense against the table, and returns true or false without actually executing the query.
#       Used to route queries to the right table when we have a bunch of them. Same arguments as Query, of course.

# We start by defining the class:
setOldClass("data.frame") # Not sure if we really need this? Better safe than sorry.
setClass("DocumentedTable",
         slots = c(
           name = "character",
           description = "character",
           dataSource = "character",
           dataYear = "numeric",
           csvData = "data.frame",
           tableColumns = "list",
           valueColumn = "character"
         ),
         prototype = c(
           name = NA_character_,
           description = NA_character_,
           dataSource = NA_character_,
           dataYear = NA_real_,
           csvData = data.frame(),
           tableColumns = list(),
           valueColumn = NA_character_
         ))
# Define a constructor function:
DocumentedTable <- function(name = NA_character_,
                            description = NA_character_,
                            dataSource = NA_character_,
                            dataYear = NA_real_,
                            csvData = data.frame(),
                            tableColumns = list(),
                            valueColumn = NA_character_) {
  new("DocumentedTable", name = name, description = description, dataSource = dataSource,
      dataYear = dataYear, csvData = csvData, tableColumns = tableColumns, valueColumn = valueColumn)
}
# Define a validator:
setValidity("DocumentedTable", function(object) {
  # This does fewer tests than it perhaps strictly should, because for DocumentedTable objects constructed
  # from an XML, all of these things and more are already tested by the test suite for the XML documentation.
  # We restrict ourselves to a minimum of tests to make sure it isn't completely broken here.

  # For the name, description, dataSource, and dataYear, there are no requirements other than their types,
  # and that they be a single string, not multiple:
  if (length(object@name) != 1) {
    return("DocumentedTable should have one name, not zero or multiple.")
  }
  if (length(object@description) != 1) {
    return("DocumentedTable should have one description, not zero or multiple.")
  }
  if (length(object@dataSource) != 1) {
    return("DocumentedTable should have one data source, not zero or multiple.")
  }
  if (length(object@dataYear) != 1) {
    return("DocumentedTable should have one data year, not zero or multiple.")
  }
  # There are however a few checks needed on the remaining slots. We begin by checking that tableColumns is
  # indeed a list of valid Column objects, and the name of each occurs as a
  for (tableColumn in object@tableColumns) {
    if(!is(tableColumn) == "Column") {
      return("One of the elements of tableColumns is not a Column object.")
    }
    validObject(tableColumn)
    if(!(name(tableColumn) %in% colnames(object@csvData))) {
      return("One of the elements of tableColumns has a name that does not occur in the column names of the CSV data.")
    }
  }
  # Having checked that, we now check that valueColumn is indeed one of the column names of our data frame. First,
  # however, we check that it is a single entry, not multiple:
  if (length(object@valueColumn) != 1) {
    return("DocumentedTable should have one valueColumn, not zero or multiple.")
  }
  if (!(object@valueColumn %in% colnames(object@csvData))) {
    return("Specified valueColumn does not occur as a column of the csvData.")
  }

  # Everything was okay, so we return TRUE:
  return(TRUE)
})
# Define setter and getter methods: (I hate how un-DRY this code is... Might be possible to just do this with some fancy meta-programming?)
setMethod("name","DocumentedTable", function(x) { x@name })
setMethod("name<-", "DocumentedTable", function(x, value) {
  x@name <- value
  validObject(x)
  x
})

setGeneric("description", function(x) standardGeneric("description"))
setGeneric("description<-", function(x, value) standardGeneric("description<-"))
setMethod("description","DocumentedTable", function(x) x@description)
setMethod("description<-","DocumentedTable", function(x, value) {
  x@description <- value
  validObject(x)
  x
})

setGeneric("dataSource", function(x) standardGeneric("dataSource"))
setGeneric("dataSource<-", function(x, value) standardGeneric("dataSource<-"))
setMethod("dataSource","DocumentedTable", function(x) x@dataSource)
setMethod("dataSource<-","DocumentedTable", function(x, value) {
  x@dataSource <- value
  validObject(x)
  x
})

setGeneric("dataYear", function(x) standardGeneric("dataYear"))
setGeneric("dataYear<-", function(x, value) standardGeneric("dataYear<-"))
setMethod("dataYear","DocumentedTable", function(x) x@dataYear)
setMethod("dataYear<-","DocumentedTable", function(x, value) {
  x@dataYear <- value
  validObject(x)
  x
})

setGeneric("csvData", function(x) standardGeneric("csvData"))
setGeneric("csvData<-", function(x, value) standardGeneric("csvData<-"))
setMethod("csvData","DocumentedTable", function(x) x@csvData)
setMethod("csvData<-","DocumentedTable", function(x, value) {
  x@csvData <- value
  validObject(x)
  x
})

setGeneric("tableColumns", function(x) standardGeneric("tableColumns"))
setGeneric("tableColumns<-", function(x, value) standardGeneric("tableColumns<-"))
setMethod("tableColumns","DocumentedTable", function(x) x@tableColumns)
setMethod("tableColumns<-","DocumentedTable", function(x, value) {
  x@tableColumns <- value
  validObject(x)
  x
})

setGeneric("valueColumn", function(x) standardGeneric("valueColumn"))
setGeneric("valueColumn<-", function(x, value) standardGeneric("valueColumn<-"))
setMethod("valueColumn","DocumentedTable", function(x) x@valueColumn)
setMethod("valueColumn<-","DocumentedTable", function(x, value) {
  x@valueColumn <- value
  validObject(x)
  x
})

# Define columnDealias:
setGeneric("columnDealias", function(x, toDealias) standardGeneric("columnDealias"))
setMethod("columnDealias", "DocumentedTable", function(x, toDealias) {
  # As before, we use this slight hack to easily vectorise the function, and the rest of the code
  # can pretend like the arguments we got were x and str:
  unname(unlist(sapply(toDealias, function(str) {
    # This is essentially a copy-paste of the code for levelDealias -- perhaps another argument
    # to try to code this with multiple-inheritance from a "dealiasable" class as well as the
    # normal inheritance? Worth thinking about.

    # We try to dealias str against every column, and then see if it succeeded zero times (in
    # which case we return NULL), once (in which case we return the value of that success), or
    # more than once, in which case we throw an error since we couldn't find an unambiguous
    # result but really should be able to if the table is set up right.

    dealiasingResults <- unname(unlist(sapply(x@tableColumns, function(col) {
      dealias(col, str)
    })))
    if (is.null(dealiasingResults)) {
      # So none of the columns could dealias it, so we return NULL:
      NULL
    } else if (length(dealiasingResults) == 1) {
      # WARNING: For some reason, if we return dealiasingResults, it pops out as an n x 1 matrix, but inside the actual function,
      # it is as expected just a vector. This is very surprising, but borne out by inserting a browser() right after the sapply.
      # I wonder why R does that to the return value?
      dealiasingResults[1]
    } else if (length(dealiasingResults) > 1) {
      # More than one column succeeded in dealiasing! This should not happen -- in a table constructed from an XML file
      # that has been tested, this is impossible. We throw an error.
      stop("More than one column succeeded in dealiasing. Are you sure your column has been constructed correctly?")
    } else {
      # This really should not be possible. Since there should be no way to reach this point, we throw an error here
      # to be alerted if our assumptions that this is unreachable are violated:
      stop("Reached case in conditional that should not be reachable. This should never run, so something weird has happened.")
    }
  })))
})

# Define canHandleQuery:
setGeneric("canHandleQuery", function(x, ...) standardGeneric("canHandleQuery"))
setMethod("canHandleQuery", "DocumentedTable", function(x, ...) {
  # This function needs to do some odd things with ... -- at some point at least
  # we will want to test that we are using all arguments of ... . There is an experimental
  # package to do this.
  #TODO: Implement this.

  # First we pull out the names of the arguments we got: (Note that "list(...)" evaluates all the arguments. Right now
  # we don't care about this, but this does prevent us from doing metaprogramming dark magics to the stuff in list(...))
  argumentNames = names(list(...))
  # Then we check that we can dealias each of these to a proper column name:
  dealiasedArgumentNames <- columnDealias(x, argumentNames)
  if (length(dealiasedArgumentNames) != length(argumentNames)) {
    # At least one argument name failed to dealias, so we don't know what to do with this, and
    # we return FALSE to indicate the table cannot handle this query:
    return(FALSE)
  }
  # Assuming we got to this point, each argument name corresponds to a column we do have. Time to check that
  # each argument value corresponds to a (potential) level of that column:
  argumentValues <- list(...)
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

# Define query:
setGeneric("query", function(x, verbose=NA, checkHandleable=NA, failIfUnable=NA, ...) standardGeneric("query"))
setMethod("query", "DocumentedTable", function(x, verbose=FALSE, checkHandleable=TRUE, failIfUnable=FALSE, ...) {
  #TODO: Make verbose more useful. Right now it only messages if the query matches no results, but it would make
  # sense for it to also warn if one of multiple specified values did not match anything. (I.e. if we run query(a=c("b","c"))
  # and find some posts with "b" but none with "c", that might be something we want to print a message about?)


  # First, we check if we can even handle the query, in the case where checkHandleable is TRUE:
  if (checkHandleable) {
    if (!canHandleQuery(x, ...)) {
      # So we can't, what do we do? We either throw an error or just a warning, depending on the failIfUnable parameter.
      # In the latter case we return NA, to mark no data was found.
      if (failIfUnable) {
        stop("Unable to handle this query, either a column name or a level did not match what the table contains.")
      } else {
        warning("Unable to handle this query, either a column name or a level did not match what the table contains. Returning NA.")
        return(NA_integer_)
      }
    }
  }
  # So we either checked handleability and passed, or we were told by the checkHandleable that this has already been checked before
  # this function was called and we can safely steam on ahead.

  # First thing we need to do is grab our parameters from ... and dealias them into something our raw CSV can handle:
  argumentNames = names(list(...))
  argumentValues <- list(...)

  # Then, we just loop over all our parameters and subset down the raw csvData according to the specifications of the parameters.
  #TODO: I strongly suspect this is a really really crappy algorithm. It should be replaced by something better, and perhaps
  # more idiomatic in R as well.
  runningSubset <- x@csvData
  for (argumentName in argumentNames) {
    # First we dealias the argumentName
    dealiasedArgumentName <- columnDealias(x, argumentName)
    # Then we loop over all the columns to find the right one:
    #TODO: This is a dumb algorithm, even if the algorithm this is in is kept, this part should be replaced.
    for (column in x@tableColumns) {
      if (name(column) == dealiasedArgumentName) {
        # So this is the column we were looking for!
        # Now we can dealias also the argument values:
        dealiasedArgumentValue <- levelDealias(column, argumentValues[[argumentName]])

        # Okay, so now we have both the right name for the column and the right name for the levels,
        # so now we can just do the subsetting:
        desiredIndices <- (runningSubset[,dealiasedArgumentName] %in% dealiasedArgumentValue)
        runningSubset <- runningSubset[desiredIndices,]
      }
    }
  }

  # So now we have the subset that filtered down to. This is the easiest step: We just sum over
  # the valueColumn in this subset and return that number. If verbose=TRUE, we also check if the
  # query actually matched any entries, and post a message if it did not. Additionally we check if
  # the query matched some rows that have valueColumn set to NA, and if so message about that.
  if (verbose) {
    if (nrow(runningSubset) == 0) {
      message("The query, while possible, did not match any rows in the data.")
    }
    if (any(is.na(runningSubset[,x@valueColumn]))) {
      message(paste("The query, while possible, matched",sum(is.na(runningSubset[,x@valueColumn])),"rows whose value were NA, so the sum over all matching rows is NA."))
    }
  }
  return(sum(runningSubset[,x@valueColumn]))
})
