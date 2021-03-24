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

# Throughout we will be using all the 'methods'-package functions to create our
# S4 classes, so instead of writing methods:: every time, we just import it:
#' @import methods

##########################################################################################
# Definition of the Dealiasable class

# One thing that all three layers of a DocumentedTable (the table itself, its columns, and
# the levels of a column) have in common is that they have a name and some aliases, and we
# will want to dealias strings against them. That is, we will want a method that takes a string
# and checks if that string is either the name of the object or in its list of aliases, and
# either returns the object's name if it is or NULL if it is not.
#
# So, since this is a shared feature of several classes, it makes sense to have this behaviour
# defined by a superclass from which they all inherit. We implement this as a virtual class,
# since there is no reason to want to have objects that are of just this class.

setClass("Dealiasable",
         contains = "VIRTUAL", # This is a virtual class
         slots = c(name = "character",
                   aliases = "character"),
         prototype = c(name = NA_character_,
                       aliases = character(0)))
# Since the class is virtual, we do not define any constructor of it -- no objects can exist.
# We do however define a validity function for it:
setValidity("Dealiasable", function(object) {
  # We have two requirements:
  #   1. That name be a single value, not a vector, and that value not be NA or the empty string.
  #   2. That aliases not contain any NA or empty strings
  isNAorEmptyString <- function(x) {
    if (is.na(x))
      return(TRUE)
    if (x == "")
      return(TRUE)
    return(FALSE)
  }
  if (length(object@name) != 1) {
    return("The length of name must be exactly one.")
  }
  if (isNAorEmptyString(object@name)) {
    return("name must not be NA or empty string.")
  }
  if (length(object@aliases) > 0) {
    if (any(sapply(object@aliases, function(alias) isNAorEmptyString(alias)))) {
      return("aliases must not contain NA or empty string.")
    }
  }
  return(TRUE)
})

# As we said, there are a few methods that should be shared, so we define those:
setGeneric("name", function(x) standardGeneric("name"))
setGeneric("name<-", function(x, value) standardGeneric("name<-"))
setGeneric("aliases", function(x) standardGeneric("aliases"))
setGeneric("aliases<-", function(x,value) standardGeneric("aliases<-"))
setGeneric("dealias", function(x, toDealias) standardGeneric("dealias"))

# Define setter and getter for name and aliases:
setMethod("name", "Dealiasable", function(x) x@name)
setMethod("name<-", "Dealiasable", function(x,value) {
  x@name <- value
  validObject(x)
  x
})
setMethod("aliases", "Dealiasable", function(x) x@aliases)
setMethod("aliases<-", "Dealiasable", function(x, value) {
  x@aliases <- value
  validObject(x)
  x
})
# Define the dealias method. This method takes a string, and checks if it is either the name of the object or one
# of the defined aliases. If it is, it returns the name of the object. Otherwise, it returns NULL. Actually, it does
# this in a vectorised way, so calling dealias(object,c("Men","Women")) returns a list of the results of applying this
# procedure in each argument.
setMethod("dealias","Dealiasable", function(x, toDealias) {
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

##########################################################################################
# Definition of the Level class

# A level contains no information beyond its name and aliases, so this just inherits from
# Dealiasable without new slots or methods:

# Create the class:
setClass("Level",
         contains = "Dealiasable")
# Define a constructor: (A very simple thing indeed.)
Level <- function(name, aliases=character(0)) {
  new("Level", name = name, aliases = aliases)
}

####################################################################################
# Definition of the Column class, from which CharacterColumn, NumericColumn, and
# MunicipalitiesColumn inherit.

# Create the base class, which just inherits straight from Dealiasable to get a name and aliases
# slot. Then the three subclasses can add new slots to contain information specific to their type
# of column.
# Note that this is a virtual class -- there can be no objects solely of class Column, only of
# the three classes that inherit from it.
setClass("Column",
         contains = c("Dealiasable", "VIRTUAL"))
# There is no constructor of pure Column objects, since such don't exist. Instead, the Column() function
# figures out from its arguments which type of column you want. Thus, we define it after we define the subclasses.

# For each subclass we define it, a validity function, and a very barebones constructor.
# Note that the validity of the shared slots name and aliases is tested by the validity function of the Dealiasable
# class, the grandparent of these classes.

# A CharacterColumn additionally contains a list of its levels:
setClass("CharacterColumn",
         contains = "Column", # This is the R syntax for making the class inherit from Column, so it will contain all
                              # the same slots and method calls dispatch upwards to Column if they aren't implemented for
                              # CharacterColumn.
         slots = c(colLevels = "list"), # This is named colLevels instead of just levels because "levels" is reserved by the language.
         prototype = c(colLevels = list())
         )
setValidity("CharacterColumn", function(object) {
  # Second, we check that the colLevels slot has length greater than zero and each element is a valid Level object:
  if (length(object@colLevels) == 0) {
    return("A CharacterColumn object needs to have at least one level defined.")
  }
  # Some very nested code here, but not too horrible, I don't think:
  if (any(!(sapply(object@colLevels, function(x) {
      if ("Level" %in% is(x)) {
        validObject(x)
      } else {
        FALSE
      }
    })))) {
    return("At least one of the colLevels is not an object of type Level.")
  }
  return(TRUE)
})
CharacterColumn <- function(name, aliases=character(0), colLevels) {
  new("CharacterColumn", name = name, aliases = aliases, colLevels = colLevels)
}

# A NumericColumn may additionally contain information on a maxLevel or minLevel:
setClass("NumericColumn",
         contains = "Column",
         slots = c(maxLevel = "numeric",
                   minLevel = "numeric"),
         prototype = c(maxLevel = numeric(0),
                       minLevel = numeric(0)))
setValidity("NumericColumn", function(object) {
  # The only thing we need to check here is that either maxLevel is omitted, or it is of length 1, and the same for minLevel:
  if (length(object@maxLevel) > 1) {
    return("maxLevel should either be omitted (of length 0) or of length 1.")
  }
  if (length(object@minLevel) > 1) {
    return("minLevel should either be omitted (of length 0) or of length 1.")
  }
  return(TRUE)
})
NumericColumn <- function(name, aliases=character(0), minLevel = numeric(0), maxLevel = numeric(0)) {
  new("NumericColumn", name = name, aliases = aliases, minLevel = minLevel, maxLevel = maxLevel)
}

# A MunicipalitiesColumn doesn't need to contain any other information than that it is
# indeed a column of municipalities:
setClass("MunicipalitiesColumn",
         contains = "Column")
# Since a MunicipalitiesColumn has no extra slots compared to a Column, all the validity testing we
# need to do is implemented by the validity of the Dealiasable class.
#setValidity("MunicipalitiesColumn", function(object) {
#
#})
MunicipalitiesColumn <- function(name, aliases=character(0)) {
  new("MunicipalitiesColumn", name = name, aliases = aliases)
}


# Having defined all three subclasses, we now define a "constructor" for the superclass, which actually just takes
# an argument for which type of Column object to create: (This is in order to be compatible with code written for
# the previous version, where there was just one class containing a flag for type and lots of conditionals.)
Column <- function(levelsType,
                   name,
                   aliases=character(0),
                   colLevels=list(),
                   maxLevel=numeric(0),
                   minLevel=numeric(0)) {
  # What we need to do is just pass the arguments onwards to the right constructor:
  if (levelsType == "NumericRange") {
    return(NumericColumn(name = name, aliases = aliases, maxLevel = maxLevel, minLevel = minLevel))
  } else if (levelsType == "Character") {
    return(CharacterColumn(name = name, aliases = aliases, colLevels = colLevels))
  } else if (levelsType == "Municipalities") {
    return(MunicipalitiesColumn(name = name, aliases = aliases))
  } else {
    stop("You supplied an invalid argument for levelsType -- it should be one of 'NumericRange', 'Character', or 'Municipalities'.")
  }
}

# Define setters and getters of the slots of the subclasses:
setGeneric("colLevels", function(x) standardGeneric("colLevels"))
setGeneric("colLevels<-", function(x, value) standardGeneric("colLevels<-"))
setMethod("colLevels","CharacterColumn", function(x) x@colLevels)
setMethod("colLevels<-","CharacterColumn", function(x, value) {
  x@colLevels <- value
  validObject(x)
  x
})

setGeneric("minLevel", function(x) standardGeneric("minLevel"))
setGeneric("minLevel<-", function(x, value) standardGeneric("minLevel<-"))
setMethod("minLevel","NumericColumn", function(x) x@minLevel)
setMethod("minLevel<-","NumericColumn", function(x, value) {
  x@minLevel <- value
  validObject(x)
  x
})

setGeneric("maxLevel", function(x) standardGeneric("maxLevel"))
setGeneric("maxLevel<-", function(x, value) standardGeneric("maxLevel<-"))
setMethod("maxLevel","NumericColumn", function(x) x@maxLevel)
setMethod("maxLevel<-","NumericColumn", function(x, value) {
  x@maxLevel <- value
  validObject(x)
  x
})

# Finally, we define a levelDealias function for each subclass. These functions all take as argument
# a potential value in a column, and either return NULL if it is not actually a level of the column,
# or return a standardised name for the argument if it is in the column. For NumericColumn and MunicipalitiesColumn,
# this just means returning the argument, while for CharacterColumn we need to dealias against its constituent
# levels.

setGeneric("levelDealias", function(x,toDealias) standardGeneric("levelDealias"))
setMethod("levelDealias", "NumericColumn", function(x, toDealias) {
  return(unname(unlist(sapply(toDealias, function(str) {
    # What we need to check is two things -- that our input is numeric (if not,
    # we return NULL) and that it falls between minLevel and maxLevel if those have
    # been set.
    if (!is.numeric(str)) {
      NULL
    } else {
      if (length(x@maxLevel) == 1 && str > x@maxLevel) {
        NULL
      } else if (length(x@minLevel) == 1 && str < x@minLevel) {
        NULL
      } else {
        str
      }
    }
  }))))
})

setMethod("levelDealias", "CharacterColumn", function(x, toDealias) {
  return(unname(unlist(sapply(toDealias, function(str) {
    # Here, we try to dealias str against every level, and check that this succeeds either
    # zero times (in which case we return NULL) or once (in which case we return the successful value). If it succeeds
    # more than once, we throw an error, since there is no unambiguous thing to dealias to.

    dealiasingResults <- unname(unlist(sapply(x@colLevels, function(lev) {
      dealias(lev, str)
    })))
    if (is.null(dealiasingResults)) {
      # So none of the levels could dealias it, so we return NULL:
      NULL
    } else if (length(dealiasingResults) == 1) {
      # Note: For some reason, if we return dealiasingResults, it pops out as an n x 1 matrix, but inside the actual function,
      # it is as expected just a vector. This is very surprising, but borne out by inserting a browser() right after the sapply.
      # I wonder why R does that to the return value?
      dealiasingResults[1]
    } else if (length(dealiasingResults) > 1) {
      # More than one level succeeded in dealiasing! This should not happen -- in a table constructed from an XML file
      # that has been tested, this is impossible. We throw an error.
      stop("More than one level succeeded in dealiasing. Are you sure your column has been constructed correctly?")
    } else { # nocov start
      # This really should not be possible. Since there should be no way to reach this point, we throw an error here
      # to be alerted if our assumptions that this is unreachable are violated. Because this should be unreachable,
      # there should be no way to write a test that covers this case, and thus we exclude it from our measurement
      # of test coverage.
      stop("Reached case in conditional that should not be reachable. This should never run, so something weird has happened.")
    } # nocov end
  }))))
})

setMethod("levelDealias", "MunicipalitiesColumn", function(x, toDealias) {
  return(unname(unlist(sapply(toDealias, function(str) {
    # Here, the check we need to do is very simple: If the thing we got is the
    # name of a municipality, we return that, otherwise, we return NULL.
    if (str %in% municipalityNames) {
      str
    } else {
      NULL
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
#
# DocumentedTable gets its name slot and setter/getter by inheritance from Dealiasable, so in fact it also has an aliases slot and a dealias
# method, but this is not currently supported by the XML documentation format from which DocumentedTable objects are created, so in practice
# the aliases slots is always empty, and dealias() does nothing for this class.

# We start by defining the class:
setOldClass("data.frame") # Not sure if we really need this? Better safe than sorry.
setClass("DocumentedTable",
         contains = "Dealiasable",
         slots = c(
           description = "character",
           dataSource = "character",
           dataYear = "character",
           csvData = "data.frame",
           tableColumns = "list",
           valueColumn = "character"
         ),
         prototype = c(
           description = NA_character_,
           dataSource = NA_character_,
           dataYear = NA_character_,
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
  # We'll frequently, but not always, want data year as just a number for a year, so for convenience we permit passing numbers
  # to the constructor too, and the constructor converts those to character:
  if (is.numeric(dataYear)) {
    dataYear <- as.character(dataYear)
  }
  new("DocumentedTable", name = name, description = description, dataSource = dataSource,
      dataYear = dataYear, csvData = csvData, tableColumns = tableColumns, valueColumn = valueColumn)
}
# Define a validator:
setValidity("DocumentedTable", function(object) {
  # This does fewer tests than it perhaps strictly should, because for DocumentedTable objects constructed
  # from an XML, all of these things and more are already tested by the test suite for the XML documentation.
  # We restrict ourselves to a minimum of tests to make sure it isn't completely broken here.

  # For the name, description, dataSource, and dataYear, there are no requirements other than their types,
  # and that they be a single string, not multiple: (The requirement on the name is checked by the validity
  # of the superclass Dealiasable.)
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
    if(!("Column" %in% is(tableColumn))) {
      # NOTE: This permits objects which inherit from Column. Note that "is(tableColumn)=="Column"" does not check
      # for exactly the class Column, but is rather a bug. Cf. the note in the validity function of the Column class
      # about this same issue.
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

# It is useful for objects of this class to show compactly, instead of vomiting out the result of show() on each
# of its slots:
setMethod("show", "DocumentedTable", function(object) {
  cat(paste("A DocumentedTable object named",name(object),"containing",length(tableColumns(object)),"columns."))
})

# Define setter and getter methods: (I hate how un-DRY this code is... Might be possible to just do this with some fancy meta-programming?)
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
  # We'll frequently, but not always, want data year as just a number for a year, so for convenience we permit passing numbers
  # to the setter too, and the setter converts those to character:
  if (is.numeric(value)) {
    value <- as.character(value)
  }
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
  stop("Setting of valueColumn slot is not implemented. Either assign to x@valueColumn if you really know what you are doing, or get it right the first time when constructing the table.")
})

# Define columnDealias:
setGeneric("columnDealias", function(x, toDealias, getColumn = FALSE) standardGeneric("columnDealias"))
setMethod("columnDealias", "DocumentedTable", function(x, toDealias, getColumn = FALSE) {
  # If toDealias is an empty vector, then there is nothing to be done: We won't find any
  # columns, since there is nothing to look for, so we return our default failure value,
  # NULL:

  if (length(toDealias) == 0) {
    return(NULL)
  }

  result <- lapply(toDealias, function(str) {
    # We try to dealias str against every column, and then see if it succeeded zero times (in
    # which case we return NULL), once (in which case we return the value of that success, or
    # the column that succeeded if getColumn is TRUE), or more than once, in which case we throw
    # an error since we couldn't find an unambiguous result but really should be able to if the
    # table is set up right.

    # We actually cheat a little bit, to make our algorithm simpler: We always run as if getColumn
    # were true, and then if it is false, we do sapply(result, name) to get the names and return
    # that.

    dealiasingResults <- lapply(x@tableColumns, function(col) {
      dealiasResult <- dealias(col, str)
      if (is.null(dealiasResult)) {
        NULL
      } else {
        col
      }
    })

    # Since the above was a lapply, we get a list with a bunch of NULLs in it. We remove those:
    dealiasingResults <- dealiasingResults[-which(sapply(dealiasingResults, is.null))]

    # Now, we check how many columns were able to dealias that name:
    if (length(dealiasingResults) == 0) {
      # So none of the columns could dealias it, so we return NULL:
      NULL
    } else if (length(dealiasingResults) == 1) {
      dealiasingResults[[1]]
    } else if (length(dealiasingResults) > 1) {
      # More than one column succeeded in dealiasing! This should not happen -- in a table constructed from an XML file
      # that has been tested, this is impossible. We throw an error.
      stop("More than one column succeeded in dealiasing. Are you sure your table has been constructed correctly?")
    } else { #nocov start
      # This really should not be possible. Since there should be no way to reach this point, we throw an error here
      # to be alerted if our assumptions that this is unreachable are violated:
      stop("Reached case in conditional that should not be reachable. This should never run, so something weird has happened.")
      # Since this case should be completely impossible, so should writing a test that hits it. Thus we exclude it from test coverage stats.
    } # nocov end
  })

  # Since the above was a lapply, we get a list which might contain a bunch of NULLs. We want to remove those.
  # The naive approach would be to just run:
  #result <- result[-which(sapply(result, is.null))]
  # This, however, will not work: For some reason, if there is no NULL, the which() will return integer(0),
  # and instead of the expected outcome of result[-integer(0)] -- doing nothing, since we supplied an empty
  # list of arguments to remove, we for some reason get the empty list back. So we need to first check if there
  # are any null elements, and only then remove them:
  nullResults <- which(sapply(result, is.null))
  if (length(nullResults) >  0) {
    result <- result[-nullResults]
  }

  # If no column succeeded, we are expected to return NULL:
  if (length(result) == 0) {
    return(NULL)
  }

  # If we want to return the actual Column objects, we can just return result,
  # otherwise, we sapply name to result to get a vector of column names instead:
  if (getColumn) {
    return(result)
  } else {
    return(sapply(result, name))
  }
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
        return(NA_real_)
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
