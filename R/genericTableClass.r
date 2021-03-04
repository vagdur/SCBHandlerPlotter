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
    if (x@levelsType == "Municipality") {
      if (str %in% municipalityNames) {
        str
      } else {
        NULL
      }
    }
    # If it is of type NumericRange, we return NULL if str is not a number. If it is a number, and maxLevel
    # or minLevel is set, we check if str is in the right range. If it is, we return it, otherwise NULL.
    if (x@levelsType == "NumericRange") {
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
    }
    # If it is of type Character, we try to dealias str against every level, and check that this succeeds either
    # zero times (in which case we return NULL) or once (in which case we return the successful value). If it succeeds
    # more than once, we throw an error, since there is no unambiguous thing to dealias to.
    if (x@levelsType == "Character") {
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
    }
  }))))
})
