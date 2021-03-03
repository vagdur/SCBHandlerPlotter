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

# So, to begin with, the Level class. We set it:
setClass("Level",
         slots = c(
           lvName = "character",
           aliases = "character"
         ),
         prototype = c(
           lvName = NA_character_,
           aliases = NA_character_
         ))
# Define a constructor: (A very simple thing indeed.)
Level <- function(lvName, aliases=character(0)) {
  new("Level", lvName = lvName, aliases = aliases)
}
# Define a validator:
setValidity("Level", function(object) {
  # We only check that the level name is just a single string. We should perhaps check that it is valid
  # as an entry in a csv file, but that is much more complicated, so we "test" that by throwing an error
  # at some later point if the csv handling breaks or something. The easiest way to test if a value will
  # work is to try to use it and see what happens, after all.
  if (length(object@lvName) != 1) {
    return("Name of level should be a single string, not a vector")
  }
})
# Define setter and getter for name and aliases:
setMethod("name", "Level", function(x) x@lvName)
setMethod("name<-", "Level", function(x,value) {
  x@lvName <- value
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
    if (str == x@lvName) {
      x@lvName
    } else if (str %in% x@aliases) {
      x@lvName
    } else {
      NULL
    }
  }))))
})
