# This file contains a script that takes an XML documentation file of a table
# and produces a documentation file in the .Rd format, to make it more human
# readable. Note that this function won't be run ever by the package --
# instead it should be getting run just before build time for the package,
# to create the .Rd files to go with the package.
# Still a bit uncertain about where to store this file, actually, and how
# to set it up to run at the right time.

# We create this as a function which reads in the filename of the XML documentation,
# and returns the documentation as one big string. This is to make it slightly easier
# to build tests for it -- the function can then presumably be called from command line
# and piped into the right file.
writeTextDocumentation <- function(filename) {
  # The documentation can be built incrementally by just appending stuff to a string:
  doc <- ""

  # Having created all the documentation, we now just return the string:
  return(doc)
}
