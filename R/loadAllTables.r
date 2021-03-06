# This file just contains the code to load all tables into a list of DocumentedTable objects,
# so that they can be queried.


# We need to wrap this part in a function, so that the variable extdataLocation doesn't stick
# around in the environment. Keeping a record of its value is verboten by the install routine
# for R libraries -- it's something about preventing a race condition when some packages are
# very slow to load, see: https://developer.r-project.org/Blog/public/2019/02/14/staged-install/index.html
# For our purposes, the solution isn't too hard, just wrap it: (So this function is not intended
# to ever be called outside of this specific file.)
loadAllTables <- function() {
  # We assume that every file in extdata with the .xml file ending is a specification of a table:
  extdataLocation <- system.file("extdata", package = "SCBHandlerPlotter")
  tablesToCheck <- list.files(extdataLocation, pattern="*\\.xml$")

  # We define the list of all loaded tables:
  allLoadedTables <- list()

  # Then we just loop over all the files, run createDocumentedTableFromXML on each of them, and append the
  # resulting documentedTable to our list
  for (tableToCheck in tablesToCheck) {
    filename <- paste(extdataLocation,"/",tableToCheck,sep="")
    loadedTable <- createDocumentedTableFromXML(filename)
    allLoadedTables[[length(allLoadedTables) + 1]] <- loadedTable
  }
  return(allLoadedTables)
}
# Now we call the function and store the result:
allLoadedTables <- loadAllTables()

# Finally, it is convenient to have a vector of the names of tables, to speed up queries with forceTable set,
# since otherwise each such query would have to loop through the list of tables anew:
loadedTableNames <- unlist(lapply(allLoadedTables, name))
