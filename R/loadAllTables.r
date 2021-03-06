# This file just contains the code to load all tables into a list of DocumentedTable objects,
# so that they can be queried. No function definitions at all, all the real work is done elsewhere.

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
