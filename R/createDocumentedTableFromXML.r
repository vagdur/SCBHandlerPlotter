# This file contains a single function, which takes as its argument the path to
# an XML file documenting a table, and returns a DocumentedTable object created
# based on that XML, which can be queried against.
# Note that this function assumes the XML file has already been validated by the
# tests of documentation! If you try to pass it an untested XML file, or one in
# an incorrect format, weird things may happen.

#TODO: Is it possible to call on the tests of the library to test the XML at runtime?
# It probably isn't, but it would cleanly solve the issue of validating input here.

createDocumentedTableFromXML <- function(pathToXML) {
  # As step zero, we load the XML file into a variable, and immediately transform it
  # into a list, which is easier to work with in R. We also immediately look into the
  # main tag, <table>, 'cause that's where all the data is.

  xmlList <- xml2::as_list(xml2::read_xml(pathToXML, encoding="UTF8"))$table

  # Then, we read in the general properties of the table, i.e. name, description, etc:
  tableName <- xmlList$name[[1]][1]
  tableDescription <- xmlList$description[[1]][1]
  tableDataSource <- xmlList$source[[1]][1]
  tableDataYear <- as.integer(xmlList$year[[1]][1])

  # We also read in the valueColumn specified by the XML, though that hides a few tags down:
  tableValueColumn <- xmlList$columns$valueColumn$colname[[1]][1]

  # Next, we load the CSV data:
  csvDataFilename <- xmlList$filename[[1]][1]
  tableCsvData <- utils::read.csv(paste(dirname(pathToXML),"/",csvDataFilename, sep=""), fileEncoding = "UTF8")

  # Now, it is time to start constructing the Column objects based on what the XML tells us:
  tableCols <- list()
  xmlListCols <- xmlList$columns
  for (xmlListColIndex in 1:(length(xmlListCols)-1)) { # The -1 because the final item is the <valueColumn> tag which we already handled
    xmlListColumn <- xmlListCols[[xmlListColIndex]]
    curColName <- xmlListColumn$colname[[1]][1]
    # The aliases of the column are split across one <identifier> tag and one or more <alias> tags -- this makes sense for writing the
    # documentation in a human-readable format, but is a little bit annoying to us. So we first load the <identifier> tag's contents into
    # our list of aliases, and then check if there are any <alias> tags, and if so, we load them:
    curColAliases <- xmlListColumn$identifier[[1]][1]
    if (length(xmlListColumn) > 3) { # There's always <identifier>, <colname>, and <levels> -- any additional tags are <alias>
      # A sapply here gets us out of having to loop again:
      curColAliasTagContents <- sapply(c(3:(length(xmlListColumn)-1)), function(tagIndex) xmlListColumn[[tagIndex]][[1]][1])
      curColAliases <- c(curColAliases, curColAliasTagContents)
    }

    # Now that we have the aliases, we figure out the levelsType, and depending on what it is load the additional data we needed:
    curColLevels <- xmlListColumn$levels
    curColLevelsType <- curColLevels$levelsType[[1]][1]
    if (curColLevelsType == "Municipalities") {
      # In this case, we actually have all the information we need already, and can just create the Column object:
      currentColumnObject <- Column(name = curColName, aliases = curColAliases, levelsType = curColLevelsType)
    } else if (curColLevelsType == "NumericRange") {
      # In this case, the only complication that can occur is that a maxLevel or minLevel is specified. We begin by
      # creating the column without those, and then set them if necessary:
      currentColumnObject <- Column(name = curColName, aliases = curColAliases, levelsType = curColLevelsType)
      if (!is.null(curColLevels$minLevel)) {
        minLevel(currentColumnObject) <- as.double(curColLevels$minLevel[[1]][1])
      }
      if (!is.null(curColLevels$maxLevel)) {
        maxLevel(currentColumnObject) <- as.double(curColLevels$maxLevel[[1]][1])
      }
    } else if (curColLevelsType == "Character") {
      # In this case, we need to do some work: We need to read off what the permitted levels of this column are,
      # and then create a Level object for each, before we can create the Column object.
      curColLevelObjects <- list()
      for (xmlLevelDescriptionIndex in 2:(length(curColLevels))) { # The 2 because the first tag is the <levelsType> tag.
        xmlLevelDescription <- curColLevels[[xmlLevelDescriptionIndex]]
        # Start by finding the name of the level:
        curLevelName <- xmlLevelDescription$levelName[[1]][1]
        # If it has any aliases, we extract all of them with a sapply:
        if (length(xmlLevelDescription) > 1) {
          curLevelAliases <- sapply(c(2:length(xmlLevelDescription)), function(aliasIndex) xmlLevelDescription[[aliasIndex]][[1]][1])
        } else {
          # It has no aliases, so it should just be an empty vector:
          curLevelAliases <- character(0)
        }
        # We create the level object and append it to the list of levels:
        curLevelObject <- Level(name = curLevelName, aliases = curLevelAliases)
        curColLevelObjects[[length(curColLevelObjects) + 1]] <- curLevelObject
      }
      # Having created Level objects for each level, we can now create the Column object:
      currentColumnObject <- Column(name = curColName, aliases = curColAliases, levelsType = curColLevelsType, colLevels = curColLevelObjects)
    } else {
      # All columns must have one of these three level types, so the XML is invalid and we stop:
      stop("XML file attempted to specify an invalid levelsType. This violates the XSD for XML documentation -- make sure you validate your XML files.")
    }
    # Having constructed our Column object, we append it onto the list of Column objects:
    tableCols[[length(tableCols) + 1]] <- currentColumnObject
  }

  # Now we have one Column object per column documented in the XML, so we can finally create our DocumentedTable object and return it:
  resultingTable <- DocumentedTable(name = tableName,
                                    description = tableDescription,
                                    dataSource = tableDataSource,
                                    dataYear = tableDataYear,
                                    csvData = tableCsvData,
                                    tableColumns = tableCols,
                                    valueColumn = tableValueColumn)
  return(resultingTable)
}
