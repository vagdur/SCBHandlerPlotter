# In order to create all the data table documentation, set working directory to the project folder, and run:
# writeAllTextDocumentation("./inst/extdata","./man")

# TODO: Add functionality to automatically remove old documentation that no longer has a corresponding XML file.



# This file contains a script that takes an XML documentation file of a table
# and produces a documentation file in the .Rd format, to make it more human
# readable. Note that this function won't be run ever by the package --
# instead it should be getting run just before build time for the package,
# to create the .Rd files to go with the package. Unfortunately there is no
# hook to do this at buildtime for an R package, so it just has to be done
# manually... It's unfortunate.

# We create this as a function which reads in the filename of the XML documentation,
# and returns the documentation as one big string. This is to make it slightly easier
# to build tests for it -- the function can then presumably be called from command line
# and piped into the right file.
writeTextDocumentation <- function(filename) {
  # The documentation can be built incrementally by just appending stuff to a string:
  doc <- ""
  # We are always documenting data, and may include some non-ASCII characters, so:
  doc <- paste(doc,'\\docType{data}\n\\encoding{utf8}\n',sep="")

  # We load the XML documentation file and turn it into a list: (Note that we assume we are given its entire path,
  # not just its filename. We also assume that the XML documentation has been tested.)
  parsedXML <- xml2::read_xml(filename, encoding="UTF-8")
  docList <- xml2::as_list(parsedXML)$table

  # We start by writing the name anb title of the dataset:
  tableName <- docList$name[[1]][1]
  tableTitle <- docList$title[[1]][1]
  doc <- paste(doc,'\\name{',tableName,'}\n\\alias{',tableName,'}\n\\title{',tableTitle,'}\n',sep="")

  # Now we proceed to write the documentation of the columns that are present:
  tableColumns <- docList$columns
  doc <- paste(doc,'\\format{A table with ',length(tableColumns)-1,' variables, to be queried with the SCB() function.\n\\describe{\n',sep="")
  for (columnIndex in 1:(length(tableColumns)-1)) { # -1 because we don't want to document the valueColumn
    currentColumn <- tableColumns[[columnIndex]]
    curColIdentifier <- currentColumn$identifier
    # Start writing the documentation of this column:
    doc <- paste(doc,"\\item{",curColIdentifier,"}{",sep="")
    # First, we figure out if this column has any aliases, and if so, we write them:
    if (length(currentColumn) > 3) { # There's always <identifier>, <colname>, and <levels>, so any additional tags must be <alias>
      columnAliases <- sapply(c(3:length(currentColumn)-1), function(x) currentColumn[[x]][[1]][1])
      doc <- paste(doc,"Also known as: ",paste(columnAliases, collapse=", "),".\n",sep="")
    }
    # What we write next depends on the type of levels the column has:
    columnLevelsType <- currentColumn$levels$levelsType[[1]][1]
    if (columnLevelsType == "Municipalities") {
      # This is the easiest case, since now the levelsType has told us everything we need to know about the column.
      doc <- paste(doc, "Municipality the observation is from.", sep="")
    } else if (columnLevelsType == "NumericRange") {
      # This is also a relatively simple case, all we need to do is figure out if a maxLevel and/or minLevel has been specified
      doc <- paste(doc,"A range of numeric values",sep="")
      # So there are four cases depending on which if any of maxLevel and minLevel are set
      if (!is.null(currentColumn$levels$minLevel)) {
        if (!is.null(currentColumn$levels$maxLevel)) {
          # Both maxLevel and minLevel are set:
          doc <- paste(doc,", taking values between ",currentColumn$levels$minLevel," and ",currentColumn$levels$maxLevel,".", sep="")
        } else {
          # minLevel is set, but maxLevel is not:
          doc <- paste(doc,", taking values above ",currentColumn$levels$minLevel,".",sep="")
        }
      } else {
        if (!is.null(currentColumn$levels$maxLevel)) {
          # minLevel is not set, but maxLevel is:
          doc <- paste(doc,", taking values below ",currentColumn$levels$maxLevel,".",sep="")
        } else {
          # Neither maxLevel nor minLevel are set:
          # There is nothing more to add, so we just add a period at the end of the sentence.
          doc <- paste(doc,".",sep="")
        }
      }
    } else if (columnLevelsType == "Character") {
      # In this case, we need to do a bit of work, looping through all the specified levels of the table, and
      # printing their name and aliases. Getting this to look good with commas in the list and so on gets a bit messy...
      doc <- paste(doc,"A column of factor data. Its levels are: ",sep="")
      columnLevels <- currentColumn$levels
      # To make the commas in the list display right, we first create a list of what should be said about each level,
      # and then paste that list together:
      columnLevelsDocs <- character(0)
      for (levelIndex in 2:(length(columnLevels))) {
        currentLevel <- columnLevels[[levelIndex]]
        # We create a string that should contain what is to be said about this level:
        currentLevelDoc <- currentLevel$levelName
        if (length(currentLevel) > 1) { # i.e. there are some aliases, which we note inside parentheses:
          currentLevelDoc <- paste(currentLevelDoc, " (a.k.a.: ",sep="")
          levelAliases <- sapply(c(2:length(currentLevel)), function(x) currentLevel[[x]][[1]][1])
          currentLevelDoc <- paste(currentLevelDoc, paste(levelAliases, collapse = ", "), ".)",sep="")
        }
        columnLevelsDocs <- append(columnLevelsDocs,currentLevelDoc)
      }
      # Now we have our list of what is to be said about each level, so we write it in our documentation:
      doc <- paste(doc,paste(columnLevelsDocs, collapse=", "),".",sep="")
    } else {
      stop("Tried to document a column with invalid levelsType. Are you sure the documentation XML passed all tests?")
    }

    # We close the final bracket in our \item{}{ block:
    doc <- paste(doc,"}\n",sep="")
  }

  # We close the opening { of the \format{ and \describe{ tags:
  doc <- paste(doc,'}}\n',sep="")

  # The final thing we need to add is the description and keyword 'datasets': (Things don't appear in this order when rendered, but this is
  # the order Roxygen would put them, so we follow their ordering.)
  tableDescription <- docList$description[[1]][1]
  doc <- paste(doc, '\\description{',tableDescription,'}\n\\keywords{datasets}',sep="")

  # Having created all the documentation, we now just return the string:
  return(doc)
}


# This function takes as argument the folder where the XML documentation is stored and the folder where the .Rd documentation should be stored.
# It then runs writeTextDocumentation on each XML file in sourceFolder, and writes the result to targetFolder as an .Rd file.
writeAllTextDocumentation <- function(sourceFolder, targetFolder) {
  # Find all the XML files:
  xmlFilesInSourceFolder <- list.files(sourceFolder, pattern="*\\.xml$")
  # We also need these with .Rd as their file extension, since we need to save them:
  filenamesWithRdExtension <- gsub("\\.xml","\\.Rd",xmlFilesInSourceFolder)
  # Now we loop over the files and print each of them to the target folder:
  for (fileIndex in c(1:length(xmlFilesInSourceFolder))) {
    targetFilePath <- paste(targetFolder,"/",filenamesWithRdExtension[fileIndex],sep="")
    sourceFilePath <- paste(sourceFolder,"/",xmlFilesInSourceFolder[fileIndex],sep="")
    # Just to be safe, if the .Rd file already exists, we remove it:
    if (file.exists(targetFilePath)) {
      file.remove(targetFilePath)
    }
    # Open a connection to the file we want to write to:
    fileConn <- file(description = targetFilePath, encoding="UTF8")
    writeLines(writeTextDocumentation(sourceFilePath),
        con = fileConn)
    # Close the connection again:
    close(fileConn)
  }
}
