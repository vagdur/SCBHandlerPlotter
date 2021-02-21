# Check a single table documentation XML file.
# This checks three things:
# 1. It is actually properly formed XML that can be read by an XML parser
# 2. It conforms to the XML Schema for this sort of file. (Found in the same folder as the XML files themselves.)
# 3. It additionally has properly formed columns. (This can't be checked with an XML Schema, I think, since XML is context-free.) That is:
#     a)  A column of levelsType "Municipalities" contains no additional information on levels, one of levelsType NumericRange contains
#         at most information on upper and lower limits of the range, and one of levelsType Character contains information on at least
#         one level.
#     b)  No two columns share a name or share an alias.
#     c)  No two levels of one column share a name or share an alias.
# Note that this not at all check that the documentation is *correct* -- the file it claims to be documenting might not even exist,
# and these tests would still not fail. It only checks that the file *could* be the documentation of a table.
# All this is divided into one actual test each, so that eventual errors can be granularly identified.
checkFormatConformity <- function(filename) {
  # For prettier output from our tests, let's just write the name of the file, not its whole path:
  fileShortName <- basename(filename)

  # First off the file has to actually exist:
  test_that(paste("file ",fileShortName," exists", sep=""),{
            expect_true(file.exists(filename))
            })
  if(!file.exists(filename)) {
    return(FALSE)
  }

  # Then, let's check that it is valid XML:
  test_that(paste("file ",fileShortName," is properly formed XML"),{
            parsedXML <- xml2::read_xml(filename, encoding="UTF-8")
            expect_true(exists("parsedXML"))
            })
  # Having done that, let's actually read it in, and also read in the XSD file containing our Schema.
  parsedXML <- xml2::read_xml(filename, encoding="UTF-8")
  xmlSchema <- xml2::read_xml(system.file("extdata", "TableDocumentationFormat.xsd", package = "SCBHandlerPlotter"), encoding="UTF-8")

  # Now, let's test that the XML matches the Schema:
  test_that(paste("file ",fileShortName," conforms to the XML Schema"), {
    expect_true(xml2::xml_validate(parsedXML, xmlSchema))
  })

  # If the XML fails to conform to our schema, it makes no sense to continue testing, so we return FALSE to indicate
  # the tests failed: (We need to do this since the caller of the function needs to know whether to continue testing.)
  if (!xml2::xml_validate(parsedXML, xmlSchema)) {
    return(FALSE)
  }

  # Now the easy part of our testing is done -- the file does exist, and it is a proper XML file which conforms to
  # our XML Schema. It remains to test the things that cannot be checked just using a context-free grammar: That the
  # column specifications actually make sense for a table, and names are unambiguous.

  # We need a return value here, so that the code executing this function can itself be a test. Otherwise the testing
  # will abort as soon as we get an error in here, and so not all XML files would get tested as soon as one fails. Thus:
  return(TRUE)
}


# Check that the documentation of the XML file matches the actual data
#
# This function actually does two things: First it checks that the file in question conforms to the format for a data
# documentation file, using the function checkFormatConformity, and then (assuming the file it got can indeed be the
# the documentation of a table) it checks that the documentation is correct.
#
# To do this, it checks:
#   1. That the file it claims to be documenting exists
#   2. That the file it claims to be documenting is indeed a csv
#   3. That the csv has exactly the columns the documentation claims it has
#   4. That each column's levels are correctly specified:
#       a)  A column with levelsType set to Municipalities contains only names of municipalities.
#       b)  A column with levelsType set to NumericRange is numeric, and if an upper or lower bound is specified,
#           then all values fall above (below) the lower (upper) bound. Note that we do not require that every number
#           inside the actual range occurs.
#       c)  A column with levelsType set to Character contains only elements as specified by one of the <level> tags,
#           and every level specified by a <level> tag actually occurs. Note the slight difference to our assumptions on
#           a NumericRange column.
#   5. That the valueColumn is numeric, possibly with NAs.

checkDocumentationCorrectness <- function(filename) {
  # To begin with, we check that it makes sense to check documentation correctness -- that is, that the file really
  # is a potentially correct documentation file:
  if(!checkFormatConformity(filename)) {
    # If this failed, it makes no sense to continue, so we return FALSE:
    return(FALSE)
  }

  # For prettier output from our tests, let's just write the name of the file, not its whole path:
  fileShortName <- basename(filename)

  # Knowing that the XML makes sense, we read it in and cast it to a super-nested list to work with:
  parsedXML <- xml2::read_xml(filename, encoding="UTF-8")
  docList <- xml2::as_list(parsedXML)$table

  # The first thing to do is to find the csv file it says it documents and test that it exists:
  csvFilename <- docList$filename[[1]][1] # The [[1]][1] is for some reason how the list we get is structured?
  test_that(paste("file ",csvFilename," which ",fileShortName," claims to document exists",sep=""),{
    expect_true(file.exists(system.file("extdata",csvFilename, package="SCBHandlerPlotter")))
  })
  if (!file.exists(system.file("extdata",csvFilename, package="SCBHandlerPlotter"))) {
    # If it doesn't exist, we can't continue:
    return(FALSE)
  }
  # If it does exist, we can read it in:
  dataTable <- read.csv(system.file("extdata",csvFilename, package="SCBHandlerPlotter"), fileEncoding = "UTF-8")

  # Having read it in, we check the valueColumn specified exists and is numeric:
  docValueColumn <- docList$columns$valueColumn$colname[[1]][1]
  test_that(paste("the valueColumn of",fileShortName,"exists in the actual table"),{
    expect_true(docValueColumn %in% colnames(dataTable))
  })
  if (!(docValueColumn %in% colnames(dataTable))) {
    # If the valueColumn doesn't exist, we can't check that it is numeric, obviously. No need to stop the testing here, though,
    # since nothing else depends on this test passing.
  } else {
    # If it does exist, we test that it is numeric:
    test_that(paste("the valueColumn of",fileShortName,"is numeric in the table"),{
      expect_true(is.numeric(dataTable[,docValueColumn])) # expect_type doesn't work here since integers and floats are different types, but both give true here
    })
  }

  # Now that we have picked out and validated the existence of the valueColumn, we can look at the rest of the columns,
  # and validate each of them.
  docColumns <- docList$columns
  # Our first test, however, is just that we have the right number of columns -- that is, the number of objects inside the <columns>
  # block is the same as the number of columns of the actual table. If they are equal, and we also check for each column in the XML
  # that it corresponds to one in the real table, then we will know that we have a one-to-one correspondence between columns in the
  # documentation and columns in the real table. (That column names are unique in the XML was checked already by checkFormatConformity,
  # and uniqueness of column names in the data is enforced by R, possibly by renaming on read.)
  test_that(paste("the number of columns documented in",fileShortName,"matches the number of columns in the data"),{
    expect_equal(length(docColumns),ncol(dataTable))
  })

  for (columnNumber in 1:(length(docColumns) - 1)) { # -1 because the final column is the valueColumn, which we've already checked.
    docColumn <- docColumns[[columnNumber]]
    # What is the documented name of this column?:
    docColumnName <- docColumn$colname[[1]][1]
    # Test that the column really exists:
    test_that(paste("column",docColumnName,"documented by",fileShortName,"exists in actual data"),{
      expect_true(docColumnName %in% colnames(dataTable))
    })
    if (!(docColumnName %in% colnames(dataTable))) {
      next # If the column doesn't actually exist, we can't test that its levels are as described
    }

    # Now, let's proceed to check that the description of the levels is correct. There are three cases,
    # depending on levelsType. (That it is one of the three was checked already by the XSD.)
    docLevelsType <- docColumn$levels$levelsType[[1]][1]
    if (docLevelsType == "Municipalities") {
      # Here, we need to check that every level of the column is the name of a municipality:
      test_that(paste("the column",docColumnName,"of",fileShortName,", documented to contain municipalities, does contain names of municipalities"),{
        expect_true(all(dataTable[,docColumnName] %in% municipalityNames))
      })
    }

    if (docLevelsType == "NumericRange") {
      # First, we test that the column is in fact numeric:
      test_that(paste("the column",docColumnName,"of",fileShortName,"is numeric in the table"),{
        expect_true(is.numeric(dataTable[,docColumnName])) # expect_type doesn't work here since integers and floats are different types, but both give true here
      })
      # If a minLevel is specified, we test that there are no values below it:
      if(!is.null(docColumn$levels$minLevel[[1]][1])) {
        docColMinLevel <- as.numeric(docColumn$levels$minLevel[[1]][1])
        test_that(paste("the column",docColumnName,"of",fileShortName,", documented to have minLevel",docColMinLevel,"takes no values below its minLevel"),{
          expect_lte(docColMinLevel, min(dataTable[,docColumnName]))
        })
      }
      # If a maxLevel is specified, we test that there are no values above it:
      if(!is.null(docColumn$levels$maxLevel[[1]][1])) {
        docColMaxLevel <- as.numeric(docColumn$levels$maxLevel[[1]][1])
        test_that(paste("the column",docColumnName,"of",fileShortName,", documented to have maxLevel",docColMaxLevel,"takes no values above its maxLevel"),{
          expect_gte(docColMaxLevel, max(dataTable[,docColumnName]))
        })
      }
    }

    if (docLevelsType == "Character") {
      # If the type is specified to be Character, we need to look at the list of <level> objects, look at the <levelName> of each,
      # and create a list of the values that occur. Then we test that this list contains exactly the same elements as the column in
      # the data.
      docLevelsList <- docColumn$levels
      documentationLevels <- character(0)
      for (levelIndex in 2:length(docLevelsList)) { # 2, because the first element is the <levelsType> tag.
        # Store the name of the level in levelsSeen:
        documentationLevels[levelIndex-1] <- docLevelsList[[levelIndex]]$levelName[[1]][1] # -1 because obviously documentationLevels should index from 1, not 2 (Got a bug from that before...)
      }
      # Having gathered our list of the levels the documentation says there are, we test two things:
      #   1. Every level in the actual data also occurs in the documentation
      #   2. Every level in the documentation actually occurs in the data
      test_that(paste("Every level of column",docColumnName,"in the actual data also occurs in the documentation",fileShortName),{
        # setdiff computes the set-minus operation A\B -- so setdiff(A,B) is all elements of A which do not also occur in B.
        # So here we take the actually occurring values and remove the ones that are documented, and we expect to get the
        # empty list, since we expect all occurring levels to be documented.
        expect_equal(setdiff(dataTable[,docColumnName], documentationLevels), character(0))
      })
      test_that(paste("Every level of column",docColumnName,"documented in",fileShortName,"occurs in the actual data"), {
        # Same logic as before, except we do B\A instead, since we want to check that every column the documentation says
        # exists actually exists.
        expect_equal(setdiff(documentationLevels, dataTable[,docColumnName]), character(0))
      })
    }
  }

  # We need a return value here, so that the code executing this function can itself be a test. Otherwise the testing
  # will abort as soon as we get an error in here, and so not all XML files would get tested as soon as one fails. Thus:
  return(TRUE)
}

# Our very first test should be that we can actually read in the XML Schema we need to validate our XML files:
test_that("XML Schema can be read",{
  xmlSchema <- xml2::read_xml(system.file("extdata", "TableDocumentationFormat.xsd", package = "SCBHandlerPlotter"), encoding="UTF-8")
  expect_true(exists("xmlSchema"))
})

# We assume that every file in extdata with the .xml file ending is a specification of a table:
extdataLocation <- system.file("extdata", package = "SCBHandlerPlotter")
tablesToCheck <- list.files(extdataLocation, pattern="*\\.xml$")
# Then we just loop over all the files and run our tests on each of them:
for (tableToCheck in tablesToCheck) {
  # We wrap this in a test so that the loop won't stop just because one file fails its tests.
  # If this test fails, there'll also be more granular tests that have failed:
  test_that(paste("documentation ",tableToCheck," is correct"),{
    expect_true(checkDocumentationCorrectness(paste(extdataLocation,"/",tableToCheck,sep="")))
  })
}
