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

  # Then, let's check that it is valid XML:
  test_that(paste("file ",fileShortName," is properly formed XML"),{
            parsedXML <- xml2::read_xml(filename)
            expect_true(exists("parsedXML"))
            })
  # Having done that, let's actually read it in, and also read in the XSD file containing our Schema.
  parsedXML <- xml2::read_xml(filename)
  xmlSchema <- xml2::read_xml(system.file("extdata", "TableDocumentationFormat.xsd", package = "SCBHandlerPlotter"))

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
# To do this, it checks in order:
#   1. That the file it claims to be documenting exists
#   2. That the file it claims to be documenting is indeed a csv
#   3. That the csv has exactly the columns the documentation claims it has
#   4. That each column's levels are correctly specified:
#       a)  A column with levelsType set to Municipalities contains only names of municipalities.
#       b)  A column with levelsType set to NumericRange is numeric, and if an upper or lower bound is specified,
#           then all values fall above (below) the lower (upper) bound.
#       c)  A column with levelsType set to Character contains only elements as specified by one of the <level> tags.
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
  parsedXML <- xml2::read_xml(filename)
  docList <- xml2::as_list(parsedXML)$table

  # The first thing to do is to find the csv file it says it documents and test that it exists:
  csvFilename <- docList$filename[[1]][1] # The [[1]][1] is for some reason how the list we get is structured?
  test_that(paste("file ",csvFilename," which ",fileShortName," claims to document exists"),{
    expect_true(file.exists(system.file("extdata",csvFilename, package="SCBHandlerPlotter")))
  })
  if (!file.exists(system.file("extdata",csvFilename, package="SCBHandlerPlotter"))) {
    # If it doesn't exist, we can't continue:
    return(FALSE)
  }
  # If it does exist, we can read it in:
  dataTable <- read.csv(system.file("extdata",csvFilename, package="SCBHandlerPlotter"))

  # We need a return value here, so that the code executing this function can itself be a test. Otherwise the testing
  # will abort as soon as we get an error in here, and so not all XML files would get tested as soon as one fails. Thus:
  return(TRUE)
}

# Our very first test should be that we can actually read in the XML Schema we need to validate our XML files:
test_that("XML Schema can be read",{
  xmlSchema <- xml2::read_xml(system.file("extdata", "TableDocumentationFormat.xsd", package = "SCBHandlerPlotter"))
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
