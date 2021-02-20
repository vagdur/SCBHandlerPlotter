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
  test_that(paste("file ",tableToCheck," is in the proper format for a documentation file"),{
    expect_true(checkFormatConformity(paste(extdataLocation,"/",tableToCheck,sep="")))
  })
}
