# Our tests here are relatively simple: We do one fuzzy test on all the XML tables, just checking
# that writeTextDocumentation doesn't throw an error and does return a string, and then we also do
# a specific test that the result of running the function on some specific known tables doesn't change.

# We assume that every file in extdata with the .xml file ending is a specification of a table:
extdataLocation <- system.file("extdata", package = "SCBHandlerPlotter")
tablesToCheck <- list.files(extdataLocation, pattern="*\\.xml$")
# Then we just loop over all the files and run our fuzzy tests on each of them:
for (tableToCheck in tablesToCheck) {
  filename <- paste(extdataLocation,"/",tableToCheck,sep="")
  test_that(paste("writeTextDocumentation produces a string when called on ",tableToCheck),{
    expect_type(writeTextDocumentation(filename), "character")
  })
}

# Having verified that it doesn't completely break on any file in the folder, let's
# also run it on some specific files we know are there, using snapshot tests so we
# are alerted if the documentation ever changes unexpectedly:
extdataLocation <- system.file("extdata", package = "SCBHandlerPlotter")

test_that("running writeTextDocumentation on AgeGenderTable.xml returns the same output as last time",{expect_snapshot_value(writeTextDocumentation(paste(extdataLocation,"/AgeGenderTable.xml",sep="")))})
test_that("running writeTextDocumentation on BirthCountryTable.xml returns the same output as last time",{expect_snapshot_value(writeTextDocumentation(paste(extdataLocation,"/BirthCountryTable.xml",sep="")))})
test_that("running writeTextDocumentation on EducationLevelTable.xml returns the same output as last time",{expect_snapshot_value(writeTextDocumentation(paste(extdataLocation,"/EducationLevelTable.xml",sep="")))})
test_that("running writeTextDocumentation on FinancialResultsTable.xml returns the same output as last time",{expect_snapshot_value(writeTextDocumentation(paste(extdataLocation,"/FinancialResultsTable.xml",sep="")))})
test_that("running writeTextDocumentation on HighSchoolEligibilityTable.xml returns the same output as last time",{expect_snapshot_value(writeTextDocumentation(paste(extdataLocation,"/HighSchoolEligibilityTable.xml",sep="")))})
test_that("running writeTextDocumentation on HouseholdSizeTable.xml returns the same output as last time",{expect_snapshot_value(writeTextDocumentation(paste(extdataLocation,"/HouseholdSizeTable.xml",sep="")))})
test_that("running writeTextDocumentation on HousingFormTable.xml returns the same output as last time",{expect_snapshot_value(writeTextDocumentation(paste(extdataLocation,"/HousingFormTable.xml",sep="")))})
test_that("running writeTextDocumentation on IncomeTable.xml returns the same output as last time",{expect_snapshot_value(writeTextDocumentation(paste(extdataLocation,"/IncomeTable.xml",sep="")))})
test_that("running writeTextDocumentation on LandUseTable.xml returns the same output as last time",{expect_snapshot_value(writeTextDocumentation(paste(extdataLocation,"/LandUseTable.xml",sep="")))})
test_that("running writeTextDocumentation on MaritalStatusTable.xml returns the same output as last time",{expect_snapshot_value(writeTextDocumentation(paste(extdataLocation,"/MaritalStatusTable.xml",sep="")))})
test_that("running writeTextDocumentation on TaxRatesTable.xml returns the same output as last time",{expect_snapshot_value(writeTextDocumentation(paste(extdataLocation,"/TaxRatesTable.xml",sep="")))})
