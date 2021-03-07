# Since createDocumentedTableFromXML returns some very complicated and big objects,
# it is hard to test whether it returns the thing we want, since writing down what
# exactly we want is hard. So we content ourselves with just testing that it returns
# objects of the right type for all our XML files, and we run snapshot tests of some
# known XML files.
#
# In fact, the tests here are  nearlyexactly copy-pastes of the tests of writeTextDocumentation,
# except we replace writeTextDocumentation by createDocumentedTableFromXML. This is because
# both functions suffer from the exact same problems with testing them.

# We assume that every file in extdata with the .xml file ending is a specification of a table:
extdataLocation <- system.file("extdata", package = "SCBHandlerPlotter")
tablesToCheck <- list.files(extdataLocation, pattern="*\\.xml$")
# Then we just loop over all the files and run our fuzzy tests on each of them:
for (tableToCheck in tablesToCheck) {
  filename <- paste(extdataLocation,"/",tableToCheck,sep="")

  # First we test that it returns an object of the right class...
  test_that(paste("createDocumentedTableFromXML produces a DocumentedTable when called on ",tableToCheck),{
    expect_s4_class(createDocumentedTableFromXML(filename), "DocumentedTable")
  })
  # ...and then we check that the object is a valid object:
  test_that(paste("createDocumentedTableFromXML does not produce an invalid object when called on ",tableToCheck),{
    expect_true(validObject(createDocumentedTableFromXML(filename)))
  })
}

# Having verified that it doesn't completely break on any file in the folder, let's
# also run it on some specific files we know are there, using snapshot tests so we
# are alerted if the documentation ever changes unexpectedly:
test_that("running createDocumentedTableFromXML on AgeGenderTable.xml returns the same output as last time", {
  skip_on_ci()
  expect_snapshot_value(createDocumentedTableFromXML(paste(extdataLocation,"/AgeGenderTable.xml",sep="")), style="serialize")
  })
test_that("running createDocumentedTableFromXML on BirthCountryTable.xml returns the same output as last time", {
  skip_on_ci()
  expect_snapshot_value(createDocumentedTableFromXML(paste(extdataLocation,"/BirthCountryTable.xml",sep="")), style="serialize")
  })
test_that("running createDocumentedTableFromXML on EducationLevelTable.xml returns the same output as last time", {
  skip_on_ci()
  expect_snapshot_value(createDocumentedTableFromXML(paste(extdataLocation,"/EducationLevelTable.xml",sep="")), style="serialize")
  })
test_that("running createDocumentedTableFromXML on FinancialResultsTable.xml returns the same output as last time", {
  skip_on_ci()
  expect_snapshot_value(createDocumentedTableFromXML(paste(extdataLocation,"/FinancialResultsTable.xml",sep="")), style="serialize")
  })
test_that("running createDocumentedTableFromXML on HighSchoolEligibilityTable.xml returns the same output as last time", {
  skip_on_ci()
  expect_snapshot_value(createDocumentedTableFromXML(paste(extdataLocation,"/HighSchoolEligibilityTable.xml",sep="")), style="serialize")
  })
test_that("running createDocumentedTableFromXML on HouseholdSizeTable.xml returns the same output as last time", {
  skip_on_ci()
  expect_snapshot_value(createDocumentedTableFromXML(paste(extdataLocation,"/HouseholdSizeTable.xml",sep="")), style="serialize")
  })
test_that("running createDocumentedTableFromXML on HousingFormTable.xml returns the same output as last time", {
  skip_on_ci()
  expect_snapshot_value(createDocumentedTableFromXML(paste(extdataLocation,"/HousingFormTable.xml",sep="")), style="serialize")
  })
test_that("running createDocumentedTableFromXML on IncomeTable.xml returns the same output as last time", {
  skip_on_ci()
  expect_snapshot_value(createDocumentedTableFromXML(paste(extdataLocation,"/IncomeTable.xml",sep="")), style="serialize")
  })
test_that("running createDocumentedTableFromXML on LandUseTable.xml returns the same output as last time", {
  skip_on_ci()
  expect_snapshot_value(createDocumentedTableFromXML(paste(extdataLocation,"/LandUseTable.xml",sep="")), style="serialize")
  })
test_that("running createDocumentedTableFromXML on MaritalStatusTable.xml returns the same output as last time", {
  skip_on_ci()
  expect_snapshot_value(createDocumentedTableFromXML(paste(extdataLocation,"/MaritalStatusTable.xml",sep="")), style="serialize")
  })
test_that("running createDocumentedTableFromXML on TaxRatesTable.xml returns the same output as last time", {
  skip_on_ci()
  expect_snapshot_value(createDocumentedTableFromXML(paste(extdataLocation,"/TaxRatesTable.xml",sep="")), style="serialize")
  })

