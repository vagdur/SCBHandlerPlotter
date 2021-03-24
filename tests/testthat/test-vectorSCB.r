#################
# The vectorSCB function doesn't actually handle queries itself, it just does a bunch of logic to make
# sure queries arrive in the right place. So what we want to test here is not that we get the right values,
# but that we get errors when we expect errors and get numbers when we expect numbers.

# So let's start by checking the input validation:
test_that("Input validation of vectorSCB works", {
  # The arguments forceTable and verbose should be of the right type and of length one:
  expect_error(vectorSCB(verbose="Sesquipedalian", forceTable="AgeGender", vectorColumn=Age))
  expect_error(vectorSCB(verbose=c(TRUE,FALSE), forceTable="AgeGender", vectorColumn=Age))
  expect_error(vectorSCB(verbose=TRUE, forceTable=TRUE, vectorColumn=Age))
  expect_error(vectorSCB(verbose=TRUE, forceTable=c("AgeGender","BirthRegion"), vectorColumn=Age))

  # The argument vectorColumn should just be an unquoted string, not a complex expression:
  expect_error(vectorSCB(vectorColumn=paste("A","ge"), Municipality="Ludvika", forceTable = "AgeGender"))

  # If we pass an argument to forceTable, it should be the name of a real table:
  expect_error(vectorSCB(vectorColumn=Age, forceTable="Not a real table name"))
  # If we pass an argument to forceTable, the vectorColumn we passed should be in that table. Whether we
  # get an error or just a warning depends on the value of failIfUnable:
  expect_error(vectorSCB(forceTable = "AgeGender", vectorColumn=NotARealColumn, failIfUnable = TRUE))
  expect_warning(expect_identical(vectorSCB(forceTable = "AgeGender", vectorColumn=NotARealColumn, failIfUnable = FALSE), NA_real_))

  # If we do not pass an argument to forceTable, our query has to actually be possible in some table. Whether we
  # get an error or just a warning depends on the value of failIfUnable:
  expect_error(vectorSCB(vectorColumn=NotARealColumn, Age=25, Municipality="Ludvika", failIfUnable = TRUE))
  expect_warning(expect_identical(vectorSCB(vectorColumn=NotARealColumn, Age=25, Municipality="Ludvika", failIfUnable = FALSE), NA_real_))
  expect_error(vectorSCB(vectorColumn=Age, NotARealColumn=25, Municipality="Ludvika", failIfUnable = TRUE))
  expect_warning(expect_identical(vectorSCB(vectorColumn=Age, NotARealColumn=25, Municipality="Ludvika", failIfUnable = FALSE), NA_real_))

  # If our query works in multiple tables, we expect an error, since there is no collisionHandlingMode implemented yet:
  expect_error(vectorSCB(vectorColumn=Municipality))
})


# Having tested that input validation gives the errors we expect for invalid arguments, let's check that we
# get the same output as usual for the things we do expect to work:
test_that("vectorSCB works", {
  expect_snapshot(vectorSCB(vectorColumn=LandUseType, Municipality="Ludvika"))
  expect_snapshot(vectorSCB(vectorColumn=LandUseType, Municipality="Ludvika", forceTable="LandUse"))
  expect_snapshot(vectorSCB(vectorColumn=LandUseType, Municipality="Ludvika", verbose=TRUE))
  expect_snapshot(vectorSCB(vectorColumn=LandUseType, Municipality="Ludvika", forceTable="LandUse", verbose=TRUE))
  expect_snapshot(vectorSCB(vectorColumn=LandUseType, LandUseType=c("Total forest", "Farmland"), Municipality="Ludvika", forceTable="LandUse", verbose=TRUE))

  # If we want to use a vectorColumn whose name is stored in a variable, we use the bang-bang operator:
  # (This is mentioned in documentation, so important that we include it in our tests. Examples aren't run by
  # the tests, only by R CMD Check, so errors there can go unnoticed for longer.)
  columnToVector <- "Municipality"
  expect_snapshot(vectorSCB(Gender = "Male", forceTable = "AgeGender", vectorColumn = !!columnToVector))
})
