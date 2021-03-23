# Tests of the SCB function.
# It is a little bit tricky to choose exactly what we want to be testing here. The choice I
# made is to test only the various settings and the routing of queries to tables, but not the
# correctness of the results of queries. The logic being that correctness of query results should
# be tested in the tests of the query() method of the DocumentedTable class.

# the first thing to test is just that querying the tables works without throwing error. We pick
# some random queries for this, to hit a couple of different tables. (Non-brokenness of tables is
# primarily tested in the tests of loadAllTables and createDocumentedTableFromXML.)

test_that("Using SCB does not give unexpected errors or warnings", {
  # we use snapshot tests here just to avoid having magic constants (the answers) in the test code.
  # First the two examples in the readme file, so we're sure they work:
  expect_snapshot(SCB(Municipality = "Uppsala", Age=24, Gender="Women", MaritalStatus = "Divorced"))
  expect_snapshot(SCB(Municipality = "Lomma", LandUseType = "Total forest"))
  # And then two more, just to hit two more tables:
  expect_snapshot(SCB(Municipality = "Lund", MunicipalOrRegionalTax = "Municipal"))
  expect_snapshot(SCB(Municipality = "Lund", HighSchoolEligibility = "True"))
})

test_that("Validation of non-query parameters works", {
  # All of these parameters are optional, but if passed, need to take certain values or types:
  expect_error(SCB(verbose="not logical"))
  expect_error(SCB(failIfUnable="not logical"))
  expect_error(SCB(forceTable=TRUE)) # Should be of type character
  expect_error(SCB(collisionHandlingMode=TRUE)) # Should be of type character
  expect_error(SCB(collisionHandlingMode="iunno whatever")) # Not a valid collision handling mode

})

test_that("Parameter forceTable works as expected", {
  # If we pass a value to forceTable, it should be the name of a table:
  expect_error(SCB(forceTable="Not a real table name")) # We should presumably never be adding a table named "Not a real table name", so this works.

  # If the thing we passed is indeed the name of a table, the first thing to happen is that we check if said table can handle the query.
  # If it can not, what happens depends on the value of failIfUnable:
  expect_warning(expect_identical(SCB(forceTable="AgeGender", NotARealColumnName="not a real level", failIfUnable=FALSE), NA_real_))
  expect_error(SCB(forceTable="AgeGender", NotARealColumnName="not a real level", failIfUnable=TRUE))

  # If it can handle the query, it should return something sensible. We'll just run two examples as snapshots, one counting women in Sweden (an application
  # which really need forceTable, since Gender is a column in several tables) and one counting the land area of Sweden (does not really need forceTable, would
  # work anyway). These are snapshot tests to avoid having magic constants (what the answers should be) in the test code.
  expect_snapshot(SCB(forceTable="AgeGender", Gender="Women"))
  expect_snapshot(SCB(forceTable="LandUse", LandUseType="Total area"))
})

test_that("Parameter failIfUnable works as expected", {
  # This is also tested partially in the tests of forceTable, but for the
  # non-forceTable case, we test it here instead.
  # The point is that we expect an error if failIfUnable is TRUE and the query
  # can't be handled, otherwise we expect a warning and to see NA_real_:
  expect_warning(expect_identical(SCB(NotARealColumnName="not a real level", failIfUnable=FALSE), NA_real_))
  expect_error(SCB(NotARealColumn="Not a real level", failIfUnable=TRUE))
})

test_that("Parameter verbose works as expected", {
  # The parameter verbose is mostly tested either in the query() tests, since it is passed on with the query,
  # or in the tests of the collisionHandling (since its main purpose is to explain what happened in resolving a
  # collision), but there is one case to check: The query is unambiguous, but verbose=TRUE and we tell the user
  # which table their answer comes from:
  expect_message(SCB(Municipality="Lomma", LandUseType="Farmland", verbose=TRUE))
  # If verbose is false the message should not print:
  expect_silent(SCB(Municipality="Lomma", LandUseType="Farmland", verbose=FALSE))
})


# Time to test the collision handling modes. For all except consensus, passing no query
# parameters at all is suitable, since we are guaranteed that every table can handle it.
# For consensus, we'll have to think a bit harder. One suitable query that should achieve consensus
# from three tables is:
# SCB(collisionHandlingMode="consensus", Gender="Women", Age=20)
# TODO: Since updating the MaritalStatus to 2020, there is no longer a query that achieves consensus,
# so this mode cannot be fully tested. Unsure what to do about this.
test_that("Collision handling mode error works as expected", {
  # Very easy, there's no conditionals at all, it's always an error:
  expect_error(SCB(collisionHandlingMode = "error"))
})

test_that("Collision handling mode namedVector works as expected", {
  # For namedVector, we expect a list of length greater than 1 when we get a collision:
  expect_gte(length(SCB(collisionHandlingMode = "namedVector")), 2)
  # If we set verbose=TRUE, we want a message as well. Here it is not suitable to just pass no
  # parameters, since there might be a table with some row set to NA, which also gives a message
  # when verbose is TRUE. So instead we use the query for 20-year-old women, since that's a table
  # we know three tables can handle, and none return NA: (This makes this test a little bit unstable
  # under addition of new tables, it might need occasional updates.)
  expect_message(SCB(Age=20, Gender="Women", collisionHandlingMode="namedVector", verbose=TRUE))
})

test_that("Collision handling mode consensus works as expected", {
  # Here we actually have a few cases. We either pass a query which doesn't achieve a consensus (in which case
  # we expect an error), or we pass one which does in which case we expect a message iff verbose=TRUE. So we add
  # some snapshot tests to make sure we're getting the right number and the right message in the latter two cases:
  expect_error(SCB(collisionHandlingMode="consensus"))
  # Since updating the MaritalStatus table to the data for 2020, there is no longer a query that achieves consensus
  # from multiple tables, so these tests can no longer be run:
  #expect_silent(SCB(collisionHandlingMode = "consensus", Gender="Women", Age=20, verbose=FALSE))
  #expect_snapshot(SCB(collisionHandlingMode = "consensus", Gender="Women", Age=20, verbose=FALSE))
  #expect_message(SCB(collisionHandlingMode = "consensus", Gender="Women", Age=20, verbose=TRUE))
  #expect_snapshot(SCB(collisionHandlingMode = "consensus", Gender="Women", Age=20, verbose=TRUE))
})

test_that("Collision handling mode arbitrary works as expected", {
  # This is in a way the easiest to test, since it has the least requirements: It always returns something if we get
  # a collision. We can't test what it returns, since that is intentionally arbitrary -- I think it will pick the first
  # table in alphabetical order that can handle the query, so it might change pretty often. We can however test that we
  # do get some return, and a message iff verbose=TRUE. So we just test for silence and for message:
  expect_silent(SCB(collisionHandlingMode = "arbitrary", verbose=FALSE))
  expect_message(SCB(collisionHandlingMode = "arbitrary", verbose=TRUE))
})
