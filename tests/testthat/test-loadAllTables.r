# Very little testing necessary here, really. The only thing we really want to check is that
# all tables have distinct names. Everything else is already tested elsewhere, in the tests of
# the functions called or the tests of the XML files:

test_that("loadAllTables works", {
  # No warnings or errors on running loadAllTables:
  expect_silent(testAllLoadedTables <- loadAllTables())

  # Everything that's returned is a valid object:
  testAllLoadedTables <- loadAllTables()
  for (loadedTable in testAllLoadedTables) {
    expect_true("DocumentedTable" %in% is(loadedTable))
    expect_true(validObject(loadedTable))
  }
})

# If two tables have the same name, this will break the forceTable functionality in SCB(),
# so we test that table names are all distinct here:
test_that("All loaded tables have distinct names", {
  expect_equal(unique(loadedTableNames),loadedTableNames)
})
