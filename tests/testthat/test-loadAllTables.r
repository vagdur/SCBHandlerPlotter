# Very little testing necessary here, really. The only thing we really want to check is that
# all tables have distinct names. Everything else is already tested elsewhere, in the tests of
# the functions called or the tests of the XML files:

test_that("All loaded tables have distinct names", {
  expect_equal(unique(loadedTableNames),loadedTableNames)
})
