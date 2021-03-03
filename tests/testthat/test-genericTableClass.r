################################################################################
# Tests of the Level class:
test_that("Level objects can be created manually", {
  tl <- new("Level", name = "Test", aliases = c("test","tset"))
  expect_s4_class(tl, "Level")
})
test_that("Level objects can be created by constructor", {
  tl <- Level("Test", c("Test","test","tset"))
  tl2 <- Level(name = "Test", aliases = c("Test","test","tset"))
  expect_s4_class(tl, "Level")
  expect_s4_class(tl2, "Level")
})
test_that("Validation of Levels objects works", {
  expect_error(err_tl <- new("Level", lvName = c("Two","Names")))
  tl <- Level("Test", c("Test","test","tset"))
  expect_error(name(tl) <- c("Two", "Names"))
})
test_that("Getters of name and aliases for Level objects work",{
  tl <- Level("Test", c("Test","test","tset"))
  expect_identical(name(tl),"Test")
  expect_identical(aliases(tl),c("Test","test","tset"))
})
test_that("Setters of name and aliases for Level objects work",{
  tl <- Level("Test", c("Test","test","tset"))
  name(tl) <- "NotTest"
  expect_identical(name(tl),"NotTest")
  aliases(tl) <- c("Not the","same")
  expect_identical(aliases(tl),c("Not the","same"))
})
test_that("Dealiasing of levels works", {
  tl <- Level("Test", c("Test","test","tset"))
  expect_identical(dealias(tl,"test"), "Test")
  expect_null(dealias(tl,"Not in the list"))
  expect_identical(dealias(tl,c("test","Test","Test","tset","NotInTheList")),c("Test","Test","Test","Test",NULL))
})
