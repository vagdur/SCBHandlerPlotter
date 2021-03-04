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

##############################################################################
# Tests of the Column class:
test_that("Column objects of levelsType Municipalities can be created manually",{
  tc <- new("Column", name="region", aliases=c("Kommun", "Municipality"), levelsType="Municipalities")
  expect_s4_class(tc, "Column")
})
test_that("Column objects of levelsType Municipalities can be created by constructor",{
  tc <- Column(name="region", aliases=c("Kommun", "Municipality"), levelsType="Municipalities")
  expect_s4_class(tc, "Column")
})

test_that("Column objects of levelsType NumericRange can be created manually", {
  tc1 <- new("Column", name="alder", aliases=c("Age","age"), levelsType="NumericRange")
  tc2 <- new("Column", name="alder", aliases=c("Age","age"), levelsType="NumericRange", maxLevel=23)
  tc3 <- new("Column", name="alder", aliases=c("Age","age"), levelsType="NumericRange", maxLevel=45, minLevel=23)
  expect_s4_class(tc1, "Column")
  expect_s4_class(tc2, "Column")
  expect_s4_class(tc3, "Column")
})
test_that("Column objects of levelsType NumericRange can be created by constructor", {
  tc1 <- Column(name="alder", aliases=c("Age","age"), levelsType="NumericRange")
  tc2 <- Column(name="alder", aliases=c("Age","age"), levelsType="NumericRange", maxLevel=23)
  tc3 <- Column(name="alder", aliases=c("Age","age"), levelsType="NumericRange", maxLevel=45, minLevel=23)
  expect_s4_class(tc1, "Column")
  expect_s4_class(tc2, "Column")
  expect_s4_class(tc3, "Column")
})

test_that("Column objects of levelsType Character can be created manually", {
  tl <- Level("Test", c("Test","test","tset"))
  tl2 <- Level(name = "Test", aliases = c("Test","test","tset"))
  tc <- new("Column", name="Tests", aliases=c("test","testa"), levelsType="Character", colLevels=list(tl,tl2))
  expect_s4_class(tc,"Column")
})
test_that("Column objects of levelsType Character can be created by constructor", {
  tl <- Level("Test", c("Test","test","tset"))
  tl2 <- Level(name = "Test", aliases = c("Test","test","tset"))
  tc <- Column(name="Tests", aliases=c("test","testa"), levelsType="Character", colLevels=list(tl,tl2))
  expect_s4_class(tc,"Column")
})

test_that("Validation of Column objects works", {
  # Some argument is required:
  expect_error(Column())
  # Name should be a single string, not many:
  expect_error(Column(name=c("One","Two"),aliases="na",levelsType = "Municipalities"))
  # levelsType should be a single string, not many:
  expect_error(Column(name="n",aliases="na",levelsType = c("One","Two")))
  # levelsType should be one of NumericRange, Municipalities, or Character:
  expect_error(Column(name="n", levelsType="Hiya!"))

  # Supplying a maxLevel or minLevel to a Column where the levelsType is not NumericRange should cause a warning:
  expect_warning(Column(name="", aliases="", levelsType = "Municipalities", maxLevel = 35))
  # Supplying a colLevels to a column that does not have levelsType Character should cause a warning:
  expect_warning(Column(name="", levelsType = "NumericRange", colLevels = list(1,2,3)))

  # Supplying a colLevels that is not a list of valid level objects, or no colLevels at all, should cause an error:
  expect_error(Column(name="", levelsType = "Character"))
  expect_error(Column(name="", levelsType = "Character", colLevels = list(1,2,3)))
  # Even if one of the levels is valid, we should fail if there is an invalid object:
  tl <- Level("Test", c("Test","test","tset"))
  tl_invalid <- Level("Test", c("Test","test","tset"))
  tl_invalid@name <- c("Test","Best") # This creates an object of class Level that is not a *valid* object
  expect_error(Column(name="", levelsType = "Character", colLevels = list(1,2,tl)))
  expect_error(Column(name="", levelsType = "Character", colLevels = list(tl_invalid)))
  expect_error(Column(name="", levelsType = "Character", colLevels = list(tl, tl_invalid)))
})

test_that("Setters and getters of Column object slots works", {
  tc1 <- Column(name="alder", aliases=c("Age","age"), levelsType="NumericRange")
  tc2 <- Column(name="alder", aliases=c("Age","age"), levelsType="NumericRange", maxLevel=23)

  tl <- Level("Test", c("Test","test","tset"))
  tl2 <- Level(name = "Test", aliases = c("Test","test","tset"))
  tc3 <- new("Column", name="Tests", aliases=c("test","testa"), levelsType="Character", colLevels=list(tl,tl2))

  expect_identical(name(tc1),"alder")
  name(tc1) <- "NewName"
  expect_identical(name(tc1),"NewName")
  expect_error(name(tc1) <- c("Two", "Names"))

  expect_identical(maxLevel(tc2),23)
  maxLevel(tc2) <- 56
  expect_identical(maxLevel(tc2), 56)
  expect_error(maxLevel(tc2) <- "Not a number")

  expect_true(is.na(minLevel(tc2)))
  minLevel(tc2) <- 12
  expect_identical(minLevel(tc2), 12)
  expect_error(minLevel(tc2) <- "Not a number")

  expect_identical(colLevels(tc3), list(tl, tl2))
  colLevels(tc3) <- list(tl)
  expect_identical(colLevels(tc3), list(tl))
  expect_error(colLevels(tc3) <- list(1,2,3))
})

test_that("Dealiasing of columns works", {
  tl <- Level("Test", c("Test","test","tset"))
  tl2 <- Level(name = "Test", aliases = c("Test","test","tset"))
  tc <- new("Column", name="Tests", aliases=c("test","testa"), levelsType="Character", colLevels=list(tl,tl2))
  expect_identical(dealias(tc, "Tests"),"Tests")
  expect_identical(dealias(tc, c("test","testa","Tests","Not one of the aliases")),c("Tests","Tests","Tests"))
  expect_true(is.null(dealias(tc,"Not one of the aliases")))
})

test_that("levelDealias works for columns", {
  # Here, we need to test all three cases of levelType separately.

  # First the NumericRange case:
  tc_num1 <- Column(name="alder", aliases=c("Age","age"), levelsType="NumericRange", maxLevel=23, minLevel=12)
  tc_num2 <- Column(name="alder", aliases=c("Age","age"), levelsType="NumericRange")
  # For 22, which falls between minLevel and maxLevel for tc_num1, levelDealias should just return its input for
  # both columns. But for 11 and 25, which fall outside the range, it should be NULL for tc_num1 but the input
  # for tc_num2.
  expect_identical(levelDealias(tc_num1, 22),22)
  expect_identical(levelDealias(tc_num2, 22),22)
  expect_identical(levelDealias(tc_num1, c(12:23)), c(12:23))
  expect_identical(levelDealias(tc_num2, c(12:23)), c(12:23))
  expect_true(is.null(levelDealias(tc_num1,11)))
  expect_identical(levelDealias(tc_num2, 11),11)
  expect_true(is.null(levelDealias(tc_num1,25)))
  expect_identical(levelDealias(tc_num2, 25),25)

  # Now the Municipalities case:
  # It should return its input back if that input was the name of a Swedish municipality, otherwise it
  # should return NULL. So it succeeds on Uppsala but fails on Copenhagen.
  tc_mun <- Column(name="region", levelsType = "Municipalities")
  expect_identical(levelDealias(tc_mun,"Uppsala"),"Uppsala")
  expect_identical(levelDealias(tc_mun, c("Stockholm","Uppsala","Oslo")), c("Stockholm", "Uppsala"))
  expect_true(is.null(levelDealias(tc_mun, "Copenhagen")))

  # Finally, the Character case:
  # This is really the main case where we need this.
  tl1 <- Level(name = "Test1", aliases = c("test1","test_1","tset1"))
  tl2 <- Level(name = "Test2", aliases = c("test2","test_2","tset2"))
  tc_char <- new("Column", name="Tests", aliases=c("test","testa"), levelsType="Character", colLevels=list(tl1,tl2))
  expect_identical(levelDealias(tc_char, "test_1"), "Test1")
  expect_true(is.null(levelDealias(tc_char, "Not an alias in the levels")))
  expect_identical(levelDealias(tc_char, c("test1","tset2","Not in the data","Test1","Test2")), c("Test1","Test2","Test1","Test2"))
})
