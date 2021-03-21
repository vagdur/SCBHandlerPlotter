################################################################################
# Test of the validity of the Dealiasable class. Since this is a virtual class,
# it is a little bit suspect to be testing anything about it at all, but we do
# have some shared expectations on the behaviour of the name and aliases field
# across classes, so I think it makes sense to test them in common here.
# If we expected to at some point want name and aliases to work differently for
# different classes -- i.e. having a class implement its own validity checking
# for name and aliases -- then this would be a bad idea, but that seems very
# unlikely to happen.

test_that("Validity of Dealiasable works", {
  # We have two requirements:
  #   1. That name be a single value, not a vector, and that value not be NA or the empty string.
  #   2. That aliases not contain any NA or empty strings
  # The function will return a character containing an error message if any of the requirements are violated,
  # so the way to test this is to expect type character.
  # Note that we don't expect this function to be testing the types of its inputs, since that is already done
  # by the underlying S4 system.

  # Since Dealiasable is a virtual class, we need to define a class that inherits from it in order to test
  # its validity:
  setClass("TestClass", contains="Dealiasable")

  expect_snapshot_error(new("TestClass", name = character(0), aliases = c("a","b")))
  expect_snapshot_error(new("TestClass", name = "", aliases = c("a","b")))
  expect_snapshot_error(new("TestClass", name = NA_character_, aliases = c("a","b")))
  expect_snapshot_error(new("TestClass", name = c("a", "b"), aliases = c("a","b")))
  expect_snapshot_error(new("TestClass", name = "n", aliases = c(NA_character_,"b")))
  expect_snapshot_error(new("TestClass", name = "n", aliases = c("","b")))

  # Finally, some tests of things that should be allowed: One name and zero or more aliases:
  expect_silent(new("TestClass", name = "n", aliases = c("a","b")))
  expect_silent(new("TestClass", name = "n", aliases = "a"))
  expect_silent(new("TestClass", name = "n", aliases = character(0)))
})

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
# Tests of the Column classes:

# First we test that objects of these classes can be created:

test_that("MunicipalitiesColumn objects can be created manually",{
  tc <- new("MunicipalitiesColumn", name="region", aliases=c("Kommun", "Municipality"))
  expect_s4_class(tc, "MunicipalitiesColumn")
})
test_that("MunicipalitiesColumn objects can be created by constructor",{
  # Here, we test both the barebones constructor specific to the subclass and the general constructor
  # for all three subclasses.
  tc1 <- Column(name="region", aliases=c("Kommun", "Municipality"), levelsType="Municipalities")
  expect_s4_class(tc1, "MunicipalitiesColumn")
  tc2 <- MunicipalitiesColumn(name="region", aliases=c("Kommun", "Municipality"))
  expect_s4_class(tc2, "MunicipalitiesColumn")
})

test_that("NumericColumn objects can be created manually", {
  tc1 <- new("NumericColumn", name="alder", aliases=c("Age","age"))
  tc2 <- new("NumericColumn", name="alder", aliases=c("Age","age"), maxLevel=23)
  tc3 <- new("NumericColumn", name="alder", aliases=c("Age","age"), maxLevel=45, minLevel=23)
  expect_s4_class(tc1, "NumericColumn")
  expect_s4_class(tc2, "NumericColumn")
  expect_s4_class(tc3, "NumericColumn")
})
test_that("NumericColumn objects can be created by constructor", {
  tc1 <- Column(name="alder", aliases=c("Age","age"), levelsType="NumericRange")
  tc2 <- Column(name="alder", aliases=c("Age","age"), levelsType="NumericRange", maxLevel=23)
  tc3 <- Column(name="alder", aliases=c("Age","age"), levelsType="NumericRange", maxLevel=45, minLevel=23)
  expect_s4_class(tc1, "NumericColumn")
  expect_s4_class(tc2, "NumericColumn")
  expect_s4_class(tc3, "NumericColumn")
  tc4 <- NumericColumn(name="alder", aliases=c("Age","age"))
  tc5 <- NumericColumn(name="alder", aliases=c("Age","age"), maxLevel=23)
  tc6 <- NumericColumn(name="alder", aliases=c("Age","age"), maxLevel=45, minLevel=23)
  expect_s4_class(tc4, "NumericColumn")
  expect_s4_class(tc5, "NumericColumn")
  expect_s4_class(tc6, "NumericColumn")
})

test_that("CharacterColumn objects can be created manually", {
  tl <- Level("Test", c("Test","test","tset"))
  tl2 <- Level(name = "Test", aliases = c("Test","test","tset"))
  tc <- new("CharacterColumn", name="Tests", aliases=c("test","testa"), colLevels=list(tl,tl2))
  expect_s4_class(tc,"CharacterColumn")
})
test_that("CharacterColumn objects can be created by constructor", {
  tl <- Level("Test", c("Test","test","tset"))
  tl2 <- Level(name = "Test", aliases = c("Test","test","tset"))

  tc1 <- Column(name="Tests", aliases=c("test","testa"), levelsType="Character", colLevels=list(tl,tl2))
  expect_s4_class(tc1,"CharacterColumn")
  tc2 <- CharacterColumn(name="Tests", aliases=c("test","testa"), colLevels=list(tl,tl2))
  expect_s4_class(tc2,"CharacterColumn")
})

test_that("Column constructor can only create column objects from one of the three subclasses", {
  expect_error(Column(name="t", levelsType="Invalid type of column"))
})

# Then, we test that the validation of the values of the slots works as expected, that is,
# that we get an error if we try to create an object whose slots contains things we think
# it should not be able to contain.
#
# In order to do this, we test our expectations on the validity of the slots specific to each
# subclass -- the shared ones are handled by the validity of Dealiasable, which is tested elsewhere.
# Further, since creation of the objects has been tested, we know the validity does return TRUE for
# the cases we expect it to, so we only need to test that it correctly throws an error when we expect it to.

# A MunicipalitiesColumn object has no extra slots, so we don't need to test any cases where we expect errors.
#test_that("Validation of MunicipalitiesColumn objects works", {
#
#})

test_that("Validation of NumericColumn objects works", {
  # Here, we have two new slots -- minLevel and maxLevel -- and we expect them to be of type numeric,
  # and of length either zero or one. Passing these arguments is optional.
  expect_error(NumericColumn(name="n", minLevel = "b"))
  expect_error(NumericColumn(name="n", maxLevel = "b"))
  expect_error(NumericColumn(name="n", minLevel = c(1,2)))
  expect_error(NumericColumn(name="n", maxLevel = c(1,2)))
  expect_silent(NumericColumn(name="n"))
})

test_that("Validation of CharacterColumn objects works", {
  # Here, we have one new slot -- colLevels -- which we expect to be a list containing one or more
  # valid Levels objects. Passing this argument is not optional.
  expect_error(CharacterColumn(name="n"))
  expect_error(CharacterColumn(name="n", colLevels = "a"))
  expect_error(CharacterColumn(name="n", colLevels = list(1,2,3)))

  # Even if one of the levels is valid, we should fail if there is an invalid object:
  tl <- Level("Test", c("Test","test","tset"))
  tl_invalid <- Level("Test", c("Test","test","tset"))
  tl_invalid@name <- c("Test","Best") # This creates an object of class Level that is not a *valid* object
  expect_error(Column(name="", levelsType = "Character", colLevels = list(1,2,tl)))
  expect_error(Column(name="", levelsType = "Character", colLevels = list(tl_invalid)))
  expect_error(Column(name="", levelsType = "Character", colLevels = list(tl, tl_invalid)))
})

# Having tested the validity functions, we now test the setters and getters: (Note that we do not test that
# the setters throw errors if we try to set an invalid value. Perhaps worth doing, but breaking that coupling to
# the validities seems so exceedingly unlikely...)

test_that("Setters and getters of shared Column slots work", {
  # For simplicity, we test this on a MunicipalitiesColumn, since that subclass has no slots of its own that
  # we need to specify:
  tc <- MunicipalitiesColumn(name = "name", aliases = c("alias1","alias2"))

  expect_identical(name(tc), "name")
  name(tc) <- "Column name"
  expect_identical(name(tc), "Column name")

  expect_identical(aliases(tc), c("alias1","alias2"))
  aliases(tc) <- c("a", "b")
  expect_identical(aliases(tc), c("a", "b"))
})

test_that("Setters and getters of NumericColumn slots work", {
  # We have two extra slots to test: maxLevel and minLevel
  tc <- NumericColumn(name = "n", minLevel = 10, maxLevel = 20)
  expect_equal(maxLevel(tc), 20)
  maxLevel(tc) <- 25
  expect_equal(maxLevel(tc), 25)
  expect_equal(minLevel(tc), 10)
  minLevel(tc) <- 15
  expect_equal(minLevel(tc), 15)
})

test_that("Setters and getters of CharacterColumn slots work", {
  # Here, we have just one extra slot: colLevels
  tl1 <- Level("Test", c("Test","test","tset"))
  tl2 <- Level(name = "Test", aliases = c("Test","test","tset"))
  tc <- Column(name="Tests", aliases=c("test","testa"), levelsType="Character", colLevels=list(tl1,tl2))
  expect_identical(colLevels(tc), list(tl1, tl2))
  colLevels(tc) <- list(tl1)
  expect_identical(colLevels(tc), list(tl1))
})

# MunicipalitiesColumn currently has no slots of its own, only ones it inherits, so there's nothing to test here:
#test_that("Setters and getters of MunicipalitiesColumn slots work", {
#
#})

# Finally, we test dealiasing both of the column itself and of its levels:

test_that("Dealiasing of columns works", {
  tl <- Level("Test", c("Test","test","tset"))
  tl2 <- Level(name = "Test", aliases = c("Test","test","tset"))
  tc <- Column(name="Tests", aliases=c("test","testa"), levelsType="Character", colLevels=list(tl,tl2))
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
  tc_char <- Column(name="Tests", aliases=c("test","testa"), levelsType="Character", colLevels=list(tl1,tl2))
  expect_identical(levelDealias(tc_char, "test_1"), "Test1")
  expect_true(is.null(levelDealias(tc_char, "Not an alias in the levels")))
  expect_identical(levelDealias(tc_char, c("test1","tset2","Not in the data","Test1","Test2")),
                                         c("Test1","Test2","Test1","Test2"))

  # If one column has more than one level succeed in dealiasing, we expect an error -- it means the alias isn't unambiguous.
  tl3 <- Level(name = "Test3", aliases = c("test3","test_3","tset2")) # note "tset2" instead of "tset3" here -- so an alias reoccurs in several levels.
  tc_char_ambiguous <- Column(name="Tests", aliases=c("test","testa"), levelsType="Character", colLevels=list(tl1,tl2,tl3))
  expect_error(levelDealias(tc_char_ambiguous, "tset2"))
})

##########################################################################################################################################################
# Tests of the DocumentedTable class

# First we test that new() and the constructor DocumentedTable both are able to create DocumentedTable objects:
test_that("DocumentedTable object can be constructed manually", {
  tdt <- new("DocumentedTable", name = "name", description = "description", dataSource = "dataSource",
             dataYear = 1312, csvData = data.frame(valueColumn=c(1,2,3)), tableColumns = list(), valueColumn = "valueColumn")
  expect_s4_class(tdt, "DocumentedTable")
})

test_that("DocumentedTable object can be created by constructor", {
  tdt <- DocumentedTable(name = "name", description = "description", dataSource = "dataSource",
             dataYear = 1312, csvData = data.frame(valueColumn=c(1,2,3)), tableColumns = list(), valueColumn = "valueColumn")
  expect_s4_class(tdt, "DocumentedTable")
})

# We create one DocumentedTable object to test on, since these can be somewhat complicated objects, so it makes sense to only do this once.
# It will have one column of each levelsType, so we begin by creating the columns:
tcol_num <- Column(name = "numeric_column",
                   aliases = c("numericColumn", "NumericColumn", "numeric.column"),
                   levelsType = "NumericRange",
                   maxLevel = 10,
                   minLevel = 0)
tcol_municip <- Column(name = "municipalities_column",
                       aliases = c("municipalitiesColumn", "MunicipalitiesColumn", "municipalities.column"),
                       levelsType = "Municipalities")
tlev_1 <- Level(name = "test_level_1",
                aliases = c("testLevel1", "TestLevel1", "test.level.1"))
tlev_2 <- Level(name = "test_level_2",
                aliases = c("testLevel2", "TestLevel2", "test.level.2"))
tcol_char <- Column(name = "character_column",
                    aliases = c("characterColumn","CharacterColumn","character.column"),
                    colLevels = list(tlev_1,tlev_2),
                    levelsType = "Character")
# And now we package all the columns into one DocumentedTable to test things on:
tdt <- DocumentedTable(name = "TestTable",
                       description = "Description",
                       dataSource = "dataSource",
                       dataYear = 1111, # Wikipedia knows of almost nothing that happened this year. Synod of Rath Breasail, anyone?
                       csvData = data.frame(numeric_column = c(1,2,3),
                                            municipalities_column = c("Uppsala","Stockholm","Lund"),
                                            character_column = c("test_level_1","test_level_2","test_level_1"),
                                            valueColumn=c(100,10,1)),
                       tableColumns = list(tcol_num, tcol_municip, tcol_char),
                       valueColumn = "valueColumn")

test_that("DocumentedTable validation works", {
  # The thing we start with should be valid:
  expect_true(validObject(tdt))

  # Changing the name of the valueColumn value to the wrong thing makes it invalid:
  tdt@valueColumn <- "Wrong thing"
  expect_error(validObject(tdt))
  tdt@valueColumn <- "valueColumn"
  validObject(tdt) # We make sure we managed to restore it to correctness, so that the consequent
                   # tests which expect errors get errors for the right reason.

  # Some more ways to mess with the correspondence between the actual data and the documentation of it:
  tdt@csvData <- data.frame(numeric_column = c(1,2,3),
                            municipalities_column = c("Uppsala","Stockholm","Lund"),
                            character_column = c("test_level_1","test_level_2","test_level_1"),
                            misnamedValueColumn=c(100,10,1))
  expect_error(validObject(tdt))
  tdt@csvData <- data.frame(numeric_column = c(1,2,3),
                            municipalities_column = c("Uppsala","Stockholm","Lund"),
                            misnamedOtherColumn = c("test_level_1","test_level_2","test_level_1"),
                            valueColumn=c(100,10,1))
  expect_error(validObject(tdt))
  tdt@csvData <- data.frame( #numeric_column = c(1,2,3), # An entire column is just missing in the CSV!
                            municipalities_column = c("Uppsala","Stockholm","Lund"),
                            character_column = c("test_level_1","test_level_2","test_level_1"),
                            valueColumn=c(100,10,1))
  expect_error(validObject(tdt))
  tdt@csvData <- data.frame(numeric_column = c(1,2,3),
                            municipalities_column = c("Uppsala","Stockholm","Lund"),
                            character_column = c("test_level_1","test_level_2","test_level_1"),
                            valueColumn=c(100,10,1))
  validObject(tdt)

  # However, removing the specification of one column *shouldn't* make the object invalid, since we can still
  # query against it. Those kinds of issue are instead dealt with in the XML documentation itself, for tables
  # constructed from that.
  tdt@tableColumns <- list(tcol_num, tcol_municip)
  expect_true(validObject(tdt))
  tdt@tableColumns <- list(tcol_num, tcol_municip, tcol_char)
  validObject(tdt)

  # Each element of the columns list should be a column object, and specifically a valid such object:
  tdt@tableColumns[[1]] <- 24 # This is not a column object at all
  expect_error(validObject(tdt))
  tdt@tableColumns[[1]] <- tcol_num
  tdt@tableColumns[[1]]@name <- c("Two", "Things") # This makes the column invalid
  expect_error(validObject(tdt))
  tdt@tableColumns[[1]]@name <- "numeric_column"
  validObject(tdt)

  # All of name, description, dataSource, dataYear, and valueColumn should be required to have length 1: (i.e. have exactly one value)
  tdt@name <- c("Two", "Things")
  expect_error(validObject(tdt))
  tdt@name <- "name"
  validObject(tdt)

  tdt@description <- c("Two", "Things")
  expect_error(validObject(tdt))
  tdt@description <- "description"
  validObject(tdt)

  tdt@dataSource <- c("Two", "Things")
  expect_error(validObject(tdt))
  tdt@dataSource <- "dataSource"
  validObject(tdt)

  tdt@dataYear <- c(1, 2)
  expect_error(validObject(tdt))
  tdt@dataYear <- 1111
  validObject(tdt)

  tdt@valueColumn <- c("Two", "Things")
  expect_error(validObject(tdt))
  tdt@valueColumn <- "valueColumn"
  validObject(tdt)
})

# Having tested that validation works, we now test that the setters and getters do what is expected:
test_that("Setters and getters of DocumentedTable work", {
  # For each slot of the class, we test three things:
  # 1.  Its getter returns its value,
  # 1.  if we use its setter to set a new value, that is the value the slot has,
  # 2.  we get an error if we try to set it to something invalid. (Note that we don't need a large assortment
  #     of invalid things to try to set it to -- what we're checking is that validObject is called, so we just need
  #     one invalid value. Testing of the validity function itself is in a different test.)

  # The first few things are very simple, just primitives that take certain values:
  expect_identical(name(tdt),"TestTable")
  name(tdt)<-"A new name"
  expect_identical(tdt@name, "A new name")
  expect_error(name(tdt) <- c("Two","Things"))

  expect_identical(description(tdt),"Description")
  description(tdt)<-"A new description"
  expect_identical(tdt@description, "A new description")
  expect_error(description(tdt) <- c("Two","Things"))

  expect_identical(dataSource(tdt),"dataSource")
  dataSource(tdt)<-"Something else"
  expect_identical(tdt@dataSource, "Something else")
  expect_error(dataSource(tdt) <- c("Two","Things"))

  expect_identical(dataYear(tdt),1111)
  dataYear(tdt)<-1066 # Battle of Hastings!
  expect_identical(tdt@dataYear, 1066)
  expect_error(name(tdt) <- c(1456, 1865))

  # In order to test the setter and getter of csvData, we obviously need to define our data frames, both
  # the one it currently contains and the one we want to change it to:
  currentCsvData <- data.frame(numeric_column = c(1,2,3),
                               municipalities_column = c("Uppsala","Stockholm","Lund"),
                               character_column = c("test_level_1","test_level_2","test_level_1"),
                               valueColumn=c(100,10,1))
  newCsvData <- data.frame(numeric_column = c(1,2,3,4),
                           municipalities_column = c("Uppsala","Stockholm","Lund","Lomma"),
                           character_column = c("test_level_1","test_level_2","test_level_1","test_level_2"),
                           valueColumn=c(100,10,1,0.1))
  expect_equal(csvData(tdt), currentCsvData)
  csvData(tdt) <- newCsvData
  expect_equal(csvData(tdt),newCsvData)
  expect_error(csvData(tdt) <- data.frame()) # An empty data frame lacks the valueColumn, so this is invalid and should give an error.
  csvData(tdt) <- currentCsvData

  # We also test getting and setting of tableColumns. Note that setting additional columns that are not present in the csvData should
  # cause an error, but it is permissible to have columns in the csvData that do not match columns in the tableColumns --all documented columns
  # must exist, but not all columns must be documented. (Though they do need to be documented to be queried...)
  expect_equal(tableColumns(tdt), list(tcol_num, tcol_municip, tcol_char))
  tableColumns(tdt) <- list(tcol_num, tcol_municip) # Removing one column from documentation should work...
  expect_equal(tableColumns(tdt), list(tcol_num, tcol_municip))
  expect_error(tableColumns(tdt) <- list(1,2,3))
  tableColumns(tdt) <- list(tcol_num, tcol_municip, tcol_char)


  # Setters and getters of valueColumn are strange, since it always needs to be a column in the csvData. Perhaps there should only be a
  # getter, not a setter? We test anyway, expecting errors:
  expect_identical(valueColumn(tdt),"valueColumn")
  expect_error(valueColumn(tdt)<-"A new name") # The value column is checked against the csvData, so changing it should always give an error
                                               # unless the csvData has been manually changed.
  expect_error(name(tdt) <- c("valueColumn","valueColumn")) # Even if the value is correct wrt to csvData, there should still only be one.
  #TODO: Remove setter of valueColumn, since it should almost always give an error.
})

# Next up, we test the columnDealias method:
test_that("columnDealias works", {
  # First we test a few things against the correctly formatted table:
  expect_identical(columnDealias(tdt, "MunicipalitiesColumn"), "municipalities_column")
  expect_identical(columnDealias(tdt, c("MunicipalitiesColumn", "municipalities_column")),
                   c("municipalities_column", "municipalities_column"))
  expect_identical(columnDealias(tdt, c("MunicipalitiesColumn", "character.column")),
                   c("municipalities_column", "character_column"))
  expect_null(columnDealias(tdt, "another_column_name"))
  expect_identical(columnDealias(tdt, c("MunicipalitiesColumn", "misplaced_column_name", "municipalities_column")),
                   c("municipalities_column", "municipalities_column"))
  # Then, we make sure that if two columns share an alias, we get an error trying to dealias that alias:
  aliases(tdt@tableColumns[[1]]) <- c("numericColumn", "NumericColumn", "numeric.column","municipalities_column")
  expect_error(columnDealias(tdt,"municipalities_column"))
  aliases(tdt@tableColumns[[1]]) <- c("numericColumn", "NumericColumn", "numeric.column")
})

# Now comes the time to test the interesting stuff: The canHandleQuery and query methods.
# We test them together, since it makes sense to do so: We have a bunch of test queries,
# and for the ones that it should be able to handle, we also test that it returns the right
# thing. These tests are not quite complete -- there are probably some edge cases that aren't tested,
# but given the way the algorithm works, it should be impossible for them to fail. Hopefully any
# edge case violations are caught by the tests of querying against the real tables.
# If the algorithm is changed, more tests should be added.

# In fact, we want to add more data to our tdt, in order to be able to test a few more cases
# that don't exist in the one we've been using so far:
tcol_num <- Column(name = "numeric_column",
                   aliases = c("numericColumn", "NumericColumn", "numeric.column"),
                   levelsType = "NumericRange",
                   maxLevel = 10,
                   minLevel = 0)
tcol_municip <- Column(name = "municipalities_column",
                       aliases = c("municipalitiesColumn", "MunicipalitiesColumn", "municipalities.column"),
                       levelsType = "Municipalities")
tlev_1 <- Level(name = "test_level_1",
                aliases = c("testLevel1", "TestLevel1", "test.level.1"))
tlev_2 <- Level(name = "test_level_2",
                aliases = c("testLevel2", "TestLevel2", "test.level.2"))
tlev_3 <- Level(name = "test_level_3",
                aliases = c("testLevel3", "TestLevel3", "test.level.3"))
tcol_char <- Column(name = "character_column",
                    aliases = c("characterColumn","CharacterColumn","character.column"),
                    colLevels = list(tlev_1,tlev_2,tlev_3),
                    levelsType = "Character")
# And now we package all the columns into one DocumentedTable to test things on:
tdt <- DocumentedTable(name = "TestTable",
                       description = "Description",
                       dataSource = "dataSource",
                       dataYear = 1111, # Wikipedia knows of almost nothing that happened this year. Synod of Rath Breasail, anyone?
                       csvData = data.frame(numeric_column = c(1,2,3,3,2),
                                            municipalities_column = c("Uppsala","Stockholm","Lund","Skara","Uppsala"),
                                            character_column = c("test_level_1","test_level_2","test_level_1","test_level_2","test_level_2"),
                                            valueColumn=c(100, 10, 1, 0.1, 0.01)),
                       tableColumns = list(tcol_num, tcol_municip, tcol_char),
                       valueColumn = "valueColumn")

test_that("canHandleQuery and query work", {
  # For reference, here is the definition of the data that tdt contains:
  #   numeric_column = c(1,2,3,3,2),
  #   municipalities_column = c("Uppsala","Stockholm","Lund","Skara","Uppsala"),
  #   character_column = c("test_level_1","test_level_2","test_level_1","test_level_2","test_level_2"),
  #   valueColumn=c(100, 10, 1, 0.1, 0.01)
  # and the definition of the levels of the character column:
  #   tlev_1 <- Level(name = "test_level_1",
  #                   aliases = c("testLevel1", "TestLevel1", "test.level.1"))
  #   tlev_2 <- Level(name = "test_level_2",
  #                   aliases = c("testLevel2", "TestLevel2", "test.level.2"))
  #   tlev_3 <- Level(name = "test_level_3",
  #                   aliases = c("testLevel3", "TestLevel3", "test.level.3"))
  # and recall that the numeric column is restricted to be between 0 and 10

  # If we make no restrictions at all, of course any table can handle that, and should just
  # be returning the sum of their valueColumn:
  expect_true(canHandleQuery(tdt))
  expect_identical(query(tdt), 111.11)

  # Restricting the municipality column to be "Uppsala" should be possible, and should return
  # the sum of value for the first and last row, i.e. 100.01. It should also work if we supply an alias for that
  # column.
  expect_true(canHandleQuery(tdt, municipalities_column = "Uppsala"))
  expect_identical(query(tdt, municipalities_column = "Uppsala"), 100.01)
  expect_true(canHandleQuery(tdt, municipalitiesColumn = "Uppsala"))
  expect_identical(query(tdt, municipalitiesColumn = "Uppsala"), 100.01)
  # Specifying both Lund and Uppsala should work, and should return 101.01, the sum of their values.
  expect_true(canHandleQuery(tdt, municipalities_column = c("Lund", "Uppsala")))
  expect_identical(query(tdt, municipalities_column = c("Lund", "Uppsala")), 101.01)
  # Specifying Lomma, which is not in our data, should be possible but return 0, and if verbose=TRUE,
  # should give a message:
  expect_true(canHandleQuery(tdt, municipalities_column = "Lomma"))
  expect_identical(query(tdt, municipalities_column = "Lomma"), 0)
  expect_message(query(tdt, municipalities_column = "Lomma", verbose=TRUE))
  expect_silent(query(tdt, municipalities_column = "Lomma", verbose=FALSE))

  # Similarly, restrictions on the numeric column should work in the same way:
  expect_true(canHandleQuery(tdt, numeric_column = 1))
  expect_identical(query(tdt, numeric_column = 1), 100)
  expect_true(canHandleQuery(tdt, numericColumn = 1))
  expect_identical(query(tdt, numericColumn = 1), 100)
  expect_true(canHandleQuery(tdt, numeric_column = c(1,2)))
  expect_identical(query(tdt, numeric_column = c(1,2)), 110.01)
  expect_true(canHandleQuery(tdt, numeric_column = c(5,6)))
  expect_identical(query(tdt, numeric_column = c(5,6)), 0)
  expect_message(query(tdt, numeric_column = c(5,6), verbose=TRUE))
  expect_silent(query(tdt, numeric_column = c(5,6), verbose=FALSE))

  # Finally, restrictions on columns should also work in the same way. Here, we also expect to be able
  # to supply aliases for the levels, not just the column name.
  #TODO: There's a subtle disconnect here: In the documentation, the actual column names are hidden, and
  # we expect the user to always refer to them by their "identifier" (which this code just treats as an alias)
  # or an alias, but the code actually permits referring to columns by their name in the CSV as well. What should
  # be done about this?
  expect_true(canHandleQuery(tdt, character_column = "test_level_1"))
  expect_equal(query(tdt, character_column = "test_level_1"),101)
  expect_true(canHandleQuery(tdt, characterColumn = "testLevel1"))
  expect_equal(query(tdt, characterColumn = "testLevel1"),101)
  expect_true(canHandleQuery(tdt, characterColumn = c("testLevel1", "test_level_2")))
  expect_equal(query(tdt, characterColumn = c("testLevel1", "test_level_2")),111.11)
  # Queries for levels that are not present should be possible, but if verbose is TRUE and this filter results in
  # there being zero matching posts, we want a message:
  expect_message(query(tdt, character.column = "testLevel3", verbose=TRUE))
  expect_silent(query(tdt, character.column = "testLevel3", verbose=FALSE))

  # So now we've extensively tested that queries against each column work. Time to do a few tests that queries against
  # multiple columns at once behave like we expect them to. Reminder of what the data looks like:
  #   numeric_column = c(1,2,3,3,2),
  #   municipalities_column = c("Uppsala","Stockholm","Lund","Skara","Uppsala"),
  #   character_column = c("test_level_1","test_level_2","test_level_1","test_level_2","test_level_2"),
  #   valueColumn=c(100, 10, 1, 0.1, 0.01)
  # and the definition of the levels of the character column:
  #   tlev_1 <- Level(name = "test_level_1",
  #                   aliases = c("testLevel1", "TestLevel1", "test.level.1"))
  #   tlev_2 <- Level(name = "test_level_2",
  #                   aliases = c("testLevel2", "TestLevel2", "test.level.2"))
  #   tlev_3 <- Level(name = "test_level_3",
  #                   aliases = c("testLevel3", "TestLevel3", "test.level.3"))

  # If we specify that characterColumn should be test_level_1 and municipality should be Uppsala, this
  # should onlt match the first instance of Uppsala, and return 100.
  expect_true(canHandleQuery(tdt, characterColumn = "testLevel1", municipalitiesColumn = "Uppsala"))
  expect_identical(query(tdt, characterColumn = "testLevel1", municipalitiesColumn = "Uppsala"), 100)

  # If we specify that numericColumn should be 2 and characterColumn should be testLevel2, this should
  # match both the Stockholm and the second Uppsala entry, giving us 10.01:
  expect_true(canHandleQuery(tdt, characterColumn = "testLevel2", numericColumn = 2))
  expect_identical(query(tdt, characterColumn = "testLevel2", numericColumn = 2), 10.01)

  # If we specify that numeric_column should be 3 and municipality should be Uppsala, this should be possible,
  # but should not match any entries and thus return 0, and should give a message iff verbose is TRUE.
  expect_true(canHandleQuery(tdt, numeric_column = 3, MunicipalitiesColumn = "Uppsala"))
  expect_identical(query(tdt, numeric_column = 3, MunicipalitiesColumn = "Uppsala"), 0)
  expect_message(query(tdt, numeric_column = 3, MunicipalitiesColumn = "Uppsala", verbose=TRUE))
  expect_silent(query(tdt, numeric_column = 3, MunicipalitiesColumn = "Uppsala", verbose=FALSE))

  # If the query matches some row where the valueColumn is NA, we want a message about this if
  # verbose is TRUE, otherwise not:
  tdt@csvData$valueColumn[5] <- NA # Was originally 0.01
  expect_message(query(tdt, MunicipalitiesColumn = "Uppsala", verbose=TRUE))
  expect_silent(query(tdt, MunicipalitiesColumn = "Uppsala", verbose=FALSE))
  tdt@csvData$valueColumn[5] <- 0.01

  # Having checked that a lot of queries it should be able to handle are indeed reported as possible to handle
  # and give the correct result, let us check that it correctly reports itself unable to handle queries it
  # should not be able to handle:

  # It should be unable to handle a query if...
  #   ...we supply it with a column name it doesn't have:
  expect_false(canHandleQuery(tdt, NotARealColumn = 5))
  #   ...we supply it with a level of a character column that column doesn't have:
  expect_false(canHandleQuery(tdt, characterColumn = "Not a real level"))
  #   ...even if some of the values of levels are present, they should *all* be present for it to say yes to the query:
  expect_false(canHandleQuery(tdt, characterColumn = c("test_level_1","test_level_2","Not a real level")))
  #   ...if we try to give a character column something that isn't even a character:
  expect_false(canHandleQuery(tdt, characterColumn = 15))
  #   ...if we try to give a name of something that isn't a real municipality to a municipality column:
  expect_false(canHandleQuery(tdt, municipalitiesColumn = "Oslo")) # 26 October 1905 never forget!
  #   ...if we try to supply a numeric column with something that isn't numeric:
  expect_false(canHandleQuery(tdt, numericColumn = "Cat"))
  #   ...if we try to supply a numeric column with something that is out of its range:
  expect_false(canHandleQuery(tdt, numericColumn = 1000000))
  expect_false(canHandleQuery(tdt, numericColumn = -1000000))
  #   ...if we supply it with several parameters and even just one of them is wrong:
  expect_false(canHandleQuery(tdt, municipalityColumn="Lomma", characterColumn = "Not a real level"))
  expect_false(canHandleQuery(tdt, characterColumn="test_level_1", numericColumn = -10000))

  # Finally, we need to check that checkHandleable and failIfUnable parameters behave as expected.
  # If checkHandleable is false, and we supply an unhandleable query (which should never happen -- that
  # parameter should only be FALSE if handleability has already been checked), we expect errors to happen:
  expect_error(query(tdt, NotARealColumn = "Not a real level", checkHandleable = FALSE))

  # If checkHandleable is TRUE and we supply an unhandleable query, what happens should depend on the value of
  # failIfUnable: If it is TRUE, we expect an error, otherwise, we expect a warning and NA.
  expect_error(query(tdt, NotARealColumn = "Not a real level", checkHandleable = TRUE, failIfUnable = TRUE))
  expect_warning(expect_equal(query(tdt, NotARealColumn = "Not a real level", checkHandleable = TRUE, failIfUnable = FALSE), NA_real_))
})
