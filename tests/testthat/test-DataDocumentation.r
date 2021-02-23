# Check a single table documentation XML file.
# This checks three things:
# 1. It is actually properly formed XML that can be read by an XML parser
# 2. It conforms to the XML Schema for this sort of file. (Found in the same folder as the XML files themselves.)
# 3. It additionally has properly formed columns. (This can't be checked with an XML Schema, I think, since XML is context-free.) That is:
#     a)  A column of levelsType "Municipalities" contains no additional information on levels, one of levelsType NumericRange contains
#         at most information on upper and lower limits of the range, and one of levelsType Character contains information on at least
#         one level.
#     b)  No two columns share a name or share an alias. (And no single column has the same alias listed twice, or an alias equal to the identifier)
#     c)  No two levels of one column share a name or share an alias. (And no single level has an alias equal to the levelName or a duplicate alias)
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
  if(!file.exists(filename)) {
    return(FALSE)
  }

  # Then, let's check that it is valid XML:
  test_that(paste("file ",fileShortName," is properly formed XML"),{
            parsedXML <- xml2::read_xml(filename, encoding="UTF-8")
            expect_true(exists("parsedXML"))
            })
  # Having done that, let's actually read it in, and also read in the XSD file containing our Schema.
  parsedXML <- xml2::read_xml(filename, encoding="UTF-8")
  xmlSchema <- xml2::read_xml(system.file("extdata", "TableDocumentationFormat.xsd", package = "SCBHandlerPlotter"), encoding="UTF-8")

  # Now, let's test that the XML matches the Schema:
  test_that(paste("file ",fileShortName," conforms to the XML Schema"), {
    expect_true(xml2::xml_validate(parsedXML, xmlSchema))
  })

  # If the XML fails to conform to our schema, it makes no sense to continue testing, so we return FALSE to indicate
  # the tests failed: (We need to do this since the caller of the function needs to know whether to continue testing.)
  if (!xml2::xml_validate(parsedXML, xmlSchema)) {
    return(FALSE)
  }

  # Now the easy part of our testing is done -- the file does exist, and it is a proper XML file which conforms to
  # our XML Schema. It remains to test the things that cannot be checked just using a context-free grammar: That the
  # column specifications actually make sense for a table.
  # Specifically, what we need to check is that:
  #     a)  All columns levels specification are correct for their levelsType:
  #           i) a column of levelsType "Municipalities" contains no additional information on levels
  #           ii) a column of levelsType NumericRange contains at most information on upper and lower limits of the range
  #           iii) a column of levelsType Character contains information on at least one level
  #     b)  No two columns share an identifier or share an alias
  #     c)  Within each column, its identifier and aliases are all distinct
  #     d)  No two levels of one column share a name or share an alias
  #     e)  Within each level of each column, its name and aliases are all distinct
  # Points c) and e) strictly speaking probably wouldn't cause any errors if violated, but they're indicative that something
  # has gone wrong, so we still enforce them just to prevent bugs from arising later.

  # The first thing we do is turn the parsed XML into a nested list, since that is easier to work with. Note that, thanks to
  # the XML Schema validation, we know quite a bit about its structure already, so for example we can immediately jump into
  # the <table></table> tags and then into the <columns></columns> tags, which is where we need to be working:
  columnsList <- xml2::as_list(parsedXML)$table$columns

  # The way to structure this is that we loop over all columns, and for each column we check a) and c)-e), and aggregate a
  # list of names and aliases of all the columns. Then, once the loop is done, we check that this list does not contain any
  # duplicate elements.
  columnIdentifiersAndAliases <- character(0)
  for (columnIndex in 1:(length(columnsList)-1)) { # -1 because the final element is the <valueColumn>, which was checked via XSD already.
    currentColumn <- columnsList[[columnIndex]]
    # The very first thing we can check is c), that this column has distinct identifier and aliases:
    currentColumnIdAndAliases <- character(0)
    # Append the identifier to our list of identifiers and aliases:
    currentColumnIdAndAliases <- append(currentColumnIdAndAliases, currentColumn$identifier[[1]][1])
    # We know that if there are no aliases, then we only have identifier, colname, and then levels. So conversely, if the total number
    # of tags is greater than three, we know the third until the n-1th tag must be an alias:
    if(length(currentColumn) > 3) {
      for (tagIndex in 3:(length(currentColumn)-1)) {
        currentColumnIdAndAliases <- append(currentColumnIdAndAliases, currentColumn[[tagIndex]][[1]][1])
      }
    }
    # Having gathered this list, we test that it consists of distinct elements:
    test_that(paste("identifier and aliases of column",currentColumn$identifier[[1]][1],"in",fileShortName,"are all distinct"),{
      # Here, instead of just checking if length(unique(x))==length(x), we compute which if any elements are duplicates, so that
      # the fail message actually tells you what the duplicates are:
      duplicates <- unique(currentColumnIdAndAliases[duplicated(currentColumnIdAndAliases)])
      expect_equal(duplicates,character(0))
    })
    # Note that we don't stop testing here if this fails, since the rest of our tests will still make sense.

    # At this point, we actually append unique(currentColumnnIdAndAliases) to the total list of identifiers and aliases, not
    # the list itself. The logic here is that if there is a duplicate inside this column, that will already have been caught
    # by the test just above, so if we add the entire list, we just pollute the fail message of the test of condition b) --
    # we want to be easily able to see failures of b) that occur because of clashes between different columns, and let clashes
    # within a column be flagged by failures of c). So:
    columnIdentifiersAndAliases <- append(columnIdentifiersAndAliases, unique(currentColumnIdAndAliases))

    # Now, we can move into the levels tag to check first a) and then d) and e):
    currentLevels <- currentColumn$levels
    currentLevelsType <- currentLevels$levelsType[[1]][1]
    # Different tests depending on the value of levelsType:
    if (currentLevelsType == "Municipalities") {
      # In this case. there should be no further information at all than the levelsType:
      test_that(paste("column",currentColumn$identifier[[1]][1],"in",fileShortName,", documented to have <levelsType> Municipalities, contains no further information on levels"),{
        expect_equal(length(currentLevels),1) # Easiest way to check this is just that the number of tags is one
      })
    } else if(currentLevelsType == "NumericRange") {
      # In this case, there are two allowed extra tags (<minLevel> and <maxLevel>), so we count how many of them are present,
      # and then check that the total number of tags is correct:
      expectedNumberOfTags <- 1 + as.integer(!is.null(currentLevels$minLevel)) + as.integer(!is.null(currentLevels$maxLevel))
      test_that(paste("column",currentColumn$identifier[[1]][1],"in",fileShortName,", documented to have <levelsType> NumericRange, contains no further information on levels other than possibly minRange and maxRange"),{
        expect_equal(length(currentLevels),expectedNumberOfTags)
      })
    } else if(currentLevelsType == "Character") {
      # Here, we check d) and e), that is, that different levels have different identifiers and aliases, and also within
      # levels there are no duplicate aliases/identifiers. First, however, we check that there is no attempt to specify
      # maxLevek or minLevel:
      test_that(paste("column",currentColumn$identifier[[1]][1],"in",fileShortName,", documented to have <levelsType> Character, does not have a minLevel specified"),{
        expect_true(is.null(currentLevels$minLevel))
      })
      test_that(paste("column",currentColumn$identifier[[1]][1],"in",fileShortName,", documented to have <levelsType> Character, does not have a maxLevel specified"),{
        expect_true(is.null(currentLevels$maxLevel))
      })
      # We actually need to break testing here if this fails, since our later tests will assume a Character column contains
      # first its <levelsType> and then one or more <level> tags, and nothing else. This assumption causes some unclear error if we let a file
      # which violates it through -- thus, better to break testing here than to output hard-to-understand errors later.
      if (!is.null(currentLevels$maxLevel)|!is.null(currentLevels$minLevel)) {
        return(FALSE)
      }

      # Having verified this, we know that all the remaining tags after <levelsType> are <level> tags. So we loop through them precisely
      # like we looped through the columns, aggregating a list of ids and aliases that appear:
      levelIdsAndAliases <- character(0)
      for (levelIndex in 2:length(currentLevels)) {
        currentLevel <- currentLevels[[levelIndex]]
        # Since a <level> tag only contains the levelName and its aliases, and we don't distinguish between these, we can
        # just do a sapply to extract them all:
        currentLevelIdAndAliases <- sapply(currentLevel, function(x) x[[1]][1])
        # Now we test that this list contains no duplicates:
        test_that(paste("level",currentLevelIdAndAliases[1],"of column",currentColumn$identifier[[1]][1],"in",fileShortName,"contains no duplicate aliases or alias duplicating the levelName"),{
          duplicates <- unique(currentLevelIdAndAliases[duplicated(currentLevelIdAndAliases)])
          expect_equal(duplicates,character(0))
        })
        # By the same logic as for the column ids and aliases, we append the unique ones from this level to the list for all levels:
        levelIdsAndAliases <- append(levelIdsAndAliases, unique(currentLevelIdAndAliases))
      }
      # And now we can test that no two different levels of the same column share a name or share an alias:
      test_that(paste("no two distinct levels of column",currentColumn$identifier[[1]][1],"in",fileShortName,"share a name or an alias"),{
        duplicates <- unique(levelIdsAndAliases[duplicated(levelIdsAndAliases)])
        expect_equal(duplicates,character(0))
      })
    } else {
      # Entering this case should be impossible, since the XML Schema says that levelsType has to have one of the three
      # values "Municipalities", "NumericRange", or "Character". So if we enter this, then the XSD must have been changed
      # without updating the tests:
      stop("Something seems to have gone wrong with the XSD, or it has been updated without updating the tests.")
    }
  }

  # Having looped through all columns, we now have a list of the identifiers and aliases of all columns. So, finishing
  # up, we test condition b), that this list of identifiers and aliases contains no duplicate elements:
  test_that(paste("distinct columns in",fileShortName,"do not have identical identifiers or aliases"),{
    # Here, instead of just checking if length(unique(x))==length(x), we compute which if any elements are duplicates, so that
    # the fail message actually tells you what the duplicates are:
    duplicates <- unique(columnIdentifiersAndAliases[duplicated(columnIdentifiersAndAliases)])
    expect_equal(duplicates,character(0))
  })

  # If we didn't encounter any errors in execution, and didn't return FALSE at any earlier point, we now return TRUE to
  # indicate the file passed all its tests and we can proceed to test if the documentation is true: (So far we only tested
  # that it is of the right format.)
  return(TRUE)
}


# Check that the documentation of the XML file matches the actual data
#
# This function actually does two things: First it checks that the file in question conforms to the format for a data
# documentation file, using the function checkFormatConformity, and then (assuming the file it got can indeed be the
# the documentation of a table) it checks that the documentation is correct.
#
# To do this, it checks:
#   1. That the file it claims to be documenting exists
#   2. That the file it claims to be documenting is indeed a csv
#   3. That the csv has exactly the columns the documentation claims it has
#   4. That each column's levels are correctly specified:
#       a)  A column with levelsType set to Municipalities contains only names of municipalities.
#       b)  A column with levelsType set to NumericRange is numeric, and if an upper or lower bound is specified,
#           then all values fall above (below) the lower (upper) bound. Note that we do not require that every number
#           inside the actual range occurs.
#       c)  A column with levelsType set to Character contains only elements as specified by one of the <level> tags,
#           and every level specified by a <level> tag actually occurs. Note the slight difference to our assumptions on
#           a NumericRange column.
#   5. That the valueColumn is numeric, possibly with NAs.

checkDocumentationCorrectness <- function(filename) {
  # To begin with, we check that it makes sense to check documentation correctness -- that is, that the file really
  # is a potentially correct documentation file:
  if(!checkFormatConformity(filename)) {
    # If this failed, it makes no sense to continue, so we return FALSE:
    return(FALSE)
  }

  # For prettier output from our tests, let's just write the name of the file, not its whole path:
  fileShortName <- basename(filename)

  # Knowing that the XML makes sense, we read it in and cast it to a super-nested list to work with:
  parsedXML <- xml2::read_xml(filename, encoding="UTF-8")
  docList <- xml2::as_list(parsedXML)$table

  # The first thing to do is to find the csv file it says it documents and test that it exists:
  csvFilename <- docList$filename[[1]][1] # The [[1]][1] is for some reason how the list we get is structured?
  test_that(paste("file ",csvFilename," which ",fileShortName," claims to document exists",sep=""),{
    expect_true(file.exists(system.file("extdata",csvFilename, package="SCBHandlerPlotter")))
  })
  if (!file.exists(system.file("extdata",csvFilename, package="SCBHandlerPlotter"))) {
    # If it doesn't exist, we can't continue:
    return(FALSE)
  }
  # If it does exist, we can read it in:
  dataTable <- read.csv(system.file("extdata",csvFilename, package="SCBHandlerPlotter"), fileEncoding = "UTF-8")

  # Having read it in, we check the valueColumn specified exists and is numeric:
  docValueColumn <- docList$columns$valueColumn$colname[[1]][1]
  test_that(paste("the valueColumn of",fileShortName,"exists in the actual table"),{
    expect_true(docValueColumn %in% colnames(dataTable))
  })
  if (!(docValueColumn %in% colnames(dataTable))) {
    # If the valueColumn doesn't exist, we can't check that it is numeric, obviously. No need to stop the testing here, though,
    # since nothing else depends on this test passing.
  } else {
    # If it does exist, we test that it is numeric:
    test_that(paste("the valueColumn of",fileShortName,"is numeric in the table"),{
      expect_true(is.numeric(dataTable[,docValueColumn])) # expect_type doesn't work here since integers and floats are different types, but both give true here
    })
  }

  # Now that we have picked out and validated the existence of the valueColumn, we can look at the rest of the columns,
  # and validate each of them.
  docColumns <- docList$columns
  # Our first test, however, is just that we have the right number of columns -- that is, the number of objects inside the <columns>
  # block is the same as the number of columns of the actual table. If they are equal, and we also check for each column in the XML
  # that it corresponds to one in the real table, then we will know that we have a one-to-one correspondence between columns in the
  # documentation and columns in the real table. (That column names are unique in the XML was checked already by checkFormatConformity,
  # and uniqueness of column names in the data is enforced by R, possibly by renaming on read.)
  test_that(paste("the number of columns documented in",fileShortName,"matches the number of columns in the data"),{
    expect_equal(length(docColumns),ncol(dataTable))
  })

  for (columnNumber in 1:(length(docColumns) - 1)) { # -1 because the final column is the valueColumn, which we've already checked.
    docColumn <- docColumns[[columnNumber]]
    # What is the documented name of this column?:
    docColumnName <- docColumn$colname[[1]][1]
    # Test that the column really exists:
    test_that(paste("column",docColumnName,"documented by",fileShortName,"exists in actual data"),{
      expect_true(docColumnName %in% colnames(dataTable))
    })
    if (!(docColumnName %in% colnames(dataTable))) {
      next # If the column doesn't actually exist, we can't test that its levels are as described
    }

    # Now, let's proceed to check that the description of the levels is correct. There are three cases,
    # depending on levelsType. (That it is one of the three was checked already by the XSD.)
    docLevelsType <- docColumn$levels$levelsType[[1]][1]
    if (docLevelsType == "Municipalities") {
      # Here, we need to check that every level of the column is the name of a municipality:
      test_that(paste("the column",docColumnName,"of",fileShortName,", documented to contain municipalities, does contain names of municipalities"),{
        expect_true(all(dataTable[,docColumnName] %in% municipalityNames))
      })
    } else if (docLevelsType == "NumericRange") {
      # First, we test that the column is in fact numeric:
      test_that(paste("the column",docColumnName,"of",fileShortName,"is numeric in the table"),{
        expect_true(is.numeric(dataTable[,docColumnName])) # expect_type doesn't work here since integers and floats are different types, but both give true here
      })
      # If a minLevel is specified, we test that there are no values below it:
      if(!is.null(docColumn$levels$minLevel[[1]][1])) {
        docColMinLevel <- as.numeric(docColumn$levels$minLevel[[1]][1])
        test_that(paste("the column",docColumnName,"of",fileShortName,", documented to have minLevel",docColMinLevel,"takes no values below its minLevel"),{
          expect_lte(docColMinLevel, min(dataTable[,docColumnName]))
        })
      }
      # If a maxLevel is specified, we test that there are no values above it:
      if(!is.null(docColumn$levels$maxLevel[[1]][1])) {
        docColMaxLevel <- as.numeric(docColumn$levels$maxLevel[[1]][1])
        test_that(paste("the column",docColumnName,"of",fileShortName,", documented to have maxLevel",docColMaxLevel,"takes no values above its maxLevel"),{
          expect_gte(docColMaxLevel, max(dataTable[,docColumnName]))
        })
      }
    } else if (docLevelsType == "Character") {
      # If the type is specified to be Character, we need to look at the list of <level> objects, look at the <levelName> of each,
      # and create a list of the values that occur. Then we test that this list contains exactly the same elements as the column in
      # the data.
      docLevelsList <- docColumn$levels
      documentationLevels <- character(0)
      for (levelIndex in 2:length(docLevelsList)) { # 2, because the first element is the <levelsType> tag.
        # Store the name of the level in levelsSeen:
        documentationLevels[levelIndex-1] <- docLevelsList[[levelIndex]]$levelName[[1]][1] # -1 because obviously documentationLevels should index from 1, not 2 (Got a bug from that before...)
      }
      # Having gathered our list of the levels the documentation says there are, we test two things:
      #   1. Every level in the actual data also occurs in the documentation
      #   2. Every level in the documentation actually occurs in the data
      test_that(paste("Every level of column",docColumnName,"in the actual data also occurs in the documentation",fileShortName),{
        # setdiff computes the set-minus operation A\B -- so setdiff(A,B) is all elements of A which do not also occur in B.
        # So here we take the actually occurring values and remove the ones that are documented, and we expect to get the
        # empty list, since we expect all occurring levels to be documented.
        expect_equal(setdiff(dataTable[,docColumnName], documentationLevels), character(0))
      })
      test_that(paste("Every level of column",docColumnName,"documented in",fileShortName,"occurs in the actual data"), {
        # Same logic as before, except we do B\A instead, since we want to check that every column the documentation says
        # exists actually exists.
        expect_equal(setdiff(documentationLevels, dataTable[,docColumnName]), character(0))
      })
    } else {
      # Entering this case should be impossible, since the XML Schema says that levelsType has to have one of the three
      # values "Municipalities", "NumericRange", or "Character". So if we enter this, then the XSD must have been changed
      # without updating the tests:
      stop("Something seems to have gone wrong with the XSD, or it has been updated without updating the tests.")
    }
  }

  # We need a return value here, so that the code executing this function can itself be a test. Otherwise the testing
  # will abort as soon as we get an error in here, and so not all XML files would get tested as soon as one fails. Thus:
  return(TRUE)
}

# Our very first test should be that we can actually read in the XML Schema we need to validate our XML files:
test_that("XML Schema can be read",{
  xmlSchema <- xml2::read_xml(system.file("extdata", "TableDocumentationFormat.xsd", package = "SCBHandlerPlotter"), encoding="UTF-8")
  expect_true(exists("xmlSchema"))
})

# We assume that every file in extdata with the .xml file ending is a specification of a table:
extdataLocation <- system.file("extdata", package = "SCBHandlerPlotter")
tablesToCheck <- list.files(extdataLocation, pattern="*\\.xml$")
# Then we just loop over all the files and run our tests on each of them:
for (tableToCheck in tablesToCheck) {
  filename <- paste(extdataLocation,"/",tableToCheck,sep="")
  # We wrap this in a test so that the loop won't stop just because one file fails its tests.
  # If this test fails, there'll also be more granular tests that have failed:
  test_that(paste("documentation ",tableToCheck," is correct"),{
    expect_true(checkDocumentationCorrectness(filename))
  })
}
