# This file contains a single function, which takes as its argument the path to
# an XML file documenting a table, and returns a DocumentedTable object created
# based on that XML, which can be queried against.
# Note that this function assumes the XML file has already been validated by the
# tests of documentation! If you try to pass it an untested XML file, or one in
# an incorrect format, weird things may happen.

#TODO: Is it possible to call on the tests of the library to test the XML at runtime?
# It probably isn't, but it would cleanly solve the issue of validating input here.

createDocumentedTableFromXML <- function(pathToXML) {
  #TODO: Implement this. The below is just to return something of the right type, so we can write tests:
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
  return(tdt)
}
