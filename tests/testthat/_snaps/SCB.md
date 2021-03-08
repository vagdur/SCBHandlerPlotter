# Using SCB does not give unexpected errors or warnings

    Code
      SCB(Municipality = "Uppsala", Age = 24, Gender = "Women", MaritalStatus = "Divorced")
    Output
      [1] 14

---

    Code
      SCB(Municipality = "Lomma", LandUseType = "Total forest")
    Output
      [1] 204

---

    Code
      SCB(Municipality = "Lund", MunicipalOrRegionalTax = "Municipal")
    Output
      [1] 21.24

---

    Code
      SCB(Municipality = "Lund", HighSchoolEligibility = "True")
    Output
      [1] 641

# Parameter forceTable works as expected

    Code
      SCB(forceTable = "AgeGender", Gender = "Women")
    Output
      [1] 5131775

---

    Code
      SCB(forceTable = "LandUse", LandUseType = "Total area")
    Output
      [1] 40731056

# Collision handling mode consensus works as expected

    Code
      SCB(collisionHandlingMode = "consensus", Gender = "Women", Age = 20, verbose = FALSE)
    Output
      [1] 52239

---

    Code
      SCB(collisionHandlingMode = "consensus", Gender = "Women", Age = 20, verbose = TRUE)
    Message <simpleMessage>
      Multiple tables are able to handle your request: AgeGender, BirthRegion, MaritalStatus.
      
                         Ran your query against all these tables, and they agreed on the result, so we are returning that result. (This may have significantly slowed your query down.)
      
                         You can get around this behaviour either by using forceTable to specify which of these tables to query, or by choosing a
                         different collisionHandlingMode than "consensus".
    Output
      [1] 52239

