# Using SCB does not give unexpected errors or warnings

    Code
      SCB(Municipality = "Uppsala", Age = 24, Gender = "Women", MaritalStatus = "Divorced")
    Output
      [1] 8

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

