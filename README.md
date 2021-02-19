# SCBHandlerPlotter
An R library for handling statistical tables from SCB, and creating interactive map plots based on them.

Data from Statistics Sweden (SCB) of course comes in various tables. This repository contains code to provide a single unified function call for fetching data from a collection of these tables. So, for example, to learn how many divorced 24-year-old women there were in Värmdö municipality in 2019, one can run 
      SCB(Municipality = "Värmdö", Age=24, Gender="kvinnor", MaritalStatus = "skilda")
and to learn how large an area of forest there is in Värmdö municipality, one runs
      SCB(Municipality = "Värmdö", LandUseClass = "total skogsmark")
despite these data points being in different tables.

Additionally, it provides code for creating interactive map plots of data on Swedish municipalities, using the ggiraph package.
