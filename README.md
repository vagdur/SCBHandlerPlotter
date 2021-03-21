<!-- badges: start -->
  [![Codecov test coverage](https://codecov.io/gh/vagdur/SCBHandlerPlotter/branch/main/graph/badge.svg)](https://codecov.io/gh/vagdur/SCBHandlerPlotter?branch=main)
  [![Build Status](https://travis-ci.com/vagdur/SCBHandlerPlotter.svg?branch=main)](https://travis-ci.com/vagdur/SCBHandlerPlotter)
<!-- badges: end -->

# SCBHandlerPlotter
An R library for handling statistical tables from SCB, and creating interactive map plots based on them.

Data from Statistics Sweden (SCB) of course comes in various tables. This repository contains code to provide a single unified function call for fetching data from a collection of these tables. So, for example, to learn how many divorced 24-year-old women there were in Uppsala municipality in 2019, one can run

    SCB(Municipality = "Uppsala", Age=24, Gender="Women", MaritalStatus = "Divorced")
and to learn how large an area of forest there is in Lomma municipality, one runs

    SCB(Municipality = "Lomma", LandUseType = "Total forest")
despite these data points being in different tables.

Additionally, it provides code for creating interactive map plots of data on Swedish municipalities, using the ggiraph package. Try running

    exampleForestPlot()
to see this in action.

## Setup

The library can be installed using

    devtools::install_github("vagdur/SCBHandlerPlotter")
    
## Data sources

All tables except one are from SCB, one is from Skolverket. The map of Sweden is from SCB, via the package [swemaps](https://github.com/reinholdsson/swemaps).
