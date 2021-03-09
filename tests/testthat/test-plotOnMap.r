########################################
# Tests of fillOutMapPlotFrame:

# This function does something pretty simple, so we don't need too many tests. It also gets hit by the
# tests of both plotOnMap and of exampleForestPlot.

test_that("fillOutMapPlotFrame works", {
  # First, check that its validation of input works. We want a data frame with at least two columns,
  # the first of which is of type character:
  expect_error(fillOutMapPlotFrame(c(1,2,3)))
  expect_error(fillOutMapPlotFrame(data.frame(columnOne=c("a","b","c"))))
  expect_error(fillOutMapPlotFrame(data.frame(colOne = c(1,2,3), colTwo=c(4,5,6))))

  # Second, we check that it behaves as expected -- adding rows for missing municipalities
  # and removing rows for non-municipalities:
  expect_identical(fillOutMapPlotFrame(data.frame(colOne=character(0), colTwo=logical(0))),
                   data.frame(colOne=municipalityNames, colTwo=rep(NA_real_, length(municipalityNames))))
  expect_identical(fillOutMapPlotFrame(data.frame(colOne=c("Oslo","London","Berlin"), colTwo=c(1,2,3))),
                   data.frame(colOne=municipalityNames, colTwo=rep(NA_real_, length(municipalityNames))))
})
# Finally, we check that we get the right thing for a mixture of real municipalities and non-municipalities.
# This will change the order in which the municipalities appear in the result away from being alphabetical
# as in municipalityNames, so we do this by snapshot testing. (The snapshots have been inspected to be correct.)
# Unfortunately the snapshot will contain non-ASCII characters (names of municipalities), so this has to be in
# a separate test that's skipped on CI:
test_that("fillOutMapPlotFrame works for mixes of real municipalities and non-municipalities", {
  skip_on_ci()
  expect_snapshot(fillOutMapPlotFrame(data.frame(colOne=c("Vetlanda","Oslo","London","Berlin"), colTwo=c(0,1,2,3))))
})


########################################
# Tests of plotOnMap:

# First we test that the input validation of the plot data, i.e. the checks that dat is either a named vector or a data frame with at least
# two columns the first of which is of type character, works:
test_that("plotOnMap plot data input validation works", {
  # list() is neither atomic nor a data frame:
  expect_error(plotOnMap(dat=list()))
  # c(1,2,3) is atomic but has no names:
  expect_error(plotOnMap(dat=c(1,2,3)))
  # data.frame(col=c(1,2,3)) is a data frame, but it only has one column:
  expect_error(plotOnMap(data.frame(col=c(1,2,3))))
  # data.frame(col=c(1,2), col2=c(3,4)) does have two columns, but the first isn't character:
  expect_error(plotOnMap(dat=data.frame(col=c(1,2), col2=c(3,4))))
  # If we pass a data frame with three columns we expect a warning:
  expect_warning(plotOnMap(data.frame(c1=c("a","b"),c2=c(1,3),c3=c(1,4))))

  # If we pass multiple data points for one municipality, we expect an error:
  inData <- c(2,3)
  names(inData) <- c("Vetlanda","Vetlanda")
  expect_error(plotOnMap(inData))
  expect_error(plotOnMap(data.frame(c1 = c("Vetlanda","Vetlanda"), c2=c(2,3))))
})

# Then, we check that the very similar validation for the tooltips works: (This is just a copy-paste of the above, basically)
test_that("plotOnMap tooltip input validation works", {
  # Here we need to pass some valid plot data in each of our tests:
  plotData <- data.frame(c1 = c("Uppsala", "Lund"), c2 <- c(1,2))

  # c(1,2,3) is neither character nor a data frame:
  expect_error(plotOnMap(dat = plotData, tooltips = c(1,2,3)))
  # c("a","b","c") is character but has no names:
  expect_error(plotOnMap(dat = plotData, tooltips = c("a","b","c")))
  # data.frame(col=c(1,2,3)) is a data frame, but it only has one column:
  expect_error(plotOnMap(dat = plotData,data.frame(col=c(1,2,3))))
  # data.frame(col=c(1,2), col2=c("","")) does have two columns, but the first isn't character:
  expect_error(plotOnMap(dat = plotData, tooltips = data.frame(col=c(1,2), col2=c("",""))))
  # data.frame(col=c("",""), col2=c(1,2)) does have two columns, but the second isn't character:
  expect_error(plotOnMap(dat = plotData, tooltips = data.frame(col=c("",""), col2=c(1,2))))
  # If we pass a data frame with three columns we expect a warning:
  expect_warning(plotOnMap(dat = plotData, tooltips = data.frame(c1=c("a","b"),c2=c("1","3"),c3=c(1,4))))

  # If we pass multiple data points for one municipality, we expect an error:
  inTooltips <- c("a","b")
  names(inTooltips) <- c("Vetlanda","Vetlanda")
  expect_error(plotOnMap(dat = plotData, tooltips = inTooltips))
  expect_error(plotOnMap(dat = plotData, tooltips = data.frame(c1 = c("Vetlanda","Vetlanda"), c2=c("2","3"))))
})

# Then, we check that validation of title parameters works:
test_that("plotOnMap validation of title parameters works",{
  # Here we need to pass some valid plot data in each of our tests:
  plotData <- data.frame(c1 = c("Uppsala", "Lund"), c2 <- c(1,2))

  # 1 is not character:
  expect_error(plotOnMap(dat=plotData, mainTitle=1))
  # c("a","b") is not of length 1:
  expect_error(plotOnMap(dat=plotData, mainTitle=c("a","b")))

  # 1 is not character:
  expect_error(plotOnMap(dat=plotData, subTitle=1))
  # c("a","b") is not of length 1:
  expect_error(plotOnMap(dat=plotData, subTitle=c("a","b")))

  # 1 is not character:
  expect_error(plotOnMap(dat=plotData, legendTitle=1))
  # c("a","b") is not of length 1:
  expect_error(plotOnMap(dat=plotData, legendTitle=c("a","b")))

  # A subtitle won't be shown if the main title is missing, so if so we throw a warning:
  expect_warning(plotOnMap(plotData, subTitle = "Hi"))
})

# Having thoroughly checked that it does give errors everywhere we want it to, we also check
# that a few typical queries we expect to work do not give errors:
test_that("plotOnMap appears to work", {
  plotDataFrame <- data.frame(muns = c("Oslo", "Lund", "Kiruna"), val = c(1,2,3))
  plotNamedVector <- c(1,2,3)
  names(plotNamedVector) <- c("Oslo", "Lund", "Kiruna")

  tooltipsDataFrame <- data.frame(muns = c("Oslo","Lund", "Vetlanda"), ttips = c("a","b","c"))
  tooltipsNamedVector <- c("a","b","c")
  names(tooltipsNamedVector) <- c("Oslo", "Lund", "Kiruna")

  # Every combination of choices of thing to be in a named vector and thing to be in a named list:
  expect_silent(plotOnMap(plotDataFrame, tooltips = tooltipsDataFrame))
  expect_silent(plotOnMap(plotNamedVector, tooltips = tooltipsDataFrame))
  expect_silent(plotOnMap(plotDataFrame, tooltips =  tooltipsNamedVector))
  expect_silent(plotOnMap(plotNamedVector, tooltips = tooltipsNamedVector))

  # Try setting various amounts of title parameters:
  expect_silent(plotOnMap(plotDataFrame, mainTitle=" "))
  expect_silent(plotOnMap(plotDataFrame, mainTitle=" ", subTitle=" "))
  expect_silent(plotOnMap(plotDataFrame, mainTitle=" ", legendTitle=" "))
  expect_silent(plotOnMap(plotDataFrame, mainTitle=" ", subTitle=" ", legendTitle=" "))
  expect_silent(plotOnMap(plotDataFrame, legendTitle=" "))

  # We should also run at least one or two tests of plotting non-numeric data:
  plotDataFrame <- data.frame(muns = c("Oslo", "Lund", "Kiruna"), val = c("1","2","3"))
  expect_silent(plotOnMap(plotDataFrame))
})

########################################
# Tests of exampleForestPlot:

# This is really very simple, since exampleForestPlot takes no arguments at all.
# So we just try to run it and make sure there are no errors. We'd have liked to
# have a snapshot test to also make sure the output isn't changing, but apparently
# the testthat library is unable to save the output of the function to disk (it is
# after all a very complicated object), so we can't have that.

# Note that this actually, in terms of test coverage, also covers a large part
# of plotOnMap, fillOutMapPlotFrame, and even SCB, since it calls all of them:
# As of writing this comment, adding just this one test (but no other tests of any
# of those functions) brings test coverage of plotOnMap.r up to 76.92%, and SCB.r to
# 16.39%.

test_that("exampleForestPlot runs without errors", {
  # In fact we should check not just that there are no errors in it, but also that it
  # can be plotted. Since this takes around 5 seconds on my machine, it's best to wrap
  # this in one single expectation of no errors or warnings:
  expect_silent(girafe(ggobj = exampleForestPlot()))
  # Note that this is also the example of the function (again, the function does only
  # one thing, so it really is *THE* example), so it's actually run twice by R CMD Check,
  # which is what determines if a build is passing, but it is not run by covr so it doesn't
  # show up in code coverage. Thus it has a place as a test as well.
})
