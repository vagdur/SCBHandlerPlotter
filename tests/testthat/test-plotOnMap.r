########################################
# Tests of fillOutMapPlotFrame:

########################################
# Tests of plotOnMap:

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
