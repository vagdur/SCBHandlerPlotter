# This function supplies devtools::release with some questions to ask before the package can be
# released to CRAN. Currently the package isn't on CRAN, so this is just futureproofing.

# Since this function literally just returns a character vector, there's no point in testing
# it, and so we exempt it from code coverage:
release_questions <- function() { # nocov start
  c(
    "Have you run writeTextDocumentation to update table documentation?",
    "Have you added the @seealso outputted by writeTextDocumentation to the SCB() Roxygen block?"
  )
} # nocov end
