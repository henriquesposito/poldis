# Skip tests that require a working spaCy (Python) installation via spacyr.
# This is more robust than skip_on_ci()/skip_on_cran(): tests run wherever
# spaCy is actually available and skip cleanly wherever it is not.
skip_if_no_spacy <- function() {
  testthat::skip_if_not_installed("spacyr")
  testthat::skip_if_not(
    reticulate::py_module_available("spacy"),
    "spaCy (Python) is not available"
  )
}
