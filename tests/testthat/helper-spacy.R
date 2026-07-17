# Skip tests that require a working spaCy (Python) installation via spacyr.
# This is more robust than skip_on_ci()/skip_on_cran(): tests run wherever
# spaCy and the en_core_web_sm model are actually available, and skip cleanly
# wherever they are not. Initialisation is attempted directly (rather than only
# checking for the Python module) so the model, not just the module, is tested.
skip_if_no_spacy <- function() {
  testthat::skip_if_not_installed("spacyr")
  available <- tryCatch({
    suppressMessages(suppressWarnings(
      spacyr::spacy_initialize(model = "en_core_web_sm")
    ))
    TRUE
  }, error = function(e) FALSE)
  testthat::skip_if_not(available, "spaCy (Python) is not available")
}
