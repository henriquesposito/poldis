text <- c("military, guns, war, peace",
          "economy, development, culture, clean water",
          "military, development")

test_that("Topics are extracted properly", {
  skip_on_ci()
  skip_on_cran()
  expect_equal(as.character(unname(unlist(gather_topics(text, dictionary = c("military", "development"))))),
               c("military", "development", "military, development"))
  expect_equal(as.character(unname(unlist(gather_topics(text, dictionary = list("military" = c("guns", "war"),
                                                                "development" = c("economy", "banks")))))),
               c("military", "development", ""))
})
