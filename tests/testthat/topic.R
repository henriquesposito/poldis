text <- c("military, guns, war, peace",
          "economy, development, culture, clean water",
          "military, development")

test_that("Topics are extracted properly", {
  skip_on_ci()
  skip_on_cran()
  expect_equal(data.frame(gather_topics(text, dictionary = c("military", "development"))),
               data.frame("topics" = c("military", "development", "military, development")))
  expect_equal(data.frame(gather_topics(text, dictionary = list("military" = c("guns", "war"),
                                                                "development" = c("economy", "banks")))),
               data.frame("topics" = c("military", "development", "")))
})
