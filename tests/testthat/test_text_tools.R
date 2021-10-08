text <- c("This function was created on the 29 September 2021",
          "Today is October 12, 2021",
          "This is the first sentence. This is the second sentence.",
          "This is the U.S.",
          "This is Sao Paulo")

test_that("Dates are extracted properly", {
  expect_equal(extract_date(text), c("29-9-2021", "12-10-2021", NA, NA, NA))
})

test_that("Titles are extracted properly", {
  expect_equal(extract_title(text)[3], c("This is the first sentence."))
})

test_that("Locations are extracted properly", {
  expect_equal(extract_location(text), c(NA, NA, NA, "United States of America", "Sao Paulo"))
})
