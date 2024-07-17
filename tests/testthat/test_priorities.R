text <- c("We must do this.", "I do not want to do this.",
          "You are going to do this.", "They should not do this.")

test_that("Topics are extracted properly", {
  skip_on_ci()
  skip_on_cran()
  expect_equal(select_priorities(text)$priorities,
               c("We must do this .", "You are going to do this ."))
})
