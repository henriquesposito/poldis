text <- c("We must implement these measures to limit our carbon emissions now.",
          "We should implement these measures to limit our carbon emissions now.",
          "We must implement these measures to limit our carbon emissions.",
          "We must vigorously implement these measures to limit our carbon emissions.",
          "We must persistently implement these measures to limit our carbon emissions.")
urgency <- get_urgency(text)

test_that("Urgency is scored properly", {
  expect_true(urgency$frequency[3] > 0.01)
  expect_true(urgency$timing[1] > 0.01)
  expect_true(urgency$intensity[4] > 0.01)
  expect_true(urgency$commitment[1] > 0.01)
  expect_equal(urgency$text, c("We must implement these measures to limit our carbon emissions now.",
                               "We should implement these measures to limit our carbon emissions now.",
                               "We must persistently implement these measures to limit our carbon emissions.",
                               "We must vigorously implement these measures to limit our carbon emissions.",
                               "We must implement these measures to limit our carbon emissions."))
})
