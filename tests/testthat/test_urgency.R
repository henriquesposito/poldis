text <- c("We must implement these measures to limit our carbon emissions now.",
          "We should implement these measures to limit our carbon emissions now.",
          "We must implement these measures to limit our carbon emissions.",
          "We must vigorously implement these measures to limit our carbon emissions.",
          "We must persistently implement these measures to limit our carbon emissions.")
urgency <- get_urgency(text)

test_that("Urgency is scored properly", {
  expect_true(urgency$Frequency[5] > 0.01)
  expect_true(urgency$Timing[1] > 0.01)
  expect_true(urgency$Intensity[4] > 1)
  expect_true(all(urgency$Commitment == c(1, 0.5, 1, 1, 1)))
})
