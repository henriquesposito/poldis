text <- c("We must implement these measures to limit our carbon emissions now.",
          "We should implement these measures to limit our carbon emissions now.",
          "We must implement these measures to limit our carbon emissions.",
          "We must vigorously implement these measures to limit our carbon emissions.",
          "We must persistently implement these measures to limit our carbon emissions.")
urgency <- get_urgency(text)

test_that("Urgency is scored properly", {
  expect_true(urgency$Frequency[5] > 1)
  expect_true(urgency$Timing[1] > 1)
  expect_true(urgency$Intensity[4] > 1)
  expect_true(urgency$Commitment[2] < urgency$Commitment[3])
  expect_true(all(order(urgency$Urgency) == c(3, 2, 4, 5, 1)))
})
