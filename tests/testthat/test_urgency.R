text <- c("We must implement these measures to limit our carbon emissions now.",
          "We should implement these measures to limit our carbon emissions now.",
          "We must implement these measures to limit our carbon emissions.",
          "We must really implement these measures to limit our carbon emissions.",
          "We must frequently implement these measures to limit our carbon emissions.")
urgency <- get_urgency(text)

test_that("Urgency is generally scored properly", {
  expect_true(urgency$Frequency[5] > 0.5)
  expect_true(urgency$Timing[1] > 0.5)
  expect_true(urgency$Intensity[4] > 0.5)
  expect_true(urgency$Commitment[2] < urgency$Commitment[3])
  expect_true(all(order(urgency$Urgency) == c(3, 5, 2, 4, 1)))
})

test_that("summarise argument in urgency works properly", {
  sum <- get_urgency(c("We really, really, really must do this.",
                       "We should, could, and must do this."))
  mean <- get_urgency(c("We really, really, really must do this.",
                        "We should, could, and must do this."),
                        summarise = "mean")
  max <- get_urgency(c("We really, really, really must do this.",
                        "We should, could, and must do this."),
                      summarise = "min")
  min <- get_urgency(c("We really, really, really must do this.",
                        "We should, could, and must do this."),
                      summarise = "min")
  expect_true(sum$Intensity[1] > mean$Intensity[1])
  expect_true(sum$Commitment[2] > mean$Commitment[2])
  expect_true(sum$Intensity[1] > max$Intensity[1])
  expect_true(mean$Commitment[2] > min$Commitment[2])
})

test_that("Urgency in priorities is scored properly", {
  skip_if_no_spacy()
  urgency2 <- get_urgency(select_priorities(text))
  expect_true(urgency$Commitment[2] < urgency$Commitment[3])
  expect_true(all(order(urgency2$Urgency) == c(3, 5, 2, 4, 1)))
  expect_true(all(urgency2$Urgency == urgency$Urgency))
})

text2 <- c("We must do this",
           "One of the most urgent challenges of our time is climate change .",
           "With our agreement with Mexico that we announced today , let 's generate half the electricity on this continent from clean energy sources within a decade .",
           "Around the world , we 've still got challenges to solve that threaten everybody in the 21st century : old scourges like disease and conflict , but also new challenges , from terrorism and climate change .",
           "We have a climate - change crisis every year that grows more urgent as we look at the challenges that climate change poses for us .")

test_that("Urgency is scored properly in more complex priorities", {
  skip_if_no_spacy()
  urgency3 <- get_urgency(select_priorities(text2))
  expect_true(nrow(urgency3) == 5)
  expect_true(urgency3$Urgency[1] < urgency3$Urgency[2])
})

# test_that("Urgency simulations work", {
#   expect_output(sim_urgency(), "Urgency score: 1")
#   expect_output(sim_urgency(urgency = 0.5), "Urgency score: 0.5")
#   expect_output(sim_urgency(commitment = 1, intensity = 1, timing = 1, frequency = 1),
#                 "Urgency score: 1")
#   expect_output(sim_urgency(commitment = 0.5, intensity = 0.5, timing = 0.5, frequency = 0.5),
#                 "Urgency score: 0.0625")
# })
