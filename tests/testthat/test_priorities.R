text <- c("We must do this.",
          "I do not want to do this.",
          "You are going to do this.",
          "They should not do this.",
          "Until Obama took him off the trash heap , he could n't do anything.",
          "We 're gon na have a beautiful clean air .",
          "One of the most urgent challenges of our time is climate change .",
          "With our agreement with Mexico that we announced today , let 's generate half the electricity on this continent from clean energy sources within a decade .",
          "Around the world , we 've still got challenges to solve that threaten everybody in the 21st century : old scourges like disease and conflict , but also new challenges , from terrorism and climate change .",
          "We have a climate - change crisis every year that grows more urgent as we look at the challenges that climate change poses for us .")

test_that("Topics are extracted properly", {
  skip_on_ci()
  skip_on_cran()
  expect_equal(select_priorities(text)$priorities,
               c("We must do this .",
                 "You are going to do this .",
                 "We 're gon na have a beautiful clean air .",
                 "With our agreement with Mexico that we announced today , let 's generate half the electricity on this continent from clean energy sources within a decade ."))
})
