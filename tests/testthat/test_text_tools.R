sentences <- extract_context(match = "war",
                             v = US_News_Conferences_1960_1980$text[100],
                             level = "sentences", n = 1)
words <- extract_context(match = "warning",
                         v = US_News_Conferences_1960_1980$text[100],
                         level = "words", n = 3)

test_that("Sentences before and after are extracted correctly", {
  expect_length(sentences, 1)
  expect_true(is.list(sentences))
  expect_equal(sentences[[1]][9],
               " President, you met this week with the leaders of the Appropriations Committee partly in regard to the defense budget. And later, Senator McClellan said he would favor slashing $3 billion from that budget, which as you know is nearly $90 billion, higher than in wartime. Could you tell us if you think that is a dangerous cut, and if so, why?,THE PRESIDENT.")
})

test_that("Words before and after are extracted correctly", {
  expect_length(words, 1)
  expect_true(is.list(words))
  expect_equal(words [[1]][1], "some economists are warning that consumers are")
})

text <- c("This function was created on the 2021 September 9. I am Henrique Sposito",
          "Today is 12, October, 2021",
          "This is the first sentence. This is the second sentence.",
          "This is the United States",
          "This is Sao Paulo")

test_that("Speakers are extracted properly", {
  skip_on_ci()
  skip_on_cran()
  expect_message(extract_names(c("this is a test for Brazil", "this is also a test")),
                 "No names found in text.")
  expect_equal(extract_names(text)[["names"]], "Henrique Sposito")
})

test_that("Titles are extracted properly", {
  expect_equal(extract_title(text)[3], "This is the first sentence.")
})

test_that("Locations are extracted properly", {
  skip_on_ci()
  skip_on_cran()
  expect_equal(extract_locations(text)[["names"]], c("Sao Paulo", "the unite state"))
})

test_that("Text is properly split", {
  a <- split_text(text)
  expect_length(a, 5)
})

test_that("Text matches are properly returned", {
  b <- extract_match(text, "Sao Paulo")
  expect_equal(b[[1]], character(0))
  expect_equal(b[[5]], "This is Sao Paulo")
})
