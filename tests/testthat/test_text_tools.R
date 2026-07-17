sentences <- extract_context(match = " war ",
                             v = US_inaugural_addresses_1993_2025$text[1],
                             level = "sentences", n = 1)
words <- extract_context(match = "war",
                         v = US_inaugural_addresses_1993_2025$text[1],
                         level = "words", n = 3)

test_that("Sentences before and after are extracted correctly", {
  expect_length(sentences, 1)
  expect_true(is.list(sentences))
})

test_that("Words before and after are extracted correctly", {
  expect_length(words, 1)
  expect_true(is.list(words))
  expect_equal(words [[1]][1], "of the cold war assumes new responsibilities")
})

text <- c("This function was created on the 2021 September 9. I am Henrique Sposito",
          "Today is 12, October, 2021",
          "This is the first sentence. This is the second sentence.",
          "This is the United States",
          "This is Sao Paulo")

test_that("Speakers are extracted properly", {
  skip_if_no_spacy()
  expect_message(extract_names(c("this is a test for Brazil", "this is also a test")),
                 "No names found in text.")
  expect_equal(extract_names(text)[["names"]], "Henrique Sposito")
})

test_that("Titles are extracted properly", {
  expect_equal(extract_first_sentence(text)[3], "This is the first sentence.")
})

test_that("Locations are extracted properly", {
  skip_if_no_spacy()
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
