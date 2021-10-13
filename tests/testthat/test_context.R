text <- US_News_Conferences$text[500]

sentences <- context(string = "war|weapons of mass destruction|conflict|NATO|peace",
                     var = text, level = "sentences")

words <- context(string = "war|weapons of mass destruction|conflict|NATO|peace",
                 var = text, level = "words")

test_that("Sentences before and after are extracted correctly", {
  expect_length(sentences, 1)
  expect_true(is.list(sentences))
  expect_equal(sentences[[1]][24],
               ",But you have to understand that those are the criteria for me. I've told you before, I don't believe we need to refight the Gulf war. It's history.")
})

test_that("Words before and after are extracted correctly", {
  expect_length(words, 1)
  expect_true(is.list(words))
  expect_equal(words [[1]][2], "peace in a world")
})

# test_that("Paragraph warnings appear", {
#   expect_error(context(string = "war|weapons of mass destruction|conflict|NATO|peace",
#                                     var = text, level = "paragraph"),
#                "No paragraph were found in text, please set level to sentences or words")
#
# })
