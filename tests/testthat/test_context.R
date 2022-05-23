text <- US_News_Conferences_1960_1980$text[100]
sentences <- context(match = "war", textvar = text, level = "sentences", n = 1)
words <- context(match = "warning", textvar = text, level = "words", n = 3)
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
