# # Test Analysis Learning Curve
# # References: https://code.datasciencedojo.com/datasciencedojo/tutorials/tree/master/Introduction%20to%20Text%20Analytics%20with%20R
#
# library(tidyverse)
# library(lubridate)
#
# load("~/GitHub/Poldis/data-raw/US_Weekly_Radio.rda")
# radio <- US_Weekly_Radio %>% dplyr::select(-source_links)
# radio <- subset(radio, speaker != "Laura Bush")
# radio <- subset(radio, speaker != "Michelle Obama")
# radio$speaker <- as.factor(radio$speaker)
# radio$date <- lubridate::mdy(radio$date)
# summary(radio)
# radio$TextLength <- nchar(radio$text)
# summary(radio$TextLength)
#
# library(ggplot2)
#
# ggplot(radio, aes(x = TextLength, fill = speaker)) +
#   theme_bw() +
#   geom_histogram(binwidth = 10) +
#   labs(y = "Text Count", x = "Length of Text",
#        title = "Distribution of Text Lengths by speaker")
#
# # Stratified split 70 - 30 %
#
# library(caret)
#
# set.seed(10000)
# indexes <- createDataPartition(radio$speaker, times = 1,
#                                p = 0.7, list = FALSE)
#
# train <- radio[indexes,]
# test <- radio[-indexes,]
#
# prop.table(table(train$speaker))
# prop.table(table(test$speaker))
#
# # Tokenization
#
# library(quanteda)
#
# train.tokens <- quanteda::tokens(train$text, what = "word",
#                        remove_numbers = TRUE, remove_punct = TRUE,
#                        remove_symbols = TRUE, split_hyphens = TRUE)
#
# train.tokens[[357]]
#
# # Lower case the tokens
# train.tokens <- quanteda::tokens_tolower(train.tokens)
# train.tokens[[357]]
# quanteda::stopwords()
# # Should stop words be removed????
#
# # Use quanteda's built-in stopword list for English
# train.tokens <- quanteda::tokens_select(train.tokens, quanteda::stopwords(),
#                               selection = "remove")
# train.tokens[[357]]
#
# # Perform stemming on the tokens
# train.tokens <- quanteda::tokens_wordstem(train.tokens, language = "english")
# train.tokens[[357]]
#
# # Create our first bag-of-words model
# train.tokens.dfm <- quanteda::dfm(train.tokens, tolower = FALSE)
#
# # Transform to a matrix and inspect
# train.tokens.matrix <- as.matrix(train.tokens.dfm)
# View(train.tokens.matrix[1:20, 1:100])
# dim(train.tokens.matrix)
# colnames(train.tokens.matrix)[1:50]
#
# # Setup a the feature data frame with labels
# train.tokens.df <- cbind(Label = train$speaker, data.frame(train.tokens.dfm))
#
# # Cleanup column names
# names(train.tokens.df) <- make.names(names(train.tokens.df))
#
# # Use caret to create stratified folds for 10-fold cross validation repeated
# # 3 times (i.e., create 30 random stratified samples)
# set.seed(20000)
# cv.folds <- caret::createMultiFolds(train$speaker, k = 10, times = 3)
#
# cv.cntrl <- caret::trainControl(method = "repeatedcv", number = 10,
#                          repeats = 3, index = cv.folds)
#
# #dealing with large datasets more efficient
# library(doSNOW)
#
# # Time the code execution
# start.time <- Sys.time()
#
# # Create a cluster to work on 3 logical cores
# cl <- makeCluster(3, type = "SOCK")
# registerDoSNOW(cl)
#
# rpart.cv.1 <- caret::train(Label ~ ., data = train.tokens.df, method = "rpart",
#                     trControl = cv.cntrl, tuneLength = 7)
#
# # Processing is done, stop cluster
# stopCluster(cl)
#
# # Total time of execution
# total.time <- Sys.time() - start.time
# total.time
#
# rpart.cv.1
#
# # Our function for calculating relative term frequency (TF)
# term.frequency <- function(row) {
#   row / sum(row)
# }
#
# # Our function for calculating inverse document frequency (IDF)
# inverse.doc.freq <- function(col) {
#   corpus.size <- length(col)
#   doc.count <- length(which(col > 0))
#
#   log10(corpus.size / doc.count)
# }
#
# # Our function for calculating TF-IDF.
# tf.idf <- function(x, idf) {
#   x * idf
# }
#
# # First step, normalize all documents via TF.
# train.tokens.df <- apply(train.tokens.matrix, 1, term.frequency)
# dim(train.tokens.df)
# View(train.tokens.df[1:20, 1:100])
#
# # Second step, calculate the IDF vector that we will use - both
# # for training data and for test data!
# train.tokens.idf <- apply(train.tokens.matrix, 2, inverse.doc.freq)
# str(train.tokens.idf)
#
# # Lastly, calculate TF-IDF for our training corpus.
# train.tokens.tfidf <-  apply(train.tokens.df, 2, tf.idf, idf = train.tokens.idf)
# dim(train.tokens.tfidf)
# View(train.tokens.tfidf[1:25, 1:25])
#
# # Transpose the matrix
# train.tokens.tfidf <- t(train.tokens.tfidf)
# dim(train.tokens.tfidf)
# View(train.tokens.tfidf[1:25, 1:25])
#
# # Make a clean data frame using the same process as before.
# train.tokens.tfidf.df <- cbind(Label = train$speaker, data.frame(train.tokens.tfidf))
# names(train.tokens.tfidf.df) <- make.names(names(train.tokens.tfidf.df))
#
# # Time the code execution
# start.time <- Sys.time()
#
# # Create a cluster to work on 3 logical cores.
# cl <- makeCluster(3, type = "SOCK")
# registerDoSNOW(cl)
#
# # As our data is non-trivial in size at this point, use a single decision
# # tree alogrithm as our first model. We will graduate to using more
# # powerful algorithms later when we perform feature extraction to shrink
# # the size of our data.
# rpart.cv.2 <- train(Label ~ ., data = train.tokens.tfidf.df, method = "rpart",
#                     trControl = cv.cntrl, tuneLength = 7)
#
# # Processing is done, stop cluster.
# stopCluster(cl)
#
# # Total time of execution on workstation was
# total.time <- Sys.time() - start.time
# total.time
#
# # Check out our results.
# rpart.cv.2
#
# # N-grams allow us to augment our document-term frequency matrices with
# # word ordering. This often leads to increased performance (e.g., accuracy)
# # for machine learning models trained with more than just unigrams (i.e.,
# # single terms). Let's add bigrams to our training data and the TF-IDF
# # transform the expanded featre matrix to see if accuracy improves.
#
# # Add bigrams to our feature matrix.
# train.tokens <- quanteda::tokens_ngrams(train.tokens, n = 1:2)
# train.tokens[[357]]
#
# # Transform to dfm and then a matrix.
# train.tokens.dfm <- dfm(train.tokens, tolower = FALSE)
# train.tokens.matrix <- as.matrix(train.tokens.dfm)
# train.tokens.dfm
#
# # Normalize all documents via TF.
# train.tokens.df <- apply(train.tokens.matrix, 1, term.frequency)
#
# # Calculate the IDF vector that we will use for training and test data!
# train.tokens.idf <- apply(train.tokens.matrix, 2, inverse.doc.freq)
#
# # Calculate TF-IDF for our training corpus
# train.tokens.tfidf <-  apply(train.tokens.df, 2, tf.idf,
#                              idf = train.tokens.idf)
#
# # Transpose the matrix
# train.tokens.tfidf <- t(train.tokens.tfidf)
#
# # Fix incomplete cases
# incomplete.cases <- which(!complete.cases(train.tokens.tfidf))
# train.tokens.tfidf[incomplete.cases,] <- rep(0.0, ncol(train.tokens.tfidf))
#
# # Make a clean data frame.
# train.tokens.tfidf.df <- cbind(Label = train$speaker, data.frame(train.tokens.tfidf))
# names(train.tokens.tfidf.df) <- make.names(names(train.tokens.tfidf.df))
#
# # Clean up unused objects in memory.
# gc()
#
# library(irlba)
#
# # Time the code execution
# start.time <- Sys.time()
#
# # Perform SVD. Specifically, reduce dimensionality down to 300 columns
# # for our latent semantic analysis (LSA).
# train.irlba <- irlba(t(train.tokens.tfidf), nv = 300, maxit = 600)
#
# # Total time of execution on workstation was
# total.time <- Sys.time() - start.time
# total.time
#
# # Take a look at the new feature data up close.
# View(train.irlba$v)
#
# # As with TF-IDF, we will need to project new data (e.g., the test data)
# # into the SVD semantic space. The following code illustrates how to do
# # this using a row of the training data that has already been transformed
# # by TF-IDF, per the mathematics illustrated in the slides.
# sigma.inverse <- 1 / train.irlba$d
# u.transpose <- t(train.irlba$u)
# document <- train.tokens.tfidf[1,]
# document.hat <- sigma.inverse * u.transpose %*% document
#
# # Look at the first 10 components of projected document and the corresponding
# # row in our document semantic space (i.e., the V matrix)
# document.hat[1:10]
# train.irlba$v[1, 1:10]
#
# # Create new feature data frame using our document semantic space of 300
# # features (i.e., the V matrix from our SVD).
# #
# train.svd <- data.frame(Label = train$speaker, train.irlba$v)
#
# # Create a cluster to work on 3 logical cores.
# cl <- makeCluster(3, type = "SOCK")
# registerDoSNOW(cl)
#
# # Time the code execution
# start.time <- Sys.time()
#
# # This will be the last run using single decision trees. With a much smaller
# # feature matrix we can now use more powerful methods like the mighty Random
# # Forest from now on!
# rpart.cv.4 <- train(Label ~ ., data = train.svd, method = "rpart",
#                     trControl = cv.cntrl, tuneLength = 7)
#
# # Processing is done, stop cluster.
# stopCluster(cl)
#
# # Total time of execution on workstation was
# total.time <- Sys.time() - start.time
# total.time
#
# # Check out our results.
# rpart.cv.4
#
# # NOTE - The following code takes a long time to run. Here's the math.
# #        We are performing 10-fold CV repeated 3 times. That means we
# #        need to build 30 models. We are also asking caret to try 7
# #        different values of the mtry parameter. Next up by default
# #        a mighty random forest leverages 500 trees. Lastly, caret will
# #        build 1 final model at the end of the process with the best
# #        mtry value over all the training data. Here's the number of
# #        tree we're building:
# #
# #             (10 * 3 * 7 * 500) + 500 = 105,500 trees!
# # Create a cluster to work on 10 logical cores.
# cl <- makeCluster(3, type = "SOCK")
# registerDoSNOW(cl)
# # Time the code execution
# start.time <- Sys.time()
# # We have reduced the dimensionality of our data using SVD. Also, the
# # application of SVD allows us to use LSA to simultaneously increase the
# # information density of each feature. To prove this out, leverage a
# # mighty Random Forest with the default of 500 trees. We'll also ask
# # caret to try 7 different values of mtry to find the mtry value that
# # gives the best result!
# rf.cv.1 <- train(Label ~ ., data = train.svd, method = "rf",
#                 trControl = cv.cntrl, tuneLength = 7)
# # Processing is done, stop cluster.
# stopCluster(cl)
# total.time <- Sys.time() - start.time
# total.time
#
# # Check out our results.
# rf.cv.1
#
# # Let's drill-down on the results.
# confusionMatrix(train.svd$Label, rf.cv.1$finalModel$predicted)
#
# # OK, now let's add in the feature we engineered previously for SMS
# # text length to see if it improves things.
# train.svd$TextLength <- train$TextLength
#
# cl <- makeCluster(3, type = "SOCK")
# registerDoSNOW(cl)
#
# # Time the code execution
# start.time <- Sys.time()
#
# # Re-run the training process with the additional feature.
# rf.cv.2 <- train(Label ~ ., data = train.svd, method = "rf",
#                  trControl = cv.cntrl, tuneLength = 7,
#                  importance = TRUE)
#
# stopCluster(cl)
#
# # Total time of execution on workstation was
# total.time <- Sys.time() - start.time
# total.time
#
# # Check the results.
# rf.cv.2
#
# # Drill-down on the results.
# confusionMatrix(train.svd$Label, rf.cv.2$finalModel$predicted)
#
# library(randomForest)
# varImpPlot(rf.cv.1$finalModel)
# varImpPlot(rf.cv.2$finalModel)
#
# # Turns out that our TextLength feature is somewhat predictive.
# library(lsa)
#
# # calculate similarity scores
# train.similarities <- lsa::cosine(t(as.matrix(train.svd[, -c(1, ncol(train.svd))])))
#
# spam.indexes <- which(train$speaker == "Barack Obama")
#
# train.svd$SpamSimilarity <- rep(0.0, nrow(train.svd))
# for(i in 1:nrow(train.svd)) {
#   train.svd$SpamSimilarity[i] <- mean(train.similarities[i, spam.indexes])
# }
#
# # As always, let's visualize our results using the mighty ggplot2
# ggplot(train.svd, aes(x = SpamSimilarity, fill = Label)) +
#   theme_bw() +
#   geom_histogram(binwidth = 0.05) +
#   labs(y = "Radio talk Count",
#        x = "Mean Obama Message Cosine Similarity",
#        title = "Distribution of Speakers Using Obama's Cosine Similarity")
#
# # Create a cluster to work on 10 logical cores.
# cl <- makeCluster(3, type = "SOCK")
# registerDoSNOW(cl)
#
# # Time the code execution
# start.time <- Sys.time()
#
# # Re-run the training process with the additional feature.
# set.seed(50000)
# rf.cv.3 <- train(Label ~ ., data = train.svd, method = "rf",
#                  trControl = cv.cntrl, tuneLength = 7,
#                  importance = TRUE)
#
# # Processing is done, stop cluster.
# stopCluster(cl)
# total.time <- Sys.time() - start.time
# total.time
#
# rf.cv.3
#
# # Drill-down on the results.
# confusionMatrix(train.svd$Label, rf.cv.3$finalModel$predicted)
#
# # How important was this feature?
# library(randomForest)
# varImpPlot(rf.cv.3$finalModel)
#
# ### We've built what appears to be an more or less predictive model
#
# # Tokenization.
# test.tokens <- tokens(test$text, what = "word",
#                       remove_numbers = TRUE, remove_punct = TRUE,
#                       remove_symbols = TRUE, remove_hyphens = TRUE)
#
# # Lower case the tokens.
# test.tokens <- tokens_tolower(test.tokens)
#
# # Stopword removal.
# test.tokens <- tokens_select(test.tokens, stopwords(),
#                              selection = "remove")
#
# # Stemming.
# test.tokens <- tokens_wordstem(test.tokens, language = "english")
#
# # Add bigrams.
# test.tokens <- tokens_ngrams(test.tokens, n = 1:2)
#
# # Convert n-grams to quanteda document-term frequency matrix.
# test.tokens.dfm <- dfm(test.tokens, tolower = FALSE)
#
# # Explore the train and test quanteda dfm objects.
# train.tokens.dfm
# test.tokens.dfm
#
# test.tokens.dfm <- dfm_select(test.tokens.dfm, pattern = train.tokens.dfm,
#                               selection = "keep")
# test.tokens.matrix <- as.matrix(test.tokens.dfm)
# test.tokens.dfm
#
# # Normalize all documents via TF.
# test.tokens.df <- apply(test.tokens.matrix, 1, term.frequency)
# str(test.tokens.df)
#
# # Lastly, calculate TF-IDF for our training corpus.
# test.tokens.tfidf <-  apply(test.tokens.df, 2, tf.idf, idf = train.tokens.idf)
# dim(test.tokens.tfidf)
# View(test.tokens.tfidf[1:25, 1:25])
#
# # Transpose the matrix
# test.tokens.tfidf <- t(test.tokens.tfidf)
#
# # Fix incomplete cases
# summary(test.tokens.tfidf[1,])
# test.tokens.tfidf[is.na(test.tokens.tfidf)] <- 0.0
# summary(test.tokens.tfidf[1,])
#
# test.svd.raw <- t(sigma.inverse * u.transpose %*% t(test.tokens.tfidf))
#
# # Lastly, we can now build the test data frame to feed into our trained
# # machine learning model for predictions. First up, add Label and TextLength.
# test.svd <- data.frame(Label = test$Label, test.svd.raw,
#                        TextLength = test$TextLength)
#
# # create a spam similarity matrix.
# test.similarities <- rbind(test.svd.raw, train.irlba$v[spam.indexes,])
# test.similarities <- cosine(t(test.similarities))
#
# test.svd$SpamSimilarity <- rep(0.0, nrow(test.svd))
# spam.cols <- (nrow(test.svd) + 1):ncol(test.similarities)
# for(i in 1:nrow(test.svd)) {
#   test.svd$SpamSimilarity[i] <- mean(test.similarities[i, spam.cols])
# }
#
# test.svd$SpamSimilarity[!is.finite(test.svd$SpamSimilarity)] <- 0
#
# # Now we can make predictions on the test data set using our trained mighty
# # random forest.
# preds <- predict(rf.cv.3, test.svd)
#
# # Drill-in on results
# confusionMatrix(preds, test.svd$Label)
#
# # The definition of overfitting is doing far better on the training data as
# # evidenced by CV than doing on a hold-out dataset (i.e., our test dataset).
# # One potential explantion of this overfitting is the use of the spam similarity
# # feature. The hypothesis here is that spam features (i.e., text content) varies
# # highly, espeically over time. As such, our average spam cosine similarity
# # is likely to overfit to the training data. To combat this, let's rebuild a
# # mighty random forest without the spam similarity feature.
# train.svd$SpamSimilarity <- NULL
# test.svd$SpamSimilarity <- NULL
#
# # Create a cluster to work on 10 logical cores.
# # cl <- makeCluster(3, type = "SOCK")
# # registerDoSNOW(cl)
#
# # Time the code execution
# # start.time <- Sys.time()
#
# # Re-run the training process with the additional feature.
# # set.seed(254812)
# # rf.cv.4 <- train(Label ~ ., data = train.svd, method = "rf",
# #                  trControl = cv.cntrl, tuneLength = 7,
# #                  importance = TRUE)
#
# # Processing is done, stop cluster.
# # stopCluster(cl)
#
# # Total time of execution on workstation was
# # total.time <- Sys.time() - start.time
# # total.time
#
# rf.cv.4
#
# # Make predictions and drill-in on the results
# preds <- predict(rf.cv.4, test.svd)
# confusionMatrix(preds, test.svd$Label)
#
