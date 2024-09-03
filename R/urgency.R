#' Urgency Analysis
#'
#' @param .data A data frame, priorities data frame coded using
#' `select_priorities()`, or text vector.
#' For data frames, function will search for "text" variable.
#' For priorities data frame function will search for "priorities" variable.
#' If missing, opens the webpage containing the urgency codebook.
#' @param summarise How to handle multiple matches for the same dimension
#' in the same text observation?
#' By default, multiple matches are added together and
#' their "sum" per text observation is returned.
#' Users can, instead, choose the "mean" which returns the average
#' score per dimension per text observation when there are multiple matches.
#' The "mean" can also be used as a form of normalization per
#' dimension and text observation in certain cases.
#' @details
#' Urgency in political discourses is an expression of how necessary and/or
#' how soon an action should be undertaken or completed.
#' This is measured along four dimensions,
#' two related to necessity and two related to timing.
#' The first two dimensions, degree of intensity and degree of commitment,
#' relate to the necessity of taking the action, while the next two dimensions,
#' frequency of action and timing of action,
#' relate to the timing in which action is taken.
#' Our dictionary includes terms in each of these dimensions.
#' The terms included in each of these dimensions were validated and adjusted
#' through an online survey that took place between July and August of 2024.
#' The survey results were recorded as counts of the number of participants
#' who selected an urgency-related word as more urgent than its pair.
#' To analyze the survey results, we employed Bradley-Terry models for
#' paired comparisons.
#' A rank of the words for each dimension of urgency was obtained from the analysis,
#' which were then used to create the urgency word scores in the dictionaries.
#' For more information on the dimensions, scores, or the survey on urgency,
#' please run `get_urgency()` to access the urgency codebook.
#' For priorities (i.e. coded using the `select_priorities()`),
#' urgency scores are calculated by multiplying the commitment scores by all
#' other dimensions.
#' This is done because commitment words are indicative of political priorities,
#' For more information please refer to the `select_priorities()` function.
#' For vectors or data frames urgency scores are calculated by
#' adding commitment and intensity dimension scores (i.e. how necessary)
#' and multiplying these by the sum of timing and frequency dimension
#' scores (i.e. how soon).
#' In both cases, zero urgency scores are indicative of no urgency but maximum
#' scores can vary.
#' @return A scored data frame for each dimension of urgency.
#' @import dplyr
#' @importFrom textstem lemmatize_strings
#' @examples
#' \donttest{
#' get_urgency(US_News_Conferences_1960_1980[1:10, 3])
#' get_urgency(US_News_Conferences_1960_1980[1:10,])
#' #get_urgency(select_priorities(US_News_Conferences_1960_1980[1:2, 3]))
#' #summary(get_urgency(US_News_Conferences_1960_1980[1:10, 3]))
#' #plot(get_urgency(US_News_Conferences_1960_1980[1:10, 3]))
#' #get_urgency()
#' }
#' @export
get_urgency <- function(.data, summarise = "sum") {
  Frequency <- Timing <- Commitment <- Intensity <- Urgency <- text_clean <- NULL
  # tries to open urgency codebook if no argument is declared
  if (missing(.data)) open_codebook(codebook = "urgency")
  # get text variable
  if (inherits(.data, "priorities")) {
    text_clean <- getElement(.data, "lemmas")
    priorities <- TRUE
  } else if (inherits(.data, "data.frame")) {
    if ("lemmas" %in% names(.data)) text_clean <- getElement(.data, "lemmas") else
      text_clean <- textstem::lemmatize_strings(getElement(.data, "text"))
    priorities <- FALSE
  } else {
    text_clean <- textstem::lemmatize_strings(.data)
    priorities <- FALSE
  }
  # assign urgency dimensions
  out <- data.frame("text_clean" = text_clean) %>%
    dplyr::mutate(Frequency = .assign_urgency_dimensions(text_clean, dimension = "frequency",
                                                         summarise = summarise, priority = priorities),
                  Timing = .assign_urgency_dimensions(text_clean, dimension = "timing",
                                                      summarise = summarise, priority = priorities),
                  Intensity = .assign_urgency_dimensions(text_clean, dimension = "intensity",
                                                         summarise = summarise, priority = priorities),
                  Commitment = .assign_urgency_dimensions(text_clean, dimension = "commitment",
                                                          summarise = summarise, priority = priorities))
  # calculate urgency scores
  if (isTRUE(priorities)) {
    out <- out %>%
      dplyr::mutate(Urgency = Commitment * ifelse(Intensity == 0, 1, Intensity) *
                      ifelse(Timing == 0, 1, Timing) * ifelse(Frequency == 0, 1, Frequency))
  } else {
    out <- out %>%
      dplyr::mutate(Urgency = (((1 + Commitment) + (1 + Intensity))/4) *
                      (((1 + Timing) + (1 + Frequency))/4))
  }
  # bind data results to original data
  out <- cbind(.data, out) %>% dplyr::select(-c(text_clean))
  class(out) <- c("urgency", class(out))
  out
}

.assign_urgency_dimensions <- function(v, dimension, summarise, priority) {
  scaled <- rescaled <- word <- grammar_function <- NULL
  # get dictionaries
  if (dimension == "frequency") {
    out <- Frequency
  } else if (dimension == "timing") {
    out <- Timing
  } else if (dimension == "commitment") {
    out <- Commitment
  } else if (dimension == "intensity") {
    out <- Intensity
  }
  # check if priorities and get correct scale and data
  if (isTRUE(priority)) { # for urgency in priorities
    out <- dplyr::rename(out, scale = rescaled) %>%
      dplyr::select(word, scale)
  } else out <- dplyr::rename(out, scale = scaled) %>% dplyr::select(word, scale)
  # remove missing and group words
  out <- out %>%
    tidyr::drop_na() %>%
    dplyr::mutate(word = textstem::lemmatize_strings(word)) %>%
    dplyr::distinct() %>%
    dplyr::group_by(scale) %>%
    summarise(terms = paste0(word, collapse = "|"))
  # summarise if multiple matches per dimension per text observation
  if (summarise == "sum") {
    out <- rowSums(do.call("cbind", lapply(seq_len(nrow(out)), function(i)
      stringr::str_count(as.character(v), paste0("\\b", out$terms[i], "\\b"))*
        out$scale[i]))) # get added scores
  } else if (summarise == "mean") {
    rowcount <- rowSums(do.call("cbind", lapply(seq_len(nrow(out)), function(i)
      stringr::str_count(as.character(v), paste0("\\b", out$terms[i], "\\b"))))) # get count
    out <- rowSums(do.call("cbind", lapply(seq_len(nrow(out)), function(i)
      stringr::str_count(as.character(v), paste0("\\b", out$terms[i], "\\b"))*
        out$scale[i]))) # get added scores
    out <- out/(replace(rowcount, rowcount == 0, 1)) # get average value
  }
  out
}

#' Simulating urgency in priorities
#'
#' @param urgency Desired urgency score, optional.
#' @param commitment Desired commitment score, optional.
#' @param intensity Desired intensity score, optional.
#' @param timing Desired timing score, optional.
#' @param frequency Desired frequency score, optional.
#' @param pronoun How would you like the simulated priorities to start?
#' By default, priorities start with the pronoun "We".
#' @details
#' Users can declare a score for one or more of the
#' urgency dimensions or an urgency score.
#' This means, if users may not declare an urgency score and the
#' score for one or more dimensions at once.
#' In those cases, the urgency score is favored.
#' @return A sentence that matches the urgency or urgency dimension scores.
#' @examples
#' \donttest{
#' sim_urgency()
#' sim_urgency(urgency = 0.5)
#' sim_urgency(urgency = 2.5)
#' sim_urgency(urgency = -2.5)
#' sim_urgency(commitment = 0.6)
#' sim_urgency(commitment = 0.6, intensity = 1.4)
#' sim_urgency(commitment = 0.6, intensity = 1.4, timing = 1.4)
#' sim_urgency(commitment = 0.6, intensity = 1.2, timing = 1.4, frequency = 1.8)
#' }
#' @export
sim_urgency <- function(urgency,
                        commitment, intensity, timing, frequency,
                        pronoun = "We") {
  grammar_function <- rescaled <- NULL
  # Filter data to avoid using adverbs, for now
  Commitment <- subset(Commitment, !is.na(rescaled) & grepl("^verb$", grammar_function))
  # only "commitment" verbs for now
  Intensity <- subset(Intensity, !is.na(rescaled))
  Timing <- subset(Timing, !is.na(rescaled))
  Frequency <- subset(Frequency, !is.na(rescaled))
  if(!missing(urgency)){
    # Either timing or frequency, for now
    time_freq <- rbind(Timing, Frequency)
    combins <- expand.grid(Intensity$word, Commitment$word, time_freq$word,
                           stringsAsFactors = FALSE)
    combins <- merge(combins, Intensity, by.x = "Var1", by.y = "word")
    combins <- combins[,c("Var1", "Var2", "Var3", "rescaled")]
    combins <- merge(combins, Commitment, by.x = "Var2", by.y = "word")
    combins <- combins[,c("Var1", "Var2", "Var3", "rescaled.x", "rescaled.y")]
    combins <- merge(combins, time_freq, by.x = "Var3", by.y = "word")
    combins <- combins[,c("Var1", "Var2", "Var3", "rescaled.x",
                          "rescaled.y", "rescaled")]
    combins$combo <- as.numeric(combins$rescaled.x) *
      as.numeric(combins$rescaled.y) * as.numeric(combins$rescaled)
    selectd <- which.min.diff(abs(urgency), combins$combo)
    formul <- combins[selectd,c("Var1", "Var2", "Var3")]
    if(urgency < 0) intcom <- c(formul[1:2], sample(c("not", "never"), 1)) else
      intcom <- formul[1:2]
    out <- paste(pronoun, paste(intcom, collapse = " "), "do this", formul[3])
    cat("Urgency score: ",
        combins[selectd,"rescaled.x"] * combins[selectd, "rescaled.y"] *
          combins[selectd,"rescaled"], "\n", sep = "")
  } else {
    if(!missing(commitment)){
      commit <- Commitment$word[which.min.diff(abs(commitment), Commitment$rescaled)]
      if(commitment < 0) commit <- paste(commit, sample(c("not","never"), 1))
      if(!missing(intensity)){
        intensifier <- Intensity$word[which.min.diff(intensity, Intensity$rescaled)]
        out <- paste(pronoun, intensifier, commit, "do this")
      } else out <- paste(pronoun, commit, "do this")
    } else out <- paste(pronoun, "do this")
    if(!missing(timing)){
      timed <- Timing$word[which.min.diff(timing, Timing$rescaled)]
      out <- paste(out, timed)
    }
    if(!missing(frequency)){
      freq <- Frequency$word[which.min.diff(frequency, Frequency$rescaled)]
      out <- paste(out, freq)
    }
    cat("Urgency score:",
        ifelse(missing(commitment),1,commitment) * ifelse(missing(intensity),1,intensity) *
          ifelse(missing(timing),1,timing) *ifelse(missing(frequency),1,frequency),
        "\n")
  }
  out <- paste0(trimws(out), ".")
  out
}

which.min.diff <- function(x, y){
  diffs <- abs(x - y)
  y <- which(diffs == min(diffs, na.rm = TRUE))
  if (length(y) > 1L)
    sample(y, 1L)
  else y
}
