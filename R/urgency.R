#' Urgency Analysis
#'
#' @param .data A data frame, priorities data frame coded using
#' `select_priorities()`, or text vector.
#' For data frames, function will search for "text" variable.
#' For priorities data frame function will search for "priorities" variable.
#' If missing, opens the webpage containing the urgency codebook.
#' @param normalize Would you like urgency scores to be normalized?
#' By default, urgency scores are normalized by "tokens",
#' the number of words in text observation.
#' Users can also declare "none", for no normalization.
#' Scores for each dimension are weighted to account for the
#' different number of words in each dimension before normalization.
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
#' The terms included in each of these dimensions of urgency were first adapted
#' from established lexicon dictionaries and later complemented by other terms
#' inductively found in texts during pre-testing.
#' The words in the dictionaries for each dimension are scored on a scale
#' between 0 and 1, with 1 being the maximum value obtainable and contributing
#' the most to the urgency score of the sentence.
#' Urgency terms were validated and adjusted through an online survey
#' with 206 participants that took place between July and August of 2024.
#' The survey collected responses anonymously but included basic demographic
#' information about participants, as English proficiency and education levels.
#' The survey results were recorded as counts of the number of participants
#' who selected an urgency-related word as more urgent than its pair.
#' To analyze the survey results, we employed Bradley-Terry models for
#' paired comparisons using the BradleyTerry2 R package (Turner and Firth, 2012).
#' A rank of the words for each dimension of urgency was obtained from the analysis,
#' which were then used to adjust and validate the urgency word scores
#' in the dictionaries.
#' For more information on the dimensions, scores, or the survey on urgency,
#' please run `get_urgency()` to access the urgency codebook.
#' @return A scored data frame for each dimension of urgency.
#' @import dplyr
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
get_urgency <- function(.data, normalize = "tokens") {
  Frequency <- Timing <- Commitment <- Intensity <- Urgency <- text_clean <- NULL
  # tries to open urgency codebook if no argument is declared
  if (missing(.data)) open_codebook(codebook = "urgency")
  # get text variable
  if (inherits(.data, "priorities")) {
    text_clean <- getElement(.data, "priorities")
  } else if (inherits(.data, "data.frame")) {
    text_clean <- getElement(.data, "text")
  } else text_clean <- .data
  # assign urgency dimensions
  out <- data.frame("text_clean" = .clean_token(text_clean)) %>%
    dplyr::mutate(Frequency = .assign_frequencies(text_clean)*1.2, #62 terms
                  Timing = .assign_timing(text_clean)*1.3, #41 terms
                  Intensity = .assign_intensity(text_clean), #103 terms
                  Commitment = .assign_commitment(text_clean)*1.1) #85 terms
  if (normalize == "tokens") {
    out <- out %>%
      dplyr::mutate(Urgency = (Frequency + Timing + Intensity + Commitment)/
                      nchar(text_clean))
  } else if (normalize == "none") {
    out <- out %>%
      dplyr::mutate(Urgency = (Frequency + Timing + Intensity + Commitment))
  }
  out <- cbind(.data, out) %>% dplyr::select(-c(text_clean))
  class(out) <- c("urgency", class(out))
  out
}

.assign_frequencies <- function(v) {
  score_frequency_scaled <- NULL
  freq_words <- frequency[,c(1,4)] %>%
    tidyr::drop_na() %>%
    dplyr::mutate(frequency = textstem::lemmatize_words(frequency)) %>%
    dplyr::distinct() %>%
    dplyr::group_by(score_frequency_scaled) %>%
    summarise(terms = paste0(frequency, collapse = "|"))
  rowSums(do.call("cbind", lapply(seq_len(nrow(freq_words)), function(i)
    stringr::str_count(as.character(v), paste0("\\b", freq_words$terms[i], "\\b"))*
      freq_words$score_frequency_scaled[i])))
}

.assign_timing <- function(v) {
  score_timing_scaled <- NULL
  timing_words <- timing[,c(1,4)] %>%
    tidyr::drop_na() %>%
    dplyr::mutate(timing = textstem::lemmatize_words(timing)) %>%
    dplyr::distinct() %>% # 41 terms
    dplyr::group_by(score_timing_scaled) %>%
    summarise(terms = paste0(timing, collapse = "|"))
  rowSums(do.call("cbind", lapply(seq_len(nrow(timing_words)), function(i)
    stringr::str_count(as.character(v), paste0("\\b", timing_words$terms[i], "\\b"))*
      timing_words$score_timing_scaled[i])))
}

.assign_intensity <- function(v) {
  score_intensity_scaled <- NULL
  intensity_words <- intensity[,c(1, 4)] %>%
    tidyr::drop_na() %>%
    dplyr::mutate(intensity = textstem::lemmatize_words(intensity)) %>%
    dplyr::distinct() %>%
    dplyr::group_by(score_intensity_scaled) %>%
    summarise(terms = paste0(intensity, collapse = "|"))
  rowSums(do.call("cbind", lapply(seq_len(nrow(intensity_words)), function(i)
    stringr::str_count(as.character(v), paste0("\\b", intensity_words$terms[i], "\\b"))*
      intensity_words$score_intensity_scaled[i])))
}

.assign_commitment <- function(v) {
  score_commitment_scaled <- NULL
  commitment_words <- commitment[,c(1,4)] %>%
    tidyr::drop_na() %>%
    dplyr::mutate(commitment = textstem::lemmatize_words(commitment)) %>%
    dplyr::distinct() %>%
    dplyr::group_by(score_commitment_scaled) %>%
    summarise(terms = paste0(commitment, collapse = "|"))
  out <- list()
  rowSums(do.call("cbind", lapply(seq_len(nrow(commitment_words)), function(i)
    stringr::str_count(as.character(v), paste0("\\b", commitment_words$terms[i], "\\b"))*
      commitment_words$score_commitment_scaled[i])))
}

#' Simulating urgency
#'
#' @examples
#' sim_urgency()
#' @export
sim_urgency <- function(urgency,
                        commitment, intensity, timing, frequency,
                        pronoun = "We"){
  if(!missing(urgency)){
    combins <- expand.grid(c("",int$word), comm$word, stringsAsFactors = FALSE)
    combins <- merge(combins, int, by.x = "Var1", by.y = "word")
    combins <- combins[,c("Var1","Var2","Rescaled")]
    combins <- merge(combins, comm, by.x = "Var2", by.y = "word")
    combins <- combins[,c("Var1","Var2","Rescaled.x","Rescaled.y")]
    combins$combo <- combins$Rescaled.x * combins$Rescaled.y
    intcom <- combins[which.min(abs(urgency - combins$combo)),c("Var1","Var2")]
    out <- paste(pronoun, paste(intcom, collapse = " "), "do this.")
  } else if(!missing(commitment)){
    commit <- comm$word[which.min(abs(abs(commitment) - comm$Rescaled))]
    if(commitment<0) commit <- paste(commit, sample(c("not","never"),1))
    if(!missing(intensity)){
      intensifier <- int$word[which.min(abs(abs(intensity) - int$Rescaled))]
      out <- paste(pronoun, intensifier, commit, "do this.")
    } else out <- paste(pronoun, commit, "do this.")
  } else out <- paste(pronoun, "do this.")
  out
}
