#' Urgency Analysis
#'
#' @param v Text vector or annotated data frame.
#' @param summarise How to handle multiple matches for the same dictionary
#' in the same text observation?
#' By default, multiple matches are added together and
#' their "sum" per text observation is returned.
#' Users can, instead, choose the "mean", which returns the average
#' score per dictionary per text observation, the "max" or the "min",
#' when there are multiple matches.
#' @details
#' Urgency in political discourses is an expression of how necessary and/or
#' how soon an action should be undertaken or completed.
#' This is measured along four dictionaries,
#' two related to necessity (e.g., degree of intensity and of commitment)
#' and two related to timing (e.g., frequency and timing of action).
#' The dictionaries include terms for each of these dimensions of urgency.
#' Their scores were developed through an online survey that took place
#' in June 2026. The survey results were recorded as counts of the number of
#' participants who selected an urgency-related word as more urgent than its pair.
#' To analyze the survey results, we employed Bradley-Terry models for
#' paired comparisons. We use raw log odds coefficients to calculate urgency.
#' Urgency scores for each disctionary are returned.
#' The aggregated urgency scores is calculated by adding these scores and
#' then divided by the coefficient for the word "must", to ensure
#' interpretability (i.e., an aggregated score of 1 in equivalent to "we must do").
#' Users are welcome to change how urgency scores are aggregated and transform these
#' scores into probabilities or odds ratios as necessary.
#' @return A scored data frame for each dimension of urgency.
#' @import dplyr
#' @importFrom textstem stem_strings
#' @examples
#' \donttest{
#' get_urgency(US_inaugural_addresses_1993_2025$text)
#' #a = get_urgency(select_priorities(US_inaugural_addresses_1993_2025$text))
#' #summary(get_urgency(US_inaugural_addresses_1993_2025$text))
#' #plot(get_urgency(US_inaugural_addresses_1993_2025$text))
#' }
#' @export
get_urgency <- function(v, summarise = "sum") {
  Frequency <- Timing <- Commitment <- Intensity <- Urgency <- text <- NULL
  # open paper if no argument is declared in the future
  #if (missing(v)) open_codebook(codebook = "urgency")
  # get text variable
  if (inherits(v, "priorities")) {
    text <- stats::na.omit(textstem::stem_strings(getElement(v, "priorities")))
  } else if (inherits(v, "data.frame")) {
    text <- textstem::stem_strings(getElement(v, "text"))
  } else text <- textstem::stem_strings(v)
  # assign urgency dimensions
  out <- data.frame("text" = text) %>%
    dplyr::mutate(Frequency = .assign_urgency_dimensions(
      text, udimension = "frequency", summarise = summarise),
                  Timing = .assign_urgency_dimensions(
                    text, udimension = "timing", summarise = summarise),
                  Intensity = .assign_urgency_dimensions(
                    text, udimension = "intensity", summarise = summarise),
                  Commitment = .assign_urgency_dimensions(
                    text, udimension = "commitment", summarise = summarise))
  # calculate urgency scores
  out <- out %>%
      dplyr::mutate(Urgency = (Commitment  + Intensity +
                                 Timing + Frequency)/7.414257) # "must" coefficient
  # bind data results to original data
  out <- cbind(v, out) %>% dplyr::select(-c(text))
  class(out) <- c("urgency", class(out))
  out
}

.assign_urgency_dimensions <- function(v, udimension, summarise) {
  coefficients <- stem_word <- NULL
  # get dictionaries
  out <- dplyr::filter(BT_models, dimension == udimension) %>%
    dplyr::select(word_stem, coefficients) %>%
    dplyr::distinct()
  matrix_count <- do.call("cbind", lapply(seq_len(nrow(out)), function(i)
    stringr::str_count(as.character(v), paste0("\\b", out$word_stem[i], "\\b"))))
  colnames(matrix_count) <- unlist(out$word_stem)
  matrix_count <- data.frame(matrix_count[, colSums(matrix_count != 0) > 0])
  values <- out$coefficients[out$word_stem %in% colnames(matrix_count)]
  if (summarise == "sum" | summarise == "mean") {
    out <- rowSums(as.data.frame(mapply(`*`, matrix_count, values)))
    if (summarise == "mean") out <- out/rowSums(matrix_count)
  } else if (summarise == "max") {
    matrix_count[matrix_count > 0] <- 1
    out <- apply(as.data.frame(mapply(`*`, matrix_count, values)), 1, max)
  } else if (summarise == "min") {
    matrix_count[matrix_count > 0] <- 1
    out <- apply(as.data.frame(mapply(`*`, matrix_count, values)), 1, min)
  }
  out
}

#' #' Simulating urgency in priorities
#' #'
#' #' @param urgency Desired urgency score, optional.
#' #' @param commitment Desired commitment score, optional.
#' #' @param intensity Desired intensity score, optional.
#' #' @param timing Desired timing score, optional.
#' #' @param frequency Desired frequency score, optional.
#' #' @param pronoun How would you like the simulated priorities to start?
#' #' By default, priorities start with the pronoun "We".
#' #' @details
#' #' Users can declare a score for one or more of the
#' #' urgency dimensions or an urgency score.
#' #' This means, if users may not declare an urgency score and the
#' #' score for one or more dimensions at once.
#' #' In those cases, the urgency score is favored.
#' #' @return A sentence that matches the urgency or urgency dimension scores.
#' #' @examples
#' #' \donttest{
#' #' sim_urgency()
#' #' sim_urgency(urgency = 1)
#' #' sim_urgency(commitment = 0.8, intensity = 1, timing = 1.2, frequency = 1.4)
#' #' }
#' #' @export
#' sim_urgency <- function(urgency,
#'                         commitment, intensity, timing, frequency,
#'                         pronoun = "We") {
#'   out <- select(BT_models, terms, coefficients, dimension)
#'   Commitment <- filter(out, dimension == "commitment")
#'   Intensity <- filter(out, dimension == "intensity")
#'   Timing <- filter(out, dimension == "timing")
#'   Frequency <- filter(out, dimension == "frequency")
#'   if(!missing(urgency)){
#'     # Either timing or frequency, for now
#'     time_freq <- rbind(Timing, Frequency)
#'     combins <- expand.grid(Intensity$terms, Commitment$terms, time_freq$terms,
#'                            stringsAsFactors = FALSE)
#'     combins <- merge(combins, Intensity, by.x = "Var1", by.y = "terms")
#'     combins <- combins[,c("Var1", "Var2", "Var3", "coefficients")]
#'     combins <- merge(combins, Commitment, by.x = "Var2", by.y = "terms")
#'     combins <- combins[,c("Var1", "Var2", "Var3", "coefficients.x", "coefficients.y")]
#'     combins <- merge(combins, time_freq, by.x = "Var3", by.y = "terms")
#'     combins <- combins[,c("Var1", "Var2", "Var3", "coefficients.x",
#'                           "coefficients.y", "coefficients")]
#'     combins$combo <- as.numeric(combins$coefficients.x) *
#'       as.numeric(combins$coefficients.y) * as.numeric(combins$coefficients)
#'     selectd <- which.min.diff(abs(urgency), combins$combo)
#'     formul <- combins[selectd,c("Var1", "Var2", "Var3")]
#'     if(urgency < 0) intcom <- c(formul[1:2], sample(c("not", "never"), 1)) else
#'       intcom <- formul[1:2]
#'     out <- paste(pronoun, paste(intcom, collapse = " "), "do this", formul[3])
#'     cat("Urgency score: ",
#'         combins[selectd,"coefficients.x"] * combins[selectd, "coefficients.y"] *
#'           combins[selectd,"coefficients"], "\n", sep = "")
#'   } else {
#'     if(!missing(commitment)){
#'       commit <- Commitment$terms[which.min.diff(abs(commitment), Commitment$coefficients)]
#'       if(commitment < 0) commit <- paste(commit, sample(c("not","never"), 1))
#'       if(!missing(intensity)){
#'         intensifier <- Intensity$terms[which.min.diff(intensity, Intensity$coefficients)]
#'         out <- paste(pronoun, intensifier, commit, "do this")
#'       } else out <- paste(pronoun, commit, "do this")
#'     } else out <- paste(pronoun, "do this")
#'     if(!missing(timing)){
#'       timed <- Timing$terms[which.min.diff(timing, Timing$coefficients)]
#'       out <- paste(out, timed)
#'     }
#'     if(!missing(frequency)){
#'       freq <- Frequency$terms[which.min.diff(frequency, Frequency$coefficients)]
#'       out <- paste(out, freq)
#'     }
#'     cat("Urgency score:",
#'         ifelse(missing(commitment),1,commitment) * ifelse(missing(intensity),1,intensity) *
#'           ifelse(missing(timing),1,timing) *ifelse(missing(frequency),1,frequency),
#'         "\n")
#'   }
#'   out <- paste0(trimws(out), ".")
#'   out
#' }
#'
#' which.min.diff <- function(x, y){
#'   diffs <- abs(x - y)
#'   y <- which(diffs == min(diffs, na.rm = TRUE))
#'   if (length(y) > 1L)
#'     sample(y, 1L)
#'   else y
#' }
