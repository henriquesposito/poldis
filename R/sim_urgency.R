#' Simulating urgency
#'
#' @examples
#' sim_urgency()
#' @export
sim_urgency <- function(urgency,
                        commitment, intensity, timing, frequency,
                        pronoun = "We"){
  if(!missing(urgency)){
    combins <- expand.grid(int$word, comm$word, time$word, stringsAsFactors = FALSE)
    # int <- rbind(int, c(word = "", NA, NA, NA, NA, NA, NA, NA, 1))
    # time <- rbind(time, c(word = "", NA, NA, NA, NA, NA, NA, 1, NA))
    combins <- merge(combins, int, by.x = "Var1", by.y = "word")
    combins <- combins[,c("Var1","Var2","Var3","Rescaled")]
    combins <- merge(combins, comm, by.x = "Var2", by.y = "word")
    combins <- combins[,c("Var1","Var2","Var3","Rescaled.x","Rescaled.y")]
    combins <- merge(combins, time, by.x = "Var3", by.y = "word")
    combins <- combins[,c("Var1","Var2","Var3","Rescaled.x","Rescaled.y","Rescaled")]
    combins$combo <- as.numeric(combins$Rescaled.x) * combins$Rescaled.y * as.numeric(combins$Rescaled)
    formul <- combins[which.min.diff(abs(urgency), combins$combo),c("Var1","Var2","Var3")]
    if(urgency < 0) intcom <- c(formul[1:2], sample(c("not","never"),1)) else
      intcom <- formul[1:2]
    out <- paste(pronoun, paste(intcom, collapse = " "), "do this", formul[3])
  } else {
    if(!missing(commitment)){
      commit <- comm$word[which.min.diff(abs(commitment), comm$Rescaled)]
      if(commitment<0) commit <- paste(commit, sample(c("not","never"),1))
      if(!missing(intensity)){
        intensifier <- int$word[which.min.diff(intensity, int$Rescaled)]
        out <- paste(pronoun, intensifier, commit, "do this")
      } else out <- paste(pronoun, commit, "do this")
    } else out <- paste(pronoun, "do this")
    if(!missing(timing)){
      timed <- time$word[which.min.diff(timing, time$Rescaled)]
      out <- paste(out, timed)
    }
    cat("Urgency score:",
        ifelse(missing(commitment),1,commitment) * ifelse(missing(intensity),1,intensity) *
          ifelse(missing(timing),1,timing),
        "\n")
  }
  out <- paste0(out, ".")
  out
}

which.min.diff <- function(x, y){
  diffs <- abs(x - y)
  y <- which(diffs == min(diffs, na.rm = TRUE))
  if (length(y) > 1L)
    sample(y, 1L)
  else y
}
