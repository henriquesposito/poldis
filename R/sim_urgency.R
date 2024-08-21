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
    intcom <- combins[which.min(abs(abs(urgency) - combins$combo)),c("Var1","Var2")]
    if(urgency < 0) intcom <- c(intcom, sample(c("not","never"),1))
    out <- paste(pronoun, paste(intcom, collapse = " "), "do this")
  } else {
    if(!missing(commitment)){
      commit <- comm$word[which.min(abs(abs(commitment) - comm$Rescaled))]
      if(commitment<0) commit <- paste(commit, sample(c("not","never"),1))
      if(!missing(intensity)){
        intensifier <- int$word[which.min(abs(abs(intensity) - int$Rescaled))]
        out <- paste(pronoun, intensifier, commit, "do this")
      } else out <- paste(pronoun, commit, "do this")
    } else out <- paste(pronoun, "do this")
  }
  out <- paste0(out, ".")
  out
}
