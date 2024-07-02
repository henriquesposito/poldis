#' @export
plot.urgency <- function(x, FUN = mean, ...) {
  thisRequires("fmsb")
  x <- x[, c("Frequency", "Timing", "Intensity", "Commitment")]
  out <- apply(x, 2, FUN)
  out <- data.frame(rbind(rep(round(max(out) + 0.1, digits = 3), 4), rep(0,4), out))
  fmsb::radarchart(out, axistype = 0, seg = 2, cglcol = "lightgrey", pcol = c("purple"))
}

#' @export
summary.topics <- function(objects, ...) {
  sort(table(unlist(strsplit(objects, split = ", "))), decreasing = TRUE)
}

#' @export
plot.topics <- function(x, ...) {
  topics <- NULL
  thisRequires("ggplot2")
  data.frame(topics = unlist(strsplit(objects, split = ", "))) %>%
    dplyr::group_by(topics) %>%
    dplyr::count() %>%
    ggplot2::ggplot(ggplot2::aes(topics, sort(n, decreasing = TRUE))) +
    ggplot2::geom_col() +
    ggplot2::labs(x = "", y = "") +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 80, vjust = 1, hjust = 1))
}
