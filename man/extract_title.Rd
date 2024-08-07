% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/text_tools.R
\name{extract_title}
\alias{extract_title}
\title{Extract first sentence from text}
\usage{
extract_title(v)
}
\arguments{
\item{v}{Text vector.}
}
\value{
A list of the first sentences in text.
}
\description{
A lot of information is contained in the first sentence of a text.
In political texts, for example, dates and locations are often contained
in the first sentence of the text.
}
\examples{
extract_title("This is the first sentence. This is the second sentence.")
}
