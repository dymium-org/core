% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/alignment.R
\name{alignment}
\alias{alignment}
\title{Alignment}
\usage{
alignment(prediction, target)
}
\arguments{
\item{prediction}{a data.table with columns that contain predicted probabilities}

\item{target}{a named list that contains name-number value pairs that represent
the number of agents to be aligned to each choice.}
}
\value{
a character vector with the same length as the nubmer of rows of
\code{prediction}. Where the index of the vector correspond to the index of the rows
of \code{prediction},
}
\description{
Microsimulation alignment ensures that the simulation outcome matches its given alignment
target while the selection probabilities are based on each individual's likelihood.
}
