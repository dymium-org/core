% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ModelBinaryChoice.R
\name{ModelBinaryChoice}
\alias{ModelBinaryChoice}
\title{ModelBinaryChoice}
\format{
\link[R6:R6Class]{R6::R6Class} object inheriting \link{ModelCustom}.
}
\arguments{
\item{params}{a \code{data.frame} object.}

\item{formula}{a \code{formula} object.}

\item{preprocessing_fn}{a pre-processing function that gets applied to the
data given to the \code{predict} method before making the prediction.}

\item{newdata}{a \code{data.frame} object.}

\item{link_function}{:: \code{character(1)}\cr
default as 'logit' using \code{stats::binomial(link = "logit")}. Choice of
'logit' and 'probit'. TODO: implement 'probit' option.}
}
\value{

}
\description{
Initialisation function

This predict method returns probabilities generated from the parameters
of this \link{Model} object.
}
