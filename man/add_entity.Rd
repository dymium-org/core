% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_entity.R
\name{add_entity}
\alias{add_entity}
\title{Add new records to an Entity object}
\usage{
add_entity(
  world,
  entity,
  newdata,
  target = NULL,
  weight_col = NULL,
  replace = TRUE,
  check_relationship_id_cols = FALSE,
  condition = TRUE
)
}
\arguments{
\item{world}{(\code{World})\cr
A \link{World} object.}

\item{entity}{(\code{character(1)})\cr
An \link{Entity}'s name.}

\item{newdata}{(\code{data.frame()}|\code{\link[data.table:data.table]{data.table::data.table()}})\cr
A new attribute data to be added to \code{entity}. The new data should have all
the same columns as the attribute data of \code{entity}.}

\item{target}{(\code{Target}|\code{integer(1)})\cr
An external target indicating how many new records from \code{newdata} should be
added. The selection is ramdomised with all records having the same weight.
However, if the \code{weight_col} is specified then the weight of each record is
equal to its value in \code{weight_col}.}

\item{weight_col}{(\code{character(1)})\cr
Name of the weight variable in \code{newdata} if any. This is only considered when
the \code{target} is set. Otherwise all records in \code{newdata} will be added to
the \link{Entity} named \code{entity}.}

\item{replace}{(\code{logical(1)})\cr
Random draw with replacement. Default as \code{TRUE}.}

\item{check_relationship_id_cols}{(\code{logical(1)})\cr
Check other relationship ids of the \link{Entity} in \code{entity} to see whether they
exist in the data of existing \code{entity} or not. Default as \code{FALSE}.}

\item{condition}{(\code{any})\cr
This allows a condition to be set whether or not this function should be
executed. Default as \code{TRUE}. But it actually accepts any object that when
is evaulated returns a logical value.}
}
\value{
the input \code{World} object.
}
\description{
This function allows new records to be added to an \link{Entity} object stored inside
a \link{World} object. This is one of the pipe-friendly functions.
}
\examples{

world <- World$new()
world$add(x = Individual$new(toy_individuals, id_col = "pid"))
add_entity(world, entity = "Individual", newdata = toy_individuals)
}
