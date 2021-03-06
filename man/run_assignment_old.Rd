% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_assignment.R
\name{run_assignment_old}
\alias{run_assignment_old}
\title{Iterate over the model once, completing all assignment steps}
\usage{
run_assignment_old(starting_values, n_to_add, tot_iters, p, q)
}
\arguments{
\item{starting_values}{Data frame formatted correctly for this model, probably from \code{prep_data()}}

\item{n_to_add}{Total number objects to assign, numeric}

\item{tot_iters}{Number of iterations to run, integer > 0}

\item{p}{Innovation parameter, numeric 0-1}

\item{q}{Immitation parameter, numeric 0-1}
}
\value{
data frame formatted like starting values with added columns of predictions
}
\description{
This functions runs all iterations for data that has been prepped WITH BPR COMPUTED
}
