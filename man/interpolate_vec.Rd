% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/integrate.R
\name{interpolate_vec}
\alias{interpolate_vec}
\title{Convert a Time-Indexed Vector to Function of Time (i.e. x = f(t))}
\usage{
interpolate_vec(x, t = vec_seq_along(x))
}
\arguments{
\item{x}{A vector to interpolate}

\item{t}{An optional time index; if empty, \code{interpolate_vec()} will use the
index of \code{x}}
}
\value{
A (thrice) differentiable function interpolating \code{t} and \code{x}
}
\description{
This is just added code to \code{vec_fun()}- need to think about better separation
of responsiblities
}
