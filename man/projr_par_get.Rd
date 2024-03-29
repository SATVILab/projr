% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/param.R
\name{projr_par_get}
\alias{projr_par_get}
\alias{projr_param_get}
\title{Get project parameters}
\usage{
projr_par_get(...)

projr_param_get(...)
}
\arguments{
\item{...}{character.
Sequential names to specify path in list.
For example, \code{projr_param_get("a", "b")}
returns the value of \code{projr$param$a$b}.}
}
\description{
Get project parameters from \code{param} key
in \code{projr} configuration.
}
