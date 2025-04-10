% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/profile.R
\name{projr_profile_create_local}
\alias{projr_profile_create_local}
\title{Create a local \code{projr} profile}
\usage{
projr_profile_create_local()
}
\description{
Create a \code{projr} profile with highest precedence
(i.e. its settings overwrite any others) that is
ignored by Git.
}
\details{
Note that if any setting in \verb{_projr-local.yml} is empty,
then a lower-precendence file's setting
(i.e. from \verb{_projr-<profile>.yml} or \verb{_projr.yml}) is used.
Empty settings are by default indicated by \code{~}.
}
\seealso{
.profile_create_local.yml_get
}
