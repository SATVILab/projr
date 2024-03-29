% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/profile.R
\name{projr_profile_create_local}
\alias{projr_profile_create_local}
\title{Create a local \code{projr} profile}
\usage{
projr_profile_create_local(overwrite = FALSE)
}
\arguments{
\item{overwrite}{logical.
If \code{TRUE}, then overwrite \verb{_projr-local.yml} if it already exists.
Default is \code{FALSE}.}
}
\description{
Create a \code{projr} profile with highest precedence
(i.e. its settings overwrite any others) that is
ignored by Git.
Useful to avoid bloat through many entries in \verb{_projr.yml}
or extra \verb{_projr-<profile>.yml} files.
Creates a file \verb{_projr-local.yml} with empty settings and ignores it from
\code{.Rbuildignore} and \code{.gitignore}.
}
\details{
Note that if any setting in \verb{_projr-local.yml} is empty,
then a lower-precendence file's setting
(i.e. from \verb{_projr-<profile>.yml} or \verb{_projr.yml}) is used.
Empty settings are by default indicated by \code{~}.
}
\seealso{
projr_profile_create_local,projr_yml_get
}
