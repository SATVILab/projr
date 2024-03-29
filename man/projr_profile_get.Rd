% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/profile.R
\name{projr_profile_get}
\alias{projr_profile_get}
\title{Get active projr profile}
\usage{
projr_profile_get()
}
\value{
Character vector of length 1
corresponding to active \code{projr} profile.
If "default" is returned, then no profile is active.
Note that any \verb{_projr-local.yml} file will
always overwrite profile-specific and default settings.
}
\description{
Get active \code{projr} profile.
For each setting, preference is given if specified
in \verb{_projr-local.yml} and, failing that, in \verb{_projr-<profile>.yml}
(or in the \verb{build-<profile>} and \verb{directories-<profile>} keys).
Note that the active \code{projr} profile is only
equal to the \code{PROJR_PROFILE} environment variable
if the latter is set and the corresponding profile exists
(either as keys  in \verb{_projr.yml} or as a file \verb{_projr-<profile>.yml}).
}
