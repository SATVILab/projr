% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build.R
\name{projr_build_output}
\alias{projr_build_output}
\alias{projr_build_major}
\alias{projr_build_minor}
\alias{projr_build_patch}
\title{Build project to output}
\usage{
projr_build_output(
  bump_component,
  msg = NULL,
  args_engine = list(),
  profile = NULL
)

projr_build_major(msg = NULL, args_engine = list(), profile = NULL)

projr_build_minor(msg = NULL, args_engine = list(), profile = NULL)

projr_build_patch(msg = NULL, args_engine = list(), profile = NULL)
}
\arguments{
\item{bump_component}{"major", "minor", "patch" or missing.
Specifies version component to increment.
If missing, then is set equal to the lowest version component
in used version format.
No default (i.e. is missing by default).}

\item{msg}{character.
Message to append to Git commit messages.
Default is \code{NULL}, in which
case the user is prompted for a message or,
if the session is not interactive, it is
left empty.
Default is \code{NULL}.
Note that the Git messages in this case would not be blank -
they would simply consist of details as to the version
being bumped to and the stage in the build process
at which the commit was made.}

\item{args_engine}{list.
Arguments passed to the
rendering engine
(\code{rmarkdown::render}, \code{quarto::render} or \code{bookdown::render_book}).}

\item{profile}{character.
\code{projr} profile to use. Will set the environment variable
\code{PROJR_PROFILE} to this value at the start of the build,}
}
\description{
\code{projr_build_output} Builds project to output, which
means recording the input and output data hashes,
building the actual bookdown document and
saving and archiving selected output.

\code{projr_build_major}, \code{projr_build_minor} and \code{projr_build_patch}
are wrappers around \code{projr_build_output} with the version component
bumped set automatically, e.g. \code{projr_build_major()} is equivalent
\code{projr_build_output(bump_component = "major")}.
}
