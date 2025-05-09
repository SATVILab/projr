% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ignore-unignore.R
\name{projr_unignore_manual}
\alias{projr_unignore_manual}
\alias{projr_unignore_manual_dir}
\alias{projr_unignore_manual_file}
\alias{projr_unignore_manual_file_git}
\alias{projr_unignore_manual_dir_git}
\alias{projr_unignore_manual_file_rbuild}
\alias{projr_unignore_manual_dir_rbuild}
\title{Manually Unignore Files or Directories in \code{.gitignore} and \code{.Rbuildignore}}
\usage{
projr_unignore_manual(unignore)

projr_unignore_manual_dir(unignore)

projr_unignore_manual_file(unignore)

projr_unignore_manual_file_git(unignore)

projr_unignore_manual_dir_git(unignore)

projr_unignore_manual_file_rbuild(unignore)

projr_unignore_manual_dir_rbuild(unignore)
}
\arguments{
\item{unignore}{A character vector of file or directory paths to be unignored.
Paths must be valid non-empty strings.}
}
\value{
Invisibly returns \code{TRUE} if the operation succeeds, or \code{FALSE} if the input
contains invalid (empty) paths.
}
\description{
These functions allow manual addition of files and directories to the
\code{.gitignore} and \code{.Rbuildignore} files \strong{after} the projr-managed block,
thereby forcing them to be \emph{not} ignored.
\itemize{
\item .unignore_manual\verb{: General function to unignore both files and directories in both }.gitignore\code{and}.Rbuildignore`. If a path does not
exist, it is treated as a file.
\item .unignore_manual_dir\verb{: Specifically unignores directories in both }.gitignore\code{and}.Rbuildignore`.
\item .unignore_manual_file\verb{: Specifically unignores files in both }.gitignore\code{and}.Rbuildignore`.
\item .unignore_manual_dir_git\verb{ and .unignore_manual_file_git}: Add
directories or files explicitly (with a \code{!} prefix) to \code{.gitignore}.
\item .unignore_manual_dir_rbuild\verb{ and .unignore_manual_file_rbuild}:
Add directories or files explicitly (with a \code{!} prefix) to \code{.Rbuildignore}.
}
}
\details{
These functions provide fine-grained control for cases where users want to
\emph{undo} any ignoring behavior for specific paths permanently. They do not
interact with the automated ignore management system of \code{projr}.
\itemize{
\item Non-existent paths provided to .unignore_manual` are assumed to be files.
\item For \code{.gitignore}, unignored directories are automatically appended with
\verb{/**} if missing, then prepended with \code{!}, ensuring proper Git \emph{unignore}
syntax.
\item For \code{.Rbuildignore}, paths are converted to regular expressions using
\code{glob2rx()}, and then prepended with \code{!} for compatibility with R's build
tools.
}
}
\examples{
# Manually unignore files and directories
projr_unignore_manual(c("output", "tempfile.log"))

# Specifically unignore directories
projr_unignore_manual_dir("data")

# Specifically unignore files
projr_unignore_manual_file("README.md")

}
\seealso{
.ignore_manual\verb{ for manually ignoring paths, and .ignore_auto} for
dynamically managed ignore entries.
}
