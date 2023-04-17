source("renv/activate.R")
if (!nzchar(Sys.getenv("GITHUB_PAT"))) {
  if (!requireNamespace("gitcreds", quietly = TRUE)) {
    utils::install.packages("gitcreds")
  }
  try(Sys.setenv(GITHUB_PAT = gitcreds::gitcreds_get(
    url = "https://github.com"
  )$password), silent = TRUE)
  if (!nzchar(Sys.getenv("GITHUB_PAT"))) {
    stop("Failed to get GITHUB_PAT environment variable. Add GITHUB_PAT=<gh_pat> to .Renviron file.") # nolint
  }
}
