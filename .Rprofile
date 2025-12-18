is_ubuntu_bioc <- nzchar(Sys.getenv("BIOCONDUCTOR_DOCKER_VERSION"))
is_gha <- Sys.getenv("GITHUB_ACTIONS") %in% c("true", "TRUE")
if (is_gha || !is_ubuntu_bioc) {
  Sys.setenv("RENV_CONFIG_PAK_ENABLED" = "FALSE")
} else {
  source("renv/activate.R")
  options(renv.config.auto.snapshot = TRUE)
}
