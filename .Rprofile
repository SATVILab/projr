if (Sys.getenv("GITHUB_ACTIONS") %in% c("true", "TRUE")) {
  Sys.setenv("RENV_CONFIG_PAK_ENABLED" = "FALSE")
} else {
  source("renv/activate.R")
  options(renv.config.auto.snapshot = TRUE)
}
