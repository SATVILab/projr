if (Sys.getenv("GITHUB_ACTIONS") == "true") {
  Sys.setenv("RENV_CONFIG_PAK_ENABLED" = "FALSE")
} else {
  source("renv/activate.R")
  options(renv.config.auto.snapshot = TRUE)
}
