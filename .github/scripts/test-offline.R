# Run only from the disconnected, credential-free CI test step.
stopifnot(
  identical(Sys.getenv("PROJR_TEST_LIVE_GITHUB"), "false"),
  identical(Sys.getenv("PROJR_AUTO_INSTALL"), "false"),
  identical(Sys.getenv("GIT_ALLOW_PROTOCOL"), "file")
)
testthat::set_max_fails(Inf)
files <- c(
  list.files("R", pattern = "[.]R$", full.names = TRUE),
  list.files("tests", pattern = "[.]R$", full.names = TRUE, recursive = TRUE)
)
invisible(lapply(files, parse))
jsonlite::fromJSON(".devcontainer/devcontainer.json")
dir.create("test-results", showWarnings = FALSE)

run_tests <- function(label, lite, filter = NULL) {
  Sys.setenv(R_PKG_TEST_LITE = if (lite) "true" else "false")
  results <- tryCatch(
    devtools::test(filter = filter, reporter = "summary", stop_on_failure = FALSE),
    error = function(e) {
      writeLines(conditionMessage(e), file.path("test-results", paste0(label, "-error.txt")))
      NULL
    }
  )
  if (is.null(results)) return(FALSE)
  table <- as.data.frame(results)
  table$result <- NULL
  write.csv(table, file.path("test-results", paste0(label, ".csv")), row.names = FALSE)
  totals <- list(
    tests = nrow(table), passed = sum(table$passed), failed = sum(table$failed),
    errors = sum(table$error), warnings = sum(table$warning), skipped = sum(table$skipped)
  )
  jsonlite::write_json(totals, file.path("test-results", paste0(label, "-totals.json")),
    auto_unbox = TRUE, pretty = TRUE
  )
  print(totals)
  !any(table$failed > 0 | table$error)
}

focused_ok <- run_tests("focused", FALSE, paste(
  "^safety-offline$", "^git$", "^auth$", "^init-github$", "^hash$", "^change$",
  "^misc$", "^build-check-packages$",
  "^scripts$", "^dir-get-coverage$", "^path$", "^path-coverage$", "^path-validation$",
  "^build-post-comprehensive$", "^integration-comprehensive$",
  "^github-httr-comprehensive$", sep = "|"
))
lite_ok <- run_tests("lite", TRUE)
if (!lite_ok || !focused_ok) quit(status = 1L)
