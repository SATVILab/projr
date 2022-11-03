library(testthat)
devtools::load_all(path = file.path(Sys.getenv("pkg"), "projr"))

# dir_temp <- file.path(tempdir(), "test")
.projr_init_prompt(getwd())

expect_identical(
  {
    .projr_version_run_onwards_get(
      version_orig_vec = c(0, 42, 33, 1),
      bump_component = "major",
      version_format_list = list(
        components = cv,
        sep = svd
      )
    )
  },
  list(
    desc = c(
      run = "1.0.0", failure = "0.42.33-1", success = "1.0.0"
    ),
    bd = c(
      run = "1.0.0", failure = "0.42.33-1", success = "1.0.0-1"
    )
  )
)
expect_identical(
  {
    .projr_version_run_onwards_get(
      version_orig_vec = c(0, 42, 33, 9001),
      bump_component = "major",
      version_format_list = list(
        components = cv,
        sep = svd
      )
    )
  },
  list(
    desc = c(
      run = "1.0.0", failure = "0.42.33-9001", success = "1.0.0"
    ),
    bd = c(
      run = "1.0.0", failure = "0.42.33-9001", success = "1.0.0-9000"
    )
  )
)
expect_identical(
  {
    .projr_version_run_onwards_get(
      version_orig_vec = c(0, 42, 33, 1),
      bump_component = "dev",
      version_format_list = list(
        components = cv,
        sep = svd
      )
    )
  },
  list(
    desc = c(
      run = "0.42.33-2", failure = "0.42.33-2", success = "0.42.33-2"
    ),
    bd = c(
      run = "0.42.33-2", failure = "0.42.33-2", success = "0.42.33-2"
    )
  )
)
expect_identical(
  {
    .projr_version_run_onwards_get(
      version_orig_vec = c(0, 42, 33, 1),
      bump_component = NULL,
      version_format_list = list(
        components = cv,
        sep = svd
      )
    )
  },
  list(
    desc = c(
      run = "0.42.33-1", failure = "0.42.33-1", success = "0.42.33-1"
    ),
    bd = c(
      run = "0.42.33-1", failure = "0.42.33-1", success = "0.42.33-1"
    )
  )
)
