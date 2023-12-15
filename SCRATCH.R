library(testthat)
# devtools::load_all(path = file.path(Sys.getenv("pkg"), "projr"))
devtools::load_all()


# devtools::test_active_file("tests/testthat/test-osf-to_manual.R")
devtools::test_active_file(
  "tests/testthat/test-change.R"
)

.projr_manifest_read(.projr_dir_proj_get("manifest.csv"))
