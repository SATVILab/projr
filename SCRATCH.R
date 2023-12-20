library(testthat)
# devtools::load_all(path = file.path(Sys.getenv("pkg"), "projr"))
devtools::load_all()
.test_set_select()
devtools::test_active_file("tests/testthat/test-path.R")
.test_unset_select()
.test_set()
devtools::test()
.test_set_fast()


.projr_build_copy_pkg(output_run)

Failure (test-path.R:36:3): .file_* and .dir_* functions work
.dir_ls(dir_tmp) (`actual`) not identical to .dir_ls(dir_tmp_2) (`expected`).

`actual`:   "d1" "subdir1" "subdir1/subdir2"
`expected`:      "subdir1" "subdir1/subdir2"
Backtrace:
    ▆
 1. ├─usethis::with_project(...) at test-path.R:36:3
 2. │ └─base::force(code)
 3. └─testthat::expect_identical(...) at test-path.R:114:7
