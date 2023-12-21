library(testthat)
# devtools::load_all(path = file.path(Sys.getenv("pkg"), "projr"))
devtools::load_all()
.test_set_select()
devtools::test_active_file("tests/testthat/test-git.R")

.test_unset_select()
devtools::test()

.test_set()
devtools::test()
.test_set_fast()

yaml::read_yaml(.dir_proj_get("_projr.yml")) 


.projr_build_copy_pkg(output_run)

• TRUE is TRUE (1): test-manual-osf-download.R:137:3

── Failed tests ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
Failure (test-git.R:132:3): .projr_git_ func!tions work
suppressWarnings(.projr_git_remote_check_upstream_git()) is not TRUE

`actual`:   FALSE
`expected`: TRUE 
Backtrace:
    ▆
 1. ├─usethis::with_project(...) at test-git.R:132:3
 2. │ └─base::force(code)
 3. └─testthat::expect_true(suppressWarnings(.projr_git_remote_check_upstream_git())) at test-git.R:142:7

Failure (test-git.R:132:3): .projr_git_ functions work
suppressWarnings(.projr_git_remote_check_upstream()) is not TRUE

`actual`:   FALSE
`expected`: TRUE 
Backtrace:
    ▆
 1. ├─usethis::with_project(...) at test-git.R:132:3
 2. │ └─base::force(code)
 3. └─testthat::expect_true(suppressWarnings(.projr_git_remote_check_upstream())) at test-git.R:143:7

Error (test-git.R:132:3): .projr_git_ functions work
<GIT_ENONFASTFORWARD/libgit2_error/error/condition>
Error in `libgit2::git_remote_push`: cannot push because a reference that you are trying to update on the remote contains commits that are not present locally.
Backtrace:
    ▆
 1. ├─usethis::with_project(...) at test-git.R:132:3
 2. │ └─base::force(code)
 3. ├─testthat::expect_true(.projr_git_push_gert()) at test-git.R:153:7
 4. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
 5. │   └─rlang::eval_bare(expr, quo_get_env(quo))
 6. ├─projr:::.projr_git_push_gert()
 7. │ └─gert::git_push() at projr/R/git.R:234:3
 8. └─gert:::raise_libgit2_error(...)

Error (test-manual-osf-download.R:9:3): .projr_checkout_osf works
Error in `.projr_build_manifest_hash_pre(output_run)`: could not find function ".projr_build_manifest_hash_pre"
Backtrace:
    ▆
 1. ├─usethis::with_project(...) at test-manual-osf-download.R:9:3
 2. │ └─base::force(code)
 3. └─projr:::.projr_test_manifest_create() at test-manual-osf-download.R:19:7
 4.   └─base::rbind(.projr_build_manifest_hash_pre(output_run), .projr_build_manifest_hash_post(output_run)) at tests/testthat/helper-setup.R:150:5

[ FAIL 4 | WARN 0 | SKIP 1 | PASS 620 ]