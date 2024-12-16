library(testthat)
devtools::load_all()
.test_set_select()
devtools::test()

renv::install('devtools', prompt = FALSE)
devtools::load_all()
projr_renv_update()


pkg_vec <- c(
  "bookdown", "cffr", "codemeta", "covr", "DT", "httr", "osfr", "piggyback", "quarto"
)
renv::install(pkg_vec, prompt = FALSE)

if (bump_component %in% c("major", "minor", "patch")) {
  version_desc_failure <- 
    .projr_version_concat(version_orig_vec, version_format_list$sep)
  version_desc_run <- version_desc_success <-
    .projr_version_concat(
      version_update_vec, version_format_list$sep
    )
}