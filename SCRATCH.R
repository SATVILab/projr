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

.projr_path_rscript_get <- function() {
  rscript <- Sys.which("Rscript")
  if (nzchar(rscript)) {
    return(rscript)
  }
  # Fallback to default R installation path
  file.path(R.home("bin"), "Rscript")
}