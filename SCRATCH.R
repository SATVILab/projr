library(testthat)
devtools::load_all(path = file.path(Sys.getenv("pkg"), "projr"))

# options(error = recover, warn = 2)
# dir_temp <- file.path(tempdir(), "test")
# debugonce(".projr_init_prompt")
Sys.setenv("PROJR_TEST" = "TRUE")
projr_init()

cd /tmp; rm -rf testProjr; mkdir testProjr; cd testProjr; radian

testthat::test_file("tests/testthat/test-dir_create.R")
testthat::test_file("tests/testthat/test-build.R")

# code coverage
covr::report(file = "report.html", browse = FALSE)
cp $projr/report.html $w_dnld/