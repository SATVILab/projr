library(testthat)
devtools::load_all(path = file.path(Sys.getenv("pkg"), "projr"))

# options(error = recover, warn = 2)
# dir_temp <- file.path(tempdir(), "test")
# debugonce(".projr_init_prompt")
Sys.setenv("PROJR_TEST" = "FALSE")
projr_init()

cd /tmp; rm -rf testProjr; mkdir testProjr; cd testProjr; radian