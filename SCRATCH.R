library(testthat)
devtools::load_all()

.test_set_select()
devtools::test()

renv::install('devtools', prompt = FALSE)
devtools::load_all()
projr_renv_update()


