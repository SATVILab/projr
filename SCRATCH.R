library(testthat)
devtools::load_all()
.test_set_select()
devtools::test()

"/usr/local/lib/R/bin/Rscript" -e "renv::restore(rebuild = TRUE, prompt = FALSE, library = c('/tmp/Rtmp4DsDsT/test_renv_fca61a36ad78/renv_lib_check', '/usr/local/lib/R/library'))"

[1] "renv::restore(project = '/tmp/RtmpbhbtvR/test_renv_1218c3bdda6bd', library = c('/tmp/RtmpbhbtvR/test_renv_1218c3bdda6bd/renv_lib_check', '/usr/local/lib/R/library'), prompt = FALSE, rebuild = TRUE)"


test -d "/tmp/RtmpbhbtvR/test_renv_1218c3bdda6bd" || echo "renv project directory not found"
test -d "/tmp/RtmpbhbtvR/test_renv_1218c3bdda6bd/renv_lib_check" || echo "first library directory not found"
test -d "/usr/local/lib/R/library" || echo "second library directory not found"
