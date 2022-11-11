library(testthat)
devtools::load_all(path = file.path(Sys.getenv("pkg"), "projr"))

library(testthat)
devtools::load_all()
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

path_dir <- file.path(
  Sys.getenv("w_gdrive"), "ProjectILC", "DataRawILC",
  "2022_02", "sun", "version_1"
)
fn_vec <- list.files(path_dir, full.names = TRUE)
x <- readChar(fn_vec[1], file.info(fn_vec[1])$size)
digest::digest(x)

path_dir <- file.path(
  Sys.getenv("w_gdrive"), "ProjectILC", "DataRawILC",
  "2021_01", "sun", "version_3", "WBA-ILC_20210319-ReAnalysis"
)
# standard digest
fn_vec <- list.files(path_dir, full.names = TRUE)
time_start <- proc.time()[3]
vapply(fn_vec[2:3], function(fn) {
  digest::digest(fn, serialize = FALSE, file = TRUE)
}, character(1))
time_end <- proc.time()[3]
(time_end - time_start) / 60

# vdigest and with a different algorithm
path_dir <- file.path(
  Sys.getenv("w_gdrive"), "ProjectILC", "DataRawILC",
  "2021_01", "sun", "version_3", "WBA-ILC_20210319-ReAnalysis"
)
vdigest_file <- digest::getVDigest(algo = "xxhash64", errormode = "warn")
fn_vec <- list.files(path_dir, full.names = TRUE)
time_start_v <- proc.time()[3]
vapply(fn_vec[2:3], vdigest_file, character(1), serialize = FALSE, file = TRUE)
time_end_v <- proc.time()[3]
(time_end_v - time_start_v) / 60

x <- readBin(fn_vec[1], "raw")
digest::digest(x)
fn_vec <- "/mnt/h/Shared drives/ProjectILC/DataRawILC/2021_01/sun/version_3/WBA-ILC_20210319-ReAnalysis/HD10005_SATVI_20210308_MTBLysate_028.zip"
x <- readChar(fn_vec[1], file.info(fn_vec[1])$size)
"H:/Shared drives/ProjectILC/DataRawILC/2021_01/sun/version_3/HD10005_SATVI_20210308_MTBLysate_028.zip"
