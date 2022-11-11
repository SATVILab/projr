library(testthat)
devtools::load_all(path = file.path(Sys.getenv("pkg"), "projr"))

############
## INIT
############

  if (answer_git == 2) {
    file.copy(
      system.file(
        "project_structure", "DELETE-AFTER-DOING.md",
        package = "projr"
      ),
      dir_proj
    )
    cat("\n")
    cat("\n")
    message("Follow steps in DELETE-AFTER-DOING.md")

    if (answer_readme %in% c(1, 2)) {
      if (file.exists(path_readme)) unlink(path_readme)
      if (!identical(readme[length(readme)], "")) readme <- c(readme, "")
      writeLines(text = readme, con = path_readme)
      if (answer_readme == 1) {
        try(rmarkdown::render(
          file.path(dir_proj, "README.Rmd"),
          output_format = "md_document",
          quiet = TRUE
        ))
      }
    }
    return(TRUE)
  }






option_license_vec <- c(
  "CC-BY (good for data and analysis projects - permissive, but requires attribution)", # nolint
  "Apache 2.0 (good for function packages - permissive with patent protection)", # nolint
  "CC0 (dedicates to public domain)",
  "Proprietary (private code, i.e. no-one may use, share or change it)"
)
long_to_short_license_vec <- setNames(c(
  "CC-BY", "Apache 2.0", "CC0", "Proprietary"
), option_license_vec)

.projr_init_prompt_ind(
  .var = "LICENSE",
  nm_item_long = "license",
  option_default = option_license_vec,
  option_other = c("Complete later"),
  option_check = c("Complete later"),
  answer_translate = long_to_short_license_vec,
  answer_auto = "Complete later"
)





############
### INIT
############

   # please provide the GitHub user name'

.var <- "GITHUB_USER_NAME"
nm_item_long <- "GitHub user/organisation name"
option_default <- c("Specify other", "Complete later")
option_default_n <- c("Yes", "No", "Complete later")
# debugonce(.projr_init_prompt_ind)


debugonce(.projr_init_prompt_ind)
.projr_init_prompt_ind(
  .var = "GITHUB_USER_NAME",
  nm_item_long = "GitHub user/organisation name"
)




   # please provide the GitHub user name
   if (nzchar(Sys.getenv("PROJR_GITHUB_USERNAME"))) {
     nm_gh <- strsplit(Sys.getenv("PROJR_GITHUB_USERNAME"), ";")[[1]]
     answer_gh <- menu(
       c(nm_gh, "Specify other", "Complete later"),
       title = "Please select GitHub user/organisation name for this project"
     )
     cat("\n")
     if (answer_gh %in% seq_along(nm_gh)) {
       nm_gh <- nm_gh[answer_gh]
       ask_gh <- FALSE
       completed_gh <- TRUE
     } else if (answer_gh == length(nm_gh) + 1) {
       cat(
         "Please provide the GitHub user/organisation name for this project.\n"
       ) # nolint
       nm_gh <- readline(prompt = ">> ")
       cat("\n")
       ask_gh <- TRUE
     } else {
       ask_gh <- FALSE
       completed_gh <- FALSE
     }
   } else {
     cat(
       "Please provide the GitHub user/organisation name for this project.\n"
     )
     nm_gh <- readline(prompt = ">> ")
     cat("\n")
     ask_gh <- TRUE
   }

   if (ask_gh) {
     answer_gh <- menu(
       c("Yes", "No", "Complete later"),
       title = paste0(
         "Is the GitHub user/organisation name `", nm_gh, "` correct?"
       )
     )
     cat("\n")
     ask_gh <- answer_gh == 2
     completed_gh <- answer_gh != 3
   }

   while (ask_gh) {
     cat("Please provide the GitHub user/organisation name.\n")
     nm_gh <- readline(prompt = ">> ")
     answer_gh <- menu(
       c("Yes", "No", "Complete later"),
       title = paste0(
         "Is the GitHub user/organisation name `", nm_gh, "` correct?"
       )
     )
     cat("\n")
     ask_gh <- answer_gh == 2
     completed_gh <- answer_gh != 3
   }
   if (!completed_gh) {
     nm_gh <- .projr_init_prompt_ind(
       .var = "PROJR_GITHUB_USER_NAME", nm_item_long = "Github user/organisation"
     )
   }

   # first name
   if (nzchar(Sys.getenv("PROJR_FIRST_NAME"))) {
     nm_first <- strsplit(Sys.getenv("PROJR_FIRST_NAME"), ";")[[1]]
     answer_first <- menu(
       c(nm_first, "Specify other", "Complete later"),
       title = "Please select your first name."
     )
     cat("\n")
     if (answer_first %in% seq_along(nm_first)) {
       nm_first <- nm_first[answer_first]
       ask_first <- FALSE
       completed_first <- TRUE
     } else if (answer_first == length(nm_first) + 1) {
       cat("Please provide your first name.\n") # nolint
       nm_first <- readline(prompt = ">> ")
       cat("\n")
       ask_first <- TRUE
     } else {
       ask_first <- FALSE
       completed_first <- FALSE
     }
   } else {
     cat("Please provide your first name.\n")
     nm_first <- readline(prompt = ">> ")
     cat("\n")
     ask_first <- TRUE
   }

   if (ask_first) {
     answer_first <- menu(
       c("Yes", "No", "Complete later"),
       title = paste0("Is the first name `", nm_first, "` correct?")
     )
     cat("\n")
     ask_first <- answer_first == 2
     completed_first <- answer_first != 3
   }

   while (ask_first) {
     cat("Please provide your first name.\n")
     nm_first <- readline(prompt = ">> ")
     answer_first <- menu(
       c("Yes", "No", "Complete later"),
       title = paste0("Is the first name `", nm_first, "` correct?")
     )
     cat("\n")
     ask_first <- answer_first == 2
     completed_first <- answer_first != 3
   }

   if (!completed_first) {
     nm_first <- "FIRST_NAME"
   }

   # last name
   if (nzchar(Sys.getenv("PROJR_LAST_NAME"))) {
     nm_last <- strsplit(Sys.getenv("PROJR_LAST_NAME"), ";")[[1]]
     answer_last <- menu(
       c(nm_last, "Specify other", "Complete later"),
       title = "Please select your surname (last/family name)."
     )
     cat("\n")
     if (answer_last %in% seq_along(nm_last)) {
       nm_last <- nm_last[answer_last]
       ask_last <- FALSE
       completed_last <- TRUE
     } else if (answer_last == length(nm_last) + 1) {
       cat("Please provide your surname.\n") # nolint
       nm_last <- readline(prompt = ">> ")
       cat("\n")
       ask_last <- TRUE
     } else {
       ask_last <- FALSE
       completed_last <- FALSE
     }
   } else {
     cat("Please provide your surname (last/family name).\n")
     nm_last <- readline(prompt = ">> ")
     cat("\n")
     ask_last <- TRUE
   }

   if (ask_last) {
     answer_last <- menu(
       c("Yes", "No", "Complete later"),
       title = paste0("Is the surname `", nm_last, "` correct?")
     )
     cat("\n")
     ask_last <- answer_last == 2
     completed_last <- answer_last != 3
   }

   while (ask_last) {
     cat("Please provide your surname.\n")
     nm_last <- readline(prompt = ">> ")
     answer_last <- menu(
       c("Yes", "No", "Complete later"),
       title = paste0("Is the surname `", nm_last, "` correct?")
     )
     cat("\n")
     ask_last <- answer_last == 2
     completed_last <- answer_last != 3
   }

   if (!completed_last) {
     nm_last <- "LAST_NAME"
   }

   # email
   if (nzchar(Sys.getenv("PROJR_EMAIL"))) {
     nm_email <- strsplit(Sys.getenv("PROJR_EMAIL"), ";")[[1]]
     answer_email <- menu(
       c(nm_email, "Specify other", "Complete later"),
       title = "Please select your email address."
     )
     cat("\n")
     if (answer_email %in% seq_along(nm_email)) {
       nm_email <- nm_email[answer_email]
       ask_email <- FALSE
       completed_email <- TRUE
     } else if (answer_email == length(nm_email) + 1) {
       cat("Please provide your email address.\n") # nolint
       nm_email <- readline(prompt = ">> ")
       cat("\n")
       ask_email <- TRUE
     } else {
       ask_email <- FALSE
       completed_email <- FALSE
     }
   } else {
     cat("Please provide your email address.\n")
     nm_email <- readline(prompt = ">> ")
     cat("\n")
     ask_email <- TRUE
   }

   if (ask_email) {
     answer_email <- menu(
       c("Yes", "No", "Complete later"),
       title = paste0("Is the email address `", nm_email, "` correct?")
     )
     cat("\n")
     ask_email <- answer_email == 2
     completed_email <- answer_email != 3
   }

   while (ask_email) {
     cat("Please provide your email address.\n")
     nm_email <- readline(prompt = ">> ")
     answer_email <- menu(
       c("Yes", "No", "Complete later"),
       title = paste0("Is the email address `", nm_email, "` correct?")
     )
     cat("\n")
     ask_email <- answer_email == 2
     completed_email <- answer_email != 3
   }

   if (!completed_email) {
     nm_email <- "USER@DOMAIN.COM"
   }

   # project title
   cat(
     "Please provide a short project title (<30 characters, initial capital and no full stop).\n" # nolint
   ) # nolint
   nm_title <- readline(prompt = ">> ")
   answer_title <- menu(
     c("Yes", "No", "Complete later"),
     title = paste0("Is the project title `", nm_title, "` correct?")
   )
   while (answer_title == 2) {
     cat("Please provide a short project title (<30 characters, initial capital and no full stop).\n") # nolint
     nm_title <- readline(prompt = ">> ")
     answer_title <- menu(
       c("Yes", "No", "Complete later"),
       title = paste0("Is the project title `", nm_title, "` correct?")
     )
   }
   if (answer_title == 3) {
     nm_title <- "PROJECT_TITLE"
   }

   # project title
   cat(
     "Please provide a sentence or two describing the project (initial capital and a full stop).\n" # nolint
   )
   nm_desc <- readline(prompt = ">> ")
   answer_desc <- menu(
     c("Yes", "No", "Complete later"),
     title =
       paste0(
         "Is the following project description correct:\n",
         nm_desc
       )
   )
   while (answer_desc == 2) {
     cat(
       "Please provide a sentence or two describing the project (initial capital and a full stop).\n" # nolint
     )
     nm_desc <- readline(prompt = ">> ")
     answer_desc <- menu(
       c("Yes", "No", "Complete later"),
       title =
         paste0(
           "Is the following project description correct:\n",
           nm_desc,
           "`"
         )
     )
   }
   if (answer_desc == 3) {
     nm_desc <- "DESCRIPTION"
   }

#############
# HASH STUF
#############

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
