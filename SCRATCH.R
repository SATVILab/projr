library(testthat)
devtools::load_all(path = file.path(Sys.getenv("pkg"), "projr"))

debugonce(projr:::.projr_pb_upload)
Sys.setenv("PROJR_TEST" = "TRUE")
library(testthat)
projr::projr_build_output() |>

# new new old archive
# ====================


            # so now, this is the confusing one as archive is character.
            # we specifically want to archive to particular directories, I guess
            # (over and above any archiving done via output to other directories).
            # so we do this only if we do not archive to *these* directories.

            # clearly this does not happen if output is not specified
            if (!output_present) {
              continue_archive <- TRUE
              # output is now logical.
              # so, we will not archive to these directories
              # indirectly if output is FALSE.
              # We will archive to these directories if
              # output is TRUE and archive is TRUE for any (though in this case
              # we will not archive to all; if you want to archive to all, you
              # need to specify specifically where you want to archive).
              # actually, no, we need to make `archive: TRUE` behave 
              # like `output: TRUE` and archive to every output directory.
              # we just don't if all the outputs also do.
            } else if (output_logical) {
              if (!all(output_val)) {
                continue_archive <- TRUE
              } else {
                # saved to all outputs, so check if any are
                # archived
                output_key_ind <- grepl(
                  "^output",
                  .projr_dir_label_strip(names(yml_projr_dir))
                )
                output_key_vec <- names(yml_projr_dir)[output_key_ind]
                archived_via_output_vec <- vapply(
                  output_key_vec, function(x) {
                  yml_projr_dir_output <- yml_projr_dir[[x]]
                  if (all(is.logical(yml_projr_dir_output[["archive"]]))) {
                    return(all(yml_projr_dir_output[["archive"]]))
                  }
                  TRUE
                }, logical(1))
                continue_archive <- !any(archived_via_output_vec)
              }
          }
        output_false <- "archive" %in% names(yml_projr_dir[[label]])
        if (!output_specified_ind || ()) {
          # archive is specified and is logical, continue only
          # if archive is TRUE
          if (all(is.logical(yml_projr_dir[[label]][["archive"]]))) {
            continue_archive <- all(yml_projr_dir[[label]][["archive"]])
            # archive character
          } else if (all(is.character(yml_projr_dir[[label]][["archive"]]))) {
            continue_archive <- TRUE
            # archive not specified, don't archive
          } else {
            continue_archive <- FALSE
          }
          # output is specified
        } else {
          # - output is FALSE and archive is specified
          if (all(is.logical(yml_projr_dir[[label]][["output"]]))) {
            if (all(!yml_projr_dir[[label]][["output"]])) {
              continue_archive_output_false <- TRUE
            } else {
              continue_ <- FALSE
            }
            # - output is character, and none of those outputs
            # correspond to this archive
          } else if (all(is.character(yml_projr_dir[[label]][["output"]]))) {
            continue_archive_output_archive_else <- !(
              label %in% yml_projr_dir[[label]][["output"]]
            )
          }
          continue_archive <- continue_archive_output_missing ||
            continue_archive_output_false ||
            continue_archive_output_archive_else
        }
      }
      if (!continue_archive) {
        next
      }
  
# new old archive
# =====================

    yml_projr_dir_output <- yml_projr_dir[[x]]
    # archive not specified, it will be
    # archived there
    if (!"archive" %in% names(yml_projr_dir_output)) {
      return(TRUE)
    }
    # archive is logical, so will be archived
    # there
    if (all(is.logical(yml_projr_dir_output[["archive"]]))) {
      return(all(yml_projr_dir_output[["archive"]]))
    }
    # archive is character, so will be archived somewhere
    if (all(is.character(yml_projr_dir_output[["archive"]]))) {
      return(TRUE)
    }

# old archive
# =====================

test_that("projr_build_copy_dir works when archiving", {
  dir_test <- file.path(tempdir(), paste0("report"))
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  if (!dir.exists(dir_test)) dir.create(dir_test)
  Sys.setenv("PROJR_TEST" = "TRUE")

  gitignore <- c(
    "# R", ".Rproj.user", ".Rhistory", ".RData",
    ".Ruserdata", "", "# docs", "docs/*"
  )
  writeLines(gitignore, file.path(dir_test, ".gitignore"))

  rbuildignore <- c("^.*\\.Rproj$", "^\\.Rproj\\.user$", "^docs$")
  writeLines(rbuildignore, file.path(dir_test, ".Rbuildignore"))
  usethis::with_project(
    path = dir_test,
    code = {
      projr_init()
      yml_projr_init <- .projr_yml_get_root_full()
      # do nothing when not output
      expect_false(.projr_build_archive(output_run = FALSE))
      version_run_on_list <- .projr_version_run_onwards_get(
        bump_component = "patch"
      )
      # do nothing when nothing saved
      expect_false(.projr_build_archive(
        output_run = FALSE,
        version_run_on_list = version_run_on_list
      ))

      # save files to output
      invisible({
        file.create(
          projr_path_get("output", "a.txt", output_safe = FALSE)
        )
        file.create(
          projr_path_get("output", "b.txt", output_safe = FALSE)
        )
        file.create(
          projr_path_get("output", "dir_c", "c.txt", output_safe = FALSE)
        )
        file.create(
          projr_path_get("output", "dir_d", "d.txt", output_safe = FALSE)
        )
      })

      .projr_build_archive(
        output_run = TRUE,
        version_run_on_list = version_run_on_list
      )

      dir_archive <- projr_dir_get(
        label = "archive",
        paste0("v", version_run_on_list$desc[["success"]])
      )
      expect_true(file.exists(file.path(dir_archive, "a.txt")))
      expect_true(file.exists(file.path(dir_archive, "b.txt")))
      expect_true(file.exists(file.path(dir_archive, "dir_c.zip")))
      expect_true(file.exists(file.path(dir_archive, "dir_d.zip")))
      expect_false(dir.exists(file.path(dir_archive, "dir_c")))
      expect_false(dir.exists(file.path(dir_archive, "dir_d")))
    },
    quiet = TRUE,
    force = TRUE
  )
  Sys.unsetenv("PROJR_TEST")
  unlink(dir_test, recursive = TRUE)
})


.projr_build_archive <- function(output_run, version_run_on_list) {
  # How is this supposed to work?

  # consider not archiving
  if (!output_run) {
    return(invisible(FALSE))
  }

  # what do we archive, and to where?
  # well, if we consider data-raw and cache and output,
  # then:
  dir_proj <- rprojroot::is_r_package$find_file()

  # set up paths
  dir_output <- projr_dir_get(label = "output", output_safe = FALSE)
  dir_archive <- projr_dir_get(
    label = "archive",
    paste0("v", version_run_on_list$desc[["success"]])
  )
  if (!fs::is_absolute_path(dir_output)) {
    dir_output <- file.path(dir_proj, dir_output)
  }
  if (!fs::is_absolute_path(dir_archive)) {
    dir_archive <- file.path(dir_proj, dir_archive)
  }

  # check if there is anything to copy
  fn_vec <- list.files(
    dir_output,
    recursive = FALSE, all.files = TRUE, full.names = TRUE
  )
  if (length(fn_vec) == 0) {
    return(invisible(FALSE))
  }

  # copy individual files across
  fn_vec_fn <- fn_vec[fs::is_file(fn_vec)]
  if (length(fn_vec_fn) > 0) {
    file.copy(
      from = fn_vec_fn,
      to = file.path(dir_archive, basename(fn_vec_fn))
    )
  }

  # zip and copy directories across
  dir_vec <- list.dirs(dir_output, recursive = FALSE, full.names = TRUE)
  for (i in seq_along(dir_vec)) {
    path_dir <- dir_vec[i]
    path_zip <- file.path(dir_archive, paste0(basename(path_dir), ".zip"))
    .projr_zip_dir(
      path_dir = path_dir,
      path_zip = path_zip
    )
  }

  invisible(TRUE)
}

# old docs projr_dir_get settings
# =====================
  if (label == "docs") {
    dir_proj <- rprojroot::is_r_package$find_file()

    if (file.exists(file.path(dir_proj, "_bookdown.yml"))) {
      yml_bd <- .projr_yml_bd_get()
      dir_base <- dir_active[["docs"]][["path"]]
      fn <- basename(yml_bd[["output_dir"]])
      path_bd <- file.path(dir_base, fn)
      yml_bd[["output_dir"]] <- path_bd
      .projr_yml_bd_set(yml_bd)
      path_final <- file.path(dir_base, fn, ...)
    } else {
      # quarto stuff
    }
    if (!fs::is_absolute_path(path_final)) {
      path_final <- fs::path_rel(
        file.path(rprojroot::is_r_package$find_file(), path_final),
        start = getwd()
      ) |>
        as.character()
    }
    if (!dir.exists(path_final)) {
      dir.create(path_final, recursive = TRUE)
    }
    return(as.character(path_final))
  }

# Archiving stuf
# =================

    # archive package (to all archives)
    dir_vec_match <- tolower(
      gsub("_", "", gsub("-", "", names(yml_projr[["directories"]])))
    )
    archive_vec_ind <- which(grepl("^archive", dir_vec_match))
    archive_vec <- names(yml_projr[["directories"]])[archive_vec_ind]
    for (x in archive_vec) {
      file.copy(
        from = path_pkg,
        to = projr_path_get(x, fn_pkg, output_safe = !output_run)
      )
    }


# QUITE DEVELOPED PIGGYBACK STUFF
# ====================================

version_format_list <- .projr_version_format_list_get()
bump_component <- "minor"
version_current <- "0.1.0"

yml_projr_dir <- projr_yml_get()[["directories"]]
  if (output_run && "github-release" %in% names(yml_projr[["build-output"]])) {
    # GitHub releases settings
    gh_list_settings <- yml_projr[["build-output"]][["github-release"]]
    # version information
    version_comp_vec <- version_format_list[["components"]] |>
        setdiff("dev")
    version_comp_vec <- rev(version_comp_vec)[
        seq_len(which(rev(version_comp_vec == item_version)))
    ]
    version_current <- projr_version_get()
    # ensure `piggyback` is available
    if (!requireNamespace("piggyback", quietly = TRUE)) {
        renv::install("piggyback")
    }
    # current releases
    gh_tbl_releases <- suppressWarnings(suppressMessages(
      piggyback::pb_releases()
      ))
    .projr_pb_upload_code(
      gh_tbl_release = gh_tbl_release,
      bump_component = bump_component
    )
    gh_list <- gh_list[-which(names(gh_list) == "source-code")]
    for (i in seq_along(gh_list)) {
        gh_tbl_releases <- piggyback::pb_releases()

        # =============================
        # sorting out tag and body
        # =============================

        label <- names(gh_list)[i]
        if (!item_list[["add"]]) {
            next
        }
        tag <- switch(item_list[["name"]],
            "@version" = paste0("v", version_current),
            item_list[["name"]]
        )
        body <- switch(item_list[["name"]],
            "@version" = paste0(
              "Version-linked source code, project inputs and/or outputs"
              ),
            "Project source code, inputs and/or outputs"
        )

        # ============================
        # uploading report
        # ============================

        if (label == "bookdown") {
            dir_bookdown <- projr_dir_get("bookdown")
            path_zip <- file.path(dirname(dir_bookdown), "doc.zip")
            if (file.exists(path_zip)) {
              file.remove(path_zip)
            }
            if (!dir.exists(dirname(path_zip))) {
              dir.create(dirname(path_zip))
            }
            setwd(dir_bookdown)
            path_zip <- paste0(basename(dir_bookdown), ".zip")
            utils::zip(
                path_zip,
                files = list.files(
                    getwd(),
                    recursive = TRUE, full.names = FALSE, all.files = TRUE
                ),
                flags = "-r9Xq"
            )
            setwd(dir_proj)
            if (!tag %in% gh_tbl_releases[["release_name"]]) {
                piggyback::pb_release_create(tag = tag, body = body)
                piggyback::pb_upload(
                    file = file.path(dir_bookdown, path_zip),
                    overwrite = TRUE, tag = tag
                )
            } else {
                piggyback::pb_upload(
                    file = file.path(dir_bookdown, path_zip),
                    overwrite = TRUE, tag = tag
                )
            }
            next
        }

        # ======================================
        # uploading non-report and non-source-code items
        # ======================================

        dir_input <- projr_dir_get("label", output_safe = FALSE)
        if (!fs::is_absolute_path(dir_input)) {
            dir_input <- file.path(dir_proj, dir_input)
        }
        fn_vec <- list.files(
            dir_input,
            recursive = TRUE, all.files = TRUE,
            full.names = TRUE
        )

        # removing other items from output directory if present
        # -----------------------------------------------

        if (label == "output") {
            nm_rem_vec <- names(yml_projr_dir)
            nm_rem_vec <- nm_rem_vec[!grepl("^archive|^output$|^bookdown$", nm_rem_vec)]
            for (i in seq_along(nm_rem_vec)) {
                fn_vec <- fn_vec[
                    !basename(fn_vec) %in% paste0(nm_rem_vec, ".zip")
                ]
            }
            dir_bookdown <- projr_dir_get("bookdown")
            fn_vec <- fn_vec[
                !basename(fn_vec) == paste0(basename(dir_bookdown), ".zip")
            ]
        }

        # =======================================
        # BEGIN HERE
        # =======================================

        if (length(fn_vec) == 0) next

        if (is.null(zip_val)) {
            zip_ind <- TRUE
        } else {
            zip_ind <- zip_val
        }

        if (zip_ind) {
            setwd(dir_input)
            path_zip <- paste0(label, ".zip")
            if (file.exists(path_zip)) {
                file.remove(path_zip)
            }
            utils::zip(
                path_zip,
                files = list.files(
                    getwd(),
                    recursive = TRUE, full.names = FALSE, all.files = TRUE
                ),
                flags = "-r9Xq"
            )
            setwd(dir_proj)
            if (!tag %in% gh_tbl_releases[["release_name"]]) {
                piggyback::pb_release_create(tag = tag, body = body)
                piggyback::pb_upload(
                    file = file.path(dir_input, path_zip),
                    overwrite = TRUE,
                    tag = tag
                )
            } else {
                piggyback::pb_upload(
                    file = file.path(dir_input, path_zip),
                    overwrite = TRUE,
                    tag = tag
                )
            }
            unlink(file.path(dir_input, path_zip))
        } else {
            setwd(dir_input)
            fn_vec <- list.files(
                getwd(),
                all.files = TRUE, recursive = TRUE, full.names = FALSE,
            )
            if (!tag %in% gh_tbl_releases[["release_name"]]) {
                piggyback::pb_release_create(tag = tag, body = body)
                piggyback::pb_upload(
                    file = file.path(dir_input, path_zip), overwrite = TRUE, tag = tag
                )
            } else {
                piggyback::pb_upload(
                    file = file.path(dir_input, path_zip), overwrite = TRUE, tag = tag
                )
            }
        }
    }
}


# UNDEVELOPED PIGGYBACK STUFF
# ===================================

  if (output_run && "github-release" %in% names(yml_projr[["build-output"]])) {
    version_comp_vec <- version_format_list[["components"]] |>
      setdiff("dev")
    version_comp_vec <- version_comp_vec[
      seq_len(which(version_comp_vec == bump_component))
    ]
    gh_list <- yml_projr[["build-output"]][["github-release"]]
    for (i in seq_along(gh_list)) {
      nm <- names(gh_list)[i]
      item_list <- gh_list[[i]]
      item_version <- item_list[["version-component-min"]]
      if (item_version != "any") {
        if (!item_version %in% version_comp_vec) next
      }
      if (!exists("gh_releases_list")) {
        renv_dep_file <- readLines(
          file.path(dir_proj, "_dependencies.R")
        )
        if (!"library(piggyback)" %in% renv_dep_file) {
          renv_dep_file <- c(renv_dep_file, "library(piggyback)", "")
          writeLines(renv_dep_file, file.path(dir_proj, "_dependencies.R"))
        }
        if (!requireNamespace("piggyback", quietly = TRUE)) {
          renv::install("piggyback")
        }
        gh_releases_list <- piggyback::pb_releases()
      }
      if (nm == "source-code") {
        #  piggyback::pb_
      }
    }
  }




############
## projr_path_get
############

    version_comp_vec <- version_format_list[["components"]] |>
      setdiff("dev")
    version_comp_vec <- version_comp_vec[
      seq_len(which(version_comp_vec == bump_component))
    ]
    gh_list <- yml_projr[["build-output"]][["github-release"]]
    for (i in seq_along(gh_list)) {
      nm <- names(gh_list)[i]
      item_list <- gh_list[[i]]
      item_version <- item_list[["version-component-min"]]
      if (item_version != "any") {
        if (!item_version %in% version_comp_vec) next
      }
      if (!exists("gh_releases_list")) {
        renv_dep_file <- readLines(
          file.path(dir_proj, "_dependencies.R")
        )
        if (!"library(piggyback)" %in% renv_dep_file) {
          renv_dep_file <- c(renv_dep_file, "library(piggyback)", "")
          writeLines(renv_dep_file, file.path(dir_proj, "_dependencies.R"))
        }
        if (!requireNamespace("piggyback", quietly = TRUE)) {
          renv::install("piggyback")
        }
        gh_releases_list <- piggyback::pb_releases()
      }
      if (nm == "source-code") {
        #  piggyback::pb_
      }
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
cp$projr / report.html$w_dnld /

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

g
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


# =======================
# OLD TEST
# =======================

      yml_projr <- yml_projr_init
      yml_projr[["build-dev"]] <- yml_projr[["build-dev"]][
        -which(names(yml_projr[["build-dev"]]) == "copy-to-output")
      ]
      .projr_yml_set(yml_projr)
      expect_false(.projr_build_output(output_run = FALSE))
      .projr_yml_set(yml_projr_init)
      yml_projr <- yml_projr_init
      yml_projr[["build-dev"]] <- list(a = FALSE, b = FALSE)
      .projr_yml_set(yml_projr)
      expect_false(.projr_build_output(output_run = FALSE))
      .projr_yml_set(yml_projr_init)

      yml_projr <- projr_yml_get()
      # test copying to other directories
      if (!dir.exists("_archive")) {
        dir.create("_archive")
      }
      if (!file.exists("_archive/V0.0.1.zip")) {
        file.create("_archive/V0.0.1.zip")
      }
      dir.create("docs/reportV0.0.0-1", recursive = TRUE)
      dir.create("docs/reportV0.0.0-9000", recursive = TRUE)
      yml_projr <- .projr_yml_get()
      yml_projr[["build-output"]] <- yml_projr[["build-output"]][
        !names(yml_projr[["build-output"]]) == "github-release"
      ]
      .projr_yml_set(list_save = yml_projr)
      projr_build_output(quiet = TRUE)
      expect_true(!dir.exists("docs/reportV0.0.0-1"))
      expect_true(!dir.exists("docs/reportV0.0.0-9000"))
      yml_bd <- .projr_yml_bd_get()
      expect_identical(basename(yml_bd$output_dir), "reportV0.0.1-1")
      desc_file <- read.dcf(file.path(dir_test, "DESCRIPTION"))
      expect_identical(desc_file[1, "Version"][[1]], "0.0.1")
      expect_identical(list.files(projr_dir_get("output")), character(0))
      expect_identical(
        list.files(projr_dir_get("output", output_safe = FALSE)),
        c("VERSION - 0.0.1", "reportV0.0.1.zip")
      )

      # test copying to other directories
      if (!dir.exists("_archive")) {
        dir.create("_archive")
      }
      if (!file.exists("_archive/V0.0.1.zip")) {
        file.create("_archive/V0.0.1.zip")
      }
      yml_projr <- .projr_yml_get()
      # check that copying non-default directories works as well
      copy_list <- list(
        `data-raw` = TRUE, cache = TRUE, bookdown = FALSE, package = TRUE
      )
      yml_projr[["build-output"]][["copy-to-output"]] <- copy_list
      yml_projr[["build-output"]] <- yml_projr[["build-output"]][
        !names(yml_projr[["build-output"]]) == "github-release"
      ]
      .projr_yml_set(list_save = yml_projr)
      if (!dir.exists("_data_raw")) {
        dir.create("_data_raw")
      }
      invisible(file.create("_data_raw/test.txt"))
      if (!dir.exists("_tmp")) {
        dir.create("_tmp")
      }
      invisible(file.create("_tmp/test.txt"))
      dir_output_safe <- "_tmp/projr_output/0.0.2"
      if (!dir.exists(dir_output_safe)) {
        dir.create(dir_output_safe)
      }
      file.create(file.path(dir_output_safe, "abc.txt"))
      dir.create(file.path(dir_output_safe, "test_dir"))
      file.create(file.path(dir_output_safe, "test_dir", "def.txt"))

      invisible(file.create("_tmp/test.txt"))

      projr_build_output(quiet = TRUE)
      expect_identical(
        list.files("_tmp/projr_output/0.0.2"),
        character(0)
      )
      expect_identical(
        list.files("_output", recursive = TRUE),
        c(
          "VERSION - 0.0.2", "abc.txt", "cache.zip",
          "data-raw.zip", "report_0.0.2.tar.gz",
          "test_dir/def.txt"
        )
      )
      # test that it runs correctly when there is an error
      yml_bd <- .projr_yml_bd_get()
      writeLines(
        c("# Error", "\n", "```{r }", "\n", "stop()", "```"),
        con = "error.Rmd"
      )
      yml_bd[["rmd_files"]] <- c(yml_bd[["rmd_files"]], "error.Rmd")
      .projr_yml_bd_set(yml_bd)
      # expect_error(projr_build_output(quiet = TRUE))
      # reset after error
      yml_bd <- .projr_yml_bd_get()
      file.remove("error.Rmd")
      yml_bd[["rmd_files"]] <- c("index.Rmd", "appendix.Rmd")
      .projr_yml_bd_set(yml_bd)


# =============================
# OLD BUILD TESTING
# =============================

test_that("projr_build_output works", {
  dir_test <- file.path(tempdir(), paste0("report"))
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  if (!dir.exists(dir_test)) dir.create(dir_test)
  Sys.setenv("PROJR_TEST" = "TRUE")

  gitignore <- c(
    "# R", ".Rproj.user", ".Rhistory", ".RData",
    ".Ruserdata", "", "# docs", "docs/*"
  )
  writeLines(gitignore, file.path(dir_test, ".gitignore"))

  rbuildignore <- c("^.*\\.Rproj$", "^\\.Rproj\\.user$", "^docs$")
  writeLines(rbuildignore, file.path(dir_test, ".Rbuildignore"))
  usethis::with_project(
    path = dir_test,
    code = {
      projr_init()
      yml_projr_init <- .projr_yml_get()
      yml_bd_init <- .projr_yml_bd_get()
      expect_false(.projr_build_output(output_run = FALSE))
      yml_projr <- yml_projr_init
      yml_projr[["build-dev"]] <- yml_projr[["build-dev"]][
        -which(names(yml_projr[["build-dev"]]) == "copy-to-output")
      ]
      .projr_yml_set(yml_projr)
      expect_false(.projr_build_output(output_run = FALSE))
      .projr_yml_set(yml_projr_init)
      yml_projr <- yml_projr_init
      yml_projr[["build-dev"]] <- list(a = FALSE, b = FALSE)
      .projr_yml_set(yml_projr)
      expect_false(.projr_build_output(output_run = FALSE))
      .projr_yml_set(yml_projr_init)

      yml_projr <- projr_yml_get()
      # test copying to other directories
      if (!dir.exists("_archive")) {
        dir.create("_archive")
      }
      if (!file.exists("_archive/V0.0.1.zip")) {
        file.create("_archive/V0.0.1.zip")
      }
      dir.create("docs/reportV0.0.0-1", recursive = TRUE)
      dir.create("docs/reportV0.0.0-9000", recursive = TRUE)
      yml_projr <- .projr_yml_get()
      yml_projr[["build-output"]] <- yml_projr[["build-output"]][
        !names(yml_projr[["build-output"]]) == "github-release"
      ]
      .projr_yml_set(list_save = yml_projr)
      projr_build_output(quiet = TRUE)
      expect_true(!dir.exists("docs/reportV0.0.0-1"))
      expect_true(!dir.exists("docs/reportV0.0.0-9000"))
      yml_bd <- .projr_yml_bd_get()
      expect_identical(basename(yml_bd$output_dir), "reportV0.0.1-1")
      desc_file <- read.dcf(file.path(dir_test, "DESCRIPTION"))
      expect_identical(desc_file[1, "Version"][[1]], "0.0.1")
      expect_identical(list.files(projr_dir_get("output")), character(0))
      expect_identical(
        list.files(projr_dir_get("output", output_safe = FALSE)),
        c("VERSION - 0.0.1", "reportV0.0.1.zip")
      )

      # test copying to other directories
      if (!dir.exists("_archive")) {
        dir.create("_archive")
      }
      if (!file.exists("_archive/V0.0.1.zip")) {
        file.create("_archive/V0.0.1.zip")
      }
      yml_projr <- .projr_yml_get()
      # check that copying non-default directories works as well
      copy_list <- list(
        `data-raw` = TRUE, cache = TRUE, bookdown = FALSE, package = TRUE
      )
      yml_projr[["build-output"]][["copy-to-output"]] <- copy_list
      yml_projr[["build-output"]] <- yml_projr[["build-output"]][
        !names(yml_projr[["build-output"]]) == "github-release"
      ]
      .projr_yml_set(list_save = yml_projr)
      if (!dir.exists("_data_raw")) {
        dir.create("_data_raw")
      }
      invisible(file.create("_data_raw/test.txt"))
      if (!dir.exists("_tmp")) {
        dir.create("_tmp")
      }
      invisible(file.create("_tmp/test.txt"))
      dir_output_safe <- "_tmp/projr_output/0.0.2"
      if (!dir.exists(dir_output_safe)) {
        dir.create(dir_output_safe)
      }
      file.create(file.path(dir_output_safe, "abc.txt"))
      dir.create(file.path(dir_output_safe, "test_dir"))
      file.create(file.path(dir_output_safe, "test_dir", "def.txt"))

      invisible(file.create("_tmp/test.txt"))

      projr_build_output(quiet = TRUE)
      expect_identical(
        list.files("_tmp/projr_output/0.0.2"),
        character(0)
      )
      expect_identical(
        list.files("_output", recursive = TRUE),
        c(
          "VERSION - 0.0.2", "abc.txt", "cache.zip",
          "data-raw.zip", "report_0.0.2.tar.gz",
          "test_dir/def.txt"
        )
      )
      # test that it runs correctly when there is an error
      yml_bd <- .projr_yml_bd_get()
      writeLines(
        c("# Error", "\n", "```{r }", "\n", "stop()", "```"),
        con = "error.Rmd"
      )
      yml_bd[["rmd_files"]] <- c(yml_bd[["rmd_files"]], "error.Rmd")
      .projr_yml_bd_set(yml_bd)
      # expect_error(projr_build_output(quiet = TRUE))
      # reset after error
      yml_bd <- .projr_yml_bd_get()
      file.remove("error.Rmd")
      yml_bd[["rmd_files"]] <- c("index.Rmd", "appendix.Rmd")
      .projr_yml_bd_set(yml_bd)
    },
    quiet = TRUE,
    force = TRUE
  )
  Sys.unsetenv("PROJR_TEST")
  unlink(dir_test, recursive = TRUE)
})
