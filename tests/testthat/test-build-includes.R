test_that("output includes collect rendered docs and nested outputs before archiving", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())
  project <- .test_setup_project(git = FALSE, rm_engine = TRUE)
  usethis::with_project(project,
    {
      yml <- .yml_get("default")
      yml$directories[["output-bundle"]] <- list(path = "bundle", include = c("docs", "output-tables"))
      yml$directories[["output-tables"]] <- list(path = "tables", include = "raw-data", license = "CC0")
      # The legacy spelling and the receiver request refer to the same copy.
      yml$directories[["raw-data"]]$output <- "output-tables"
      yml$directories$docs <- list(path = "docs")
      yml$build$scripts <- c("analysis.R", "report.Rmd")
      .yml_set(yml, "default")
      .yml_dest_rm_type_all("default")
      remote <- withr::local_tempdir()
      projr_yml_dest_add_local("bundle", "output-bundle", remote, structure = "archive")
      writeLines("input", projr_path_get("raw-data", ".sample"))
      writeLines("writeLines('table', projr::projr_path_get('output-tables', '.table'))", "analysis.R")
      writeLines(c("---", "title: Report", "output: html_document", "---", "Report text."), "report.Rmd")

      projr_build_dev()
      files <- c("docs/report.html", "output-tables/.table", "output-tables/raw-data/.sample")
      dev <- projr_path_get_dir("output-bundle", safe = TRUE, create = FALSE)
      expect_true(all(file.exists(file.path(dev, files))))
      expect_false(dir.exists("bundle"))
      expect_false(dir.exists("tables"))

      projr_build_patch(msg = "Archive the bundle")
      files <- c(files, "output-tables/LICENSE")
      expect_true(all(file.exists(file.path("bundle", files))))
      manifest <- .manifest_read_project("output-bundle")
      expect_true(all(files %in% manifest$fn))
      archived <- file.path(remote, "output-bundle", unique(manifest$version))
      expect_true(all(file.exists(file.path(archived, files))))
      expect_identical(readLines(file.path(archived, "output-tables/raw-data/.sample")), "input")

      # Refresh only the reserved subdirectory when keeping other output files.
      writeLines("keep", "bundle/keep.txt")
      unlink(projr_path_get("raw-data", ".sample"))
      projr_build_patch(msg = "Refresh the bundle", clear_output = "never")
      expect_false(file.exists("bundle/output-tables/raw-data/.sample"))
      expect_identical(readLines("bundle/keep.txt"), "keep")
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that("include checks reject undeclared labels, cycles and unsafe paths before cleaning", {
  skip_if(.is_test_select())
  project <- .test_setup_project(git = FALSE, rm_engine = TRUE)
  usethis::with_project(project,
    {
      yml <- .yml_get("default")
      yml$directories$output$include <- "output-bundle"
      yml$directories[["output-bundle"]] <- list(path = "bundle", include = "output")
      .yml_set(yml, "default")
      expect_error(projr_yml_check(), "Cycle")

      yml$directories[["output-bundle"]]$include <- NULL
      for (source in list("raw-absent", "cache", TRUE, list(raw = "raw-data"))) {
        yml$directories$output$include <- source
        .yml_set(yml, "default")
        expect_error(projr_yml_check(), "declared raw, docs or output")
      }
      yml$directories$output$include <- "raw-data"
      yml$directories[["raw-data"]]$include <- "output"
      .yml_set(yml, "default")
      expect_error(projr_yml_check(), "Only output directories")
      yml$directories[["raw-data"]]$include <- NULL
      yml$directories[["raw-data"]]$path <- file.path(yml$directories$output$path, "original")
      .yml_set(yml, "default")
      input <- projr_path_get("raw-data", "original.txt")
      writeLines("keep", input)
      expect_error(projr_build_patch(msg = "Bad layout"), "contains a raw directory")
      expect_identical(readLines(input), "keep")

      yml$directories[["raw-data"]]$path <- "_input"
      .yml_set(yml, "default")
      occupied <- projr_path_get("output", "raw-data", safe = FALSE)
      writeLines("keep", occupied)
      expect_error(.build_include_check_config(), "file or link conflicts")
      expect_identical(readLines(occupied), "keep")
      unlink(occupied)
      if (.Platform$OS.type != "windows") {
        fs::link_create(dirname(input), occupied)
        expect_error(.build_include_check_config(), "file or link conflicts")
        expect_identical(readLines(input), "keep")
        fs::link_delete(occupied)
      }

      # The staged subdirectory belongs to a script. Keep its data on failure.
      staged <- projr_path_get("output", "raw-data", "own.csv")
      writeLines("own data", staged)
      expect_error(.build_copy(FALSE, NULL, list(), engines = list()), "Rendered output conflicts")
      expect_identical(readLines(staged), "own data")
    },
    quiet = TRUE,
    force = TRUE
  )
})
