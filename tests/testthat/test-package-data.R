test_that("package data paths are fixed, root-relative and read-only on lookup", {
  skip_if(.is_test_select())
  project <- fs::path_real(.test_setup_project(git = FALSE, rm_engine = TRUE))
  usethis::with_project(project, {
    config <- projr_yml_get("default")
    config$directories$cache$path <- "build-cache"
    config$directories$data <- list(path = "old-location")
    .yml_set(config)
    dir.create("analysis")
    withr::with_dir("analysis", {
      expect_identical(
        as.character(projr_path_get("data", "sample.rda", safe = FALSE, create = FALSE, format = "absolute")),
        file.path(project, "data", "sample.rda")
      )
      staged <- projr_path_get("data", "sample.rda", create = FALSE, format = "absolute")
      expect_true(fs::path_has_parent(staged, file.path(project, "build-cache")))
      expect_identical(basename(dirname(staged)), "data")
    })
    expect_false(any(dir.exists(c("data", "build-cache", "old-location"))))
    unlink("DESCRIPTION")
    sample <- 1L
    projr_use_data(sample, safe = FALSE)
    expect_true(file.exists("data/sample.rda"))
    expect_false(any(file.exists(c("old-location", "DESCRIPTION"))))
  })
})

test_that("package data conflicts are checked when saving a dataset", {
  skip_if(.is_test_select())
  project <- .test_setup_project(git = FALSE, rm_engine = TRUE)
  usethis::with_project(project, {
    dir.create(".projr/config", recursive = TRUE, showWarnings = FALSE)
    profile <- ".projr/config/_projr-lab.yml"
    withr::local_envvar(c(PROJR_PROFILE = "lab"))
    value <- 1L
    for (path in c("data", "data/observations")) {
      yaml::write_yaml(list(directories = list("raw-data" = list(path = path))), profile)
      expect_true(projr_yml_check())
      expect_identical(projr_path_get_dir("data", safe = FALSE, create = FALSE), "data")
      expect_identical(projr_path_get_dir("raw-data", create = FALSE), path)
      expect_error(projr_use_data(value), "raw-data")
      expect_error(projr_use_data(value, internal = TRUE, safe = FALSE), "raw-data")
    }
    writeLines('writeLines("ran", "done.txt")', "analysis.R")
    projr_build_dev(scripts = "analysis.R", project = project)
    expect_identical(readLines("done.txt"), "ran")
    expect_false(file.exists("R/sysdata.rda"))
    yaml::write_yaml(list(directories = list(
      "raw-data" = list(path = "data-old"),
      output = list(path = "inputs/data")
    )), profile)
    expect_true(file.exists(projr_use_data(value, safe = FALSE)))
    if (.Platform$OS.type != "windows") {
      fs::link_create(file.path(project, "data"), "linked-data")
      yaml::write_yaml(list(directories = list(output = list(path = "linked-data/new"))), profile)
      expect_error(projr_use_data(value, overwrite = TRUE), "output")
    }
  })
})

test_that("dev stages package datasets and production collects them alongside curated data", {
  skip_if(.is_test_select())
  project <- .test_setup_project(git = FALSE, rm_engine = TRUE)
  usethis::with_project(project, {
    config <- projr_yml_get("default")
    config$build$scripts <- "datasets.R"
    .yml_set(config)
    dir.create("data")
    curated <- "maintained by hand"
    save(curated, file = "data/curated.rda")
    before <- tools::md5sum("data/curated.rda")
    writeLines(c(
      "sample_data <- 1:3",
      "lookup <- c(red = 1L, blue = 2L)",
      "projr::projr_use_data(sample_data)",
      "projr::projr_use_data(lookup, internal = TRUE)"
    ), "datasets.R")
    projr_build_dev(project = project)
    staged <- projr_path_get("data", "sample_data.rda", create = FALSE)
    expect_true(file.exists(staged))
    expect_true(file.exists(file.path(dirname(dirname(staged)), "R", "sysdata.rda")))
    expect_false(any(file.exists(c("data/sample_data.rda", "R/sysdata.rda"))))
    expect_identical(tools::md5sum("data/curated.rda"), before)

    projr_build_patch(msg = "Publish package datasets", clear_output = "post", project = project)
    saved <- new.env(parent = emptyenv())
    expect_identical(load("data/sample_data.rda", saved), "sample_data")
    expect_identical(saved$sample_data, 1:3)
    expect_identical(load("R/sysdata.rda", saved), "lookup")
    expect_identical(saved$lookup, c(red = 1L, blue = 2L))
    expect_identical(tools::md5sum("data/curated.rda"), before)
  })
})
