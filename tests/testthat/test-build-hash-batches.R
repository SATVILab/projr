test_that("a build shares each file hash batch across its eligible directory labels", {
  skip_if(.is_test_select())
  project <- .test_setup_project(git = FALSE, rm_engine = TRUE)
  usethis::with_project(project, {
    config <- .yml_get("default")
    config$settings <- list(hashing = list(approach = "cpp", cores = 2L))
    config$directories[["raw-alias"]] <- list(path = config$directories[["raw-data"]]$path)
    config$directories[["raw-second"]] <- list(path = "_other")
    config$directories[["raw-unused"]] <- list(path = "_empty")
    config$directories[["output-second"]] <- list(path = "_other_output")
    config$directories$cache$hash <- TRUE
    config$build$scripts <- "analysis.R"
    .yml_set(config)
    projr_version_set("1.0.0")

    writeLines("first", projr_path_get("raw-data", "input-one.txt"))
    writeLines("second", projr_path_get("raw-second", "input-two.txt"))
    writeLines("cache", projr_path_get("cache", "cache-input.txt"))
    writeLines("skip", projr_path_get("cache", "projr", "skip.txt"))
    writeLines(c(
      'writeLines("one", projr::projr_path_get("output", "result-one.txt"))',
      'writeLines("two", projr::projr_path_get("output-second", "result-two.txt"))'
    ), "analysis.R")

    original <- .hash_file
    batches <- list()
    testthat::local_mocked_bindings(.hash_file = function(fn, engine, cores) {
      batches[[length(batches) + 1L]] <<- list(files = basename(fn), engine = engine, cores = cores)
      original(fn, engine, cores)
    })
    projr_build_patch(msg = "Hash directory batches")
    expect_length(batches, 2L)
    expect_setequal(batches[[1L]]$files, c("input-one.txt", "input-two.txt", "cache-input.txt"))
    expect_equal(sum(batches[[1L]]$files == "input-one.txt"), 1L)
    expect_setequal(batches[[2L]]$files, c("result-one.txt", "result-two.txt"))
    expect_identical(vapply(batches, `[[`, character(1), "engine"), c("cpp", "cpp"))
    expect_identical(vapply(batches, `[[`, integer(1), "cores"), c(2L, 2L))

    rows <- .manifest_read_project(version = "v1.0.1")
    expect_setequal(rows$label[rows$fn == "input-one.txt"], c("raw-data", "raw-alias"))
    expect_identical(rows$fn[rows$label == "raw-unused"], "")
    expect_identical(rows$hash[rows$label == "raw-unused"], "")
    expect_identical(
      rows$hash[rows$label == "output-second"],
      digest::digest(
        file = projr_path_get("output-second", "result-two.txt", safe = FALSE),
        algo = "md5"
      )
    )
  })
})
