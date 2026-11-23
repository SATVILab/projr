.hash_dir <- function(
  path_dir,
  version = NULL,
  dir_exc = NULL,
  engine = .yml_settings_hash_engine(),
  cores = .yml_settings_hash_cores()
) {
  .assert_string(path_dir, TRUE)
  .assert_dir_exists(path_dir, TRUE)
  files <- .hash_dir_files(path_dir, dir_exc)
  if (!length(files)) {
    return(.zero_tbl_get_hash())
  }
  out_tbl <- data.frame(
    fn = files,
    version = .version_v_add(version %||% .version_get()),
    hash = .hash_file(file.path(path_dir, files), engine, cores)
  )
  rownames(out_tbl) <- NULL
  out_tbl
}

.hash_dir_files <- function(path_dir, dir_exc) {
  if (is.null(dir_exc)) {
    files <- list.files(path_dir, recursive = TRUE, all.files = TRUE)
  } else {
    # Skip excluded top-level trees (especially the build cache) before descending.
    entries <- list.files(path_dir, all.files = TRUE, no.. = TRUE)
    entries <- .path_filter_spec(entries, dir_exc)
    is_dir <- dir.exists(file.path(path_dir, entries))
    nested <- lapply(entries[is_dir], function(entry) {
      files <- list.files(
        file.path(path_dir, entry),
        recursive = TRUE,
        all.files = TRUE
      )
      if (length(files)) file.path(entry, files)
    })
    files <- sort(c(entries[!is_dir], unlist(nested, use.names = FALSE)))
    # Exclusions can also refer to a path beneath one of the retained directories.
    if (length(files)) files <- .path_filter_spec(files, dir_exc)
  }

  # Recursive list.files omits directories but can include dangling symlinks.
  files[file.exists(file.path(path_dir, files))]
}

.hash_file <- function(
  fn,
  engine = .yml_settings_hash_engine(),
  cores = .yml_settings_hash_cores()
) {
  if (is.null(fn)) {
    return(character())
  }
  workers <- min(cores, length(fn))
  hashes <- if (workers <= 1L) {
    .hash_file_batch(fn, engine)
  } else {
    chunks <- lapply(parallel::splitIndices(length(fn), workers), function(i) {
      fn[i]
    })
    if (.Platform$OS.type == "windows") {
      pool <- parallel::makePSOCKcluster(workers)
      on.exit(parallel::stopCluster(pool), add = TRUE)
      parallel::clusterCall(pool, setwd, getwd())
      parallel::clusterCall(pool, .libPaths, .libPaths())
      # Use the exact loaded DLL, including when pkgload is running from source.
      worker <- .hash_file_batch
      environment(worker) <- baseenv()
      dll <- getLoadedDLLs()[["projr"]][["path"]]
      parts <- parallel::parLapply(pool, chunks, worker, engine, dll)
    } else {
      parts <- parallel::mclapply(
        chunks,
        function(paths) {
          tryCatch(.hash_file_batch(paths, engine), error = identity)
        },
        mc.cores = workers,
        mc.set.seed = FALSE
      )
      for (part in parts) {
        if (inherits(part, "error")) {
          stop(part)
        }
        if (!is.character(part) || inherits(part, "try-error")) {
          stop(
            "A file hashing worker exited without returning hashes.",
            call. = FALSE
          )
        }
      }
    }
    unlist(parts, use.names = FALSE)
  }
  names(hashes) <- names(fn) %||% fn
  hashes
}

.hash_file_batch <- function(fn, engine, dll = NULL) {
  switch(
    engine,
    cpp = {
      paths <- path.expand(fn)
      if (is.null(dll)) {
        return(hash_files_cpp(paths))
      }
      routine <- getDLLRegisteredRoutines(dyn.load(dll))$.Call[[
        "_projr_hash_files_cpp"
      ]]
      do.call(.Call, list(routine, paths))
    },
    legacy = vapply(
      fn,
      digest::digest,
      character(1),
      algo = "md5",
      file = TRUE,
      serialize = FALSE
    ),
    stop("Unknown hash engine: ", engine, call. = FALSE)
  )
}

.zero_tbl_get_hash <- function() {
  out_df <- data.frame(
    fn = character(0),
    version = character(0),
    hash = character(0)
  )
  rownames(out_df) <- NULL
  out_df
}
