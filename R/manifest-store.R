# Each label has one version index and immutable, content-addressed file lists.
.manifest_store_ready <- function(path_dir) {
  marker <- file.path(path_dir, "FORMAT")
  if (!file.exists(marker)) {
    return(FALSE)
  }
  format <- readLines(marker, warn = FALSE)
  if (identical(format, "importing")) return(FALSE)
  if (!identical(format, "1")) {
    stop("Unknown manifest format in ", marker, call. = FALSE)
  }
  TRUE
}

.manifest_store_empty_index <- function() {
  data.frame(
    label = character(), version = character(),
    directory_version = character(), snapshot = character()
  )
}

.manifest_store_read_index <- function(path) {
  index <- .manifest_store_read_csv(path, names(.manifest_store_empty_index()))
  if (!nrow(index)) {
    return(index)
  }
  labels <- unique(index$label)
  if (length(labels) != 1L || !nzchar(labels) ||
    .path_encode_component(labels) != basename(dirname(path)) ||
    anyDuplicated(index$version) || any(!nzchar(index$version)) ||
    any(!grepl("^[a-f0-9]{64}$", index$snapshot))) {
    stop("Invalid directory manifest index in ", path, call. = FALSE)
  }
  expected <- .manifest_store_versions(index)
  if (!identical(index$directory_version, expected$directory_version[match(index$version, expected$version)])) {
    stop("Invalid directory versions in ", path, call. = FALSE)
  }
  index
}

.manifest_store_versions <- function(index) {
  ordered <- order(numeric_version(sub("^v", "", index$version)), index$version, method = "radix")
  index <- index[ordered, , drop = FALSE]
  index$directory_version <- index$version[match(index$snapshot, index$snapshot)]
  rownames(index) <- NULL
  index
}

.manifest_store_index <- function(path_dir = .manifest_project_dir(), label = NULL) {
  if (!.manifest_store_ready(path_dir)) {
    return(.manifest_store_empty_index())
  }
  # Collect the indexes from one completed write. Their immutable file lists
  # remain readable after we release the lock, including during later builds.
  path_lock <- file.path(path_dir, ".versions.lock")
  if (file.exists(path_lock) && file.access(path_lock, 2L) == 0L) {
    lock <- filelock::lock(path_lock, exclusive = FALSE)
    on.exit(filelock::unlock(lock))
  }
  directories <- if (is.null(label)) {
    list.dirs(path_dir, recursive = FALSE, full.names = TRUE)
  } else {
    file.path(path_dir, vapply(unique(label), .path_encode_component, character(1)))
  }
  paths <- file.path(directories, "versions.csv")
  paths <- paths[file.exists(paths)]
  if (!length(paths)) {
    return(.manifest_store_empty_index())
  }
  index <- do.call(rbind, lapply(paths, .manifest_store_read_index))
  rownames(index) <- NULL
  index
}

.manifest_store_read <- function(path_dir = .manifest_project_dir(),
                                 label = NULL, version = NULL, fn = NULL) {
  index <- .manifest_store_index(path_dir, label)
  if (!is.null(version)) index <- index[index$version %in% version, , drop = FALSE]
  .manifest_store_expand(index, path_dir, fn)
}

.manifest_store_expand <- function(index, path_dir, fn = NULL) {
  if (!nrow(index)) {
    return(.zero_tbl_get_manifest())
  }
  directories <- vapply(unique(index$label), .path_encode_component, character(1))
  paths <- file.path(path_dir, directories[index$label], paste0("files-", index$snapshot, ".csv"))
  unique_paths <- unique(paths)
  snapshots <- lapply(unique_paths, function(path) {
    if (!file.exists(path)) stop("Missing manifest snapshot ", path, call. = FALSE)
    expected <- sub("^files-(.+)\\.csv$", "\\1", basename(path))
    if (digest::digest(file = path, algo = "sha256") != expected) {
      stop("Manifest snapshot checksum mismatch in ", path, call. = FALSE)
    }
    files <- .manifest_store_read_csv(path, c("fn", "hash"))
    if (!is.null(fn)) files <- files[files$fn %in% fn, , drop = FALSE]
    files
  })
  by_build <- match(paths, unique_paths)
  rows <- lapply(seq_len(nrow(index)), function(i) {
    files <- snapshots[[by_build[i]]]
    if (!nrow(files)) {
      if (!is.null(fn)) return(.zero_tbl_get_manifest())
      return(.empty_tbl_get_manifest(index$label[i], index$version[i]))
    }
    data.frame(label = index$label[i], fn = files$fn, version = index$version[i], hash = files$hash)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

.manifest_store_read_csv <- function(path, columns) {
  rows <- utils::read.csv(path,
    colClasses = "character", na.strings = character(),
    stringsAsFactors = FALSE, encoding = "UTF-8"
  )
  if (!identical(names(rows), columns)) {
    stop("Invalid manifest columns in ", path, call. = FALSE)
  }
  rows
}

# Quote every field and emit UTF-8 bytes with LF endings on every platform.
# The SHA-256 in an index is the hash of those exact CSV bytes.
.manifest_store_write_csv <- function(rows, path) {
  quote_field <- function(x) paste0('"', gsub('"', '""', enc2utf8(x), fixed = TRUE), '"')
  con <- file(path, "wb")
  on.exit(close(con))
  writeLines(paste(quote_field(names(rows)), collapse = ","), con, useBytes = TRUE)
  if (nrow(rows)) {
    lines <- do.call(paste, c(lapply(rows, quote_field), sep = ","))
    writeLines(lines, con, useBytes = TRUE)
  }
}

.manifest_store_write_files <- function(files, path) {
  fn <- enc2utf8(files$fn)
  hash <- enc2utf8(files$hash)
  indices <- manifest_file_order(fn, hash)
  con <- file(path, "wb")
  on.exit(close(con))
  writeBin(charToRaw('"fn","hash"\n'), con)
  if (!length(indices)) return(invisible(path))
  for (start in seq.int(1, length(indices), by = 8192)) {
    batch <- indices[seq.int(start, min(start + 8191, length(indices)))]
    writeBin(manifest_file_bytes(fn, hash, batch), con)
  }
  invisible(path)
}

.manifest_store_write <- function(manifest, path_dir = .path_get(".projr", "manifests"),
                                  legacy_path = .path_get("manifest.csv")) {
  fs::dir_create(path_dir)
  lock <- filelock::lock(file.path(path_dir, ".versions.lock"))
  on.exit(filelock::unlock(lock), add = TRUE)
  ready <- .manifest_store_ready(path_dir)

  batches <- list(manifest)
  if (!ready && !is.null(legacy_path) && file.exists(legacy_path)) {
    batches <- list(.manifest_read(legacy_path), manifest)
  }
  tmp <- tempfile(".manifest-", tmpdir = path_dir)
  on.exit(unlink(tmp), add = TRUE)
  marker <- file.path(path_dir, "FORMAT")
  if (!ready) {
    indexes <- file.path(list.dirs(path_dir, recursive = FALSE), "versions.csv")
    indexes <- indexes[file.exists(indexes) | fs::link_exists(indexes)]
    if (file.exists(marker)) {
      # The importing marker identifies provisional indexes from a failed run.
      fs::file_delete(indexes)
    } else {
      if (length(indexes)) {
        stop("Cannot initialize manifest store with an existing index: ", indexes[1L], call. = FALSE)
      }
      writeLines("importing", tmp)
      fs::file_move(tmp, marker)
    }
  }
  for (rows in batches) {
    columns <- names(.zero_tbl_get_manifest())
    if (!all(columns %in% names(rows))) stop("Missing manifest columns.", call. = FALSE)
    if (!nrow(rows)) next
    rows <- rows[columns]
    rows[] <- lapply(rows, as.character)
    if (anyNA(rows) || any(!nzchar(rows$label)) || any(!nzchar(rows$version))) {
      stop("Missing label, version, or file metadata in manifest.", call. = FALSE)
    }
    numeric_version(sub("^v", "", unique(rows$version))) # Validate before publishing any index.
    for (indices in split(seq_len(nrow(rows)), rows$label)) {
      directory <- file.path(path_dir, .path_encode_component(rows$label[indices[1L]]))
      fs::dir_create(directory)
      .manifest_store_write_label(rows[indices, , drop = FALSE], directory, tmp)
    }
  }
  # Git checkouts must preserve the bytes named by each snapshot's checksum.
  path_attributes <- file.path(path_dir, ".gitattributes")
  rule <- "files-*.csv -text"
  attributes <- if (file.exists(path_attributes)) readLines(path_attributes, warn = FALSE)
  if (!rule %in% attributes) {
    writeLines(c(attributes, rule), tmp)
    fs::file_move(tmp, path_attributes)
  }
  if (!ready) {
    writeLines("1", tmp)
    fs::file_move(tmp, marker)
  }
  invisible(path_dir)
}

.manifest_store_write_label <- function(rows, directory, tmp) {
  path <- file.path(directory, "versions.csv")
  old <- if (file.exists(path)) .manifest_store_read_index(path) else .manifest_store_empty_index()
  groups <- split(seq_len(nrow(rows)), factor(rows$version, levels = unique(rows$version)))
  snapshots <- vapply(groups, function(indices) {
    files <- rows[indices, c("fn", "hash"), drop = FALSE]
    # projr's historical empty directory is one row with blank fn and hash.
    if (all(files$fn == "" & files$hash == "")) files <- files[FALSE, ]
    .manifest_store_write_files(files, tmp)
    id <- digest::digest(file = tmp, algo = "sha256")
    target <- file.path(directory, paste0("files-", id, ".csv"))
    if (!file.exists(target)) fs::file_move(tmp, target)
    id
  }, character(1))
  incoming <- data.frame(
    label = rows$label[1L], version = names(snapshots),
    directory_version = "", snapshot = unname(snapshots)
  )
  index <- rbind(old[!old$version %in% incoming$version, , drop = FALSE], incoming)
  index <- .manifest_store_versions(index)
  if (!identical(index, old)) {
    .manifest_store_write_csv(index, tmp)
    fs::file_move(tmp, path)
  }
  invisible(path)
}
