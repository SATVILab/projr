# Each build records the merged configuration used for that version, including
# its active profiles and any concrete destinations needed to resolve @uuid.
.get_build_record <- function(lines, version) {
  headers <- grep("^#{2,4} v[0-9]+([.-][0-9]+)*:", lines)
  versions <- sub("^#{2,4} (v[0-9.-]+):.*", "\\1", lines[headers])
  pos <- if (is.null(version)) 1L else match(version, versions)
  if (!length(headers) || is.na(pos)) {
    return(list(version = version))
  }
  start <- headers[pos]
  end <- c(headers[-1L] - 1L, length(lines))[pos]
  entry <- lines[start:end]
  block <- function(heading) {
    start <- match(heading, entry)
    if (is.na(start)) {
      return(NULL)
    }
    text <- utils::tail(entry, -start)
    first <- match("```yaml", text)
    if (is.na(first)) {
      return(NULL)
    }
    text <- utils::tail(text, -first)
    last <- match("```", text)
    if (is.na(last)) stop("Incomplete YAML in the build log for ", versions[pos], ".", call. = FALSE)
    yaml::yaml.load(paste(utils::head(text, last - 1L), collapse = "\n"), eval.expr = FALSE)
  }
  list(
    version = versions[pos], yml = block("**`projr` config**"),
    destinations = block("**Resolved archive destinations**")
  )
}

.get_remote_target <- function(type, title, definition, record, version, head, tag = NULL, optional = FALSE) {
  if (!is.null(tag)) {
    return(tag)
  }
  recorded <- record$destinations[[type]][[title]]$destination
  if (!is.null(recorded)) {
    return(recorded)
  }
  template <- if (type == "local") definition$path %||% "_archive" else definition$tag %||% definition$title %||% title
  if (optional) {
    tokens <- regmatches(template, gregexpr(.template_token_pattern, template, perl = TRUE))[[1L]]
    if ("@uuid" %in% tokens || ("@branch" %in% tokens && !nzchar(head %||% "")) ||
      ("@version" %in% tokens && is.null(version))) {
      return(NULL)
    }
  }
  .template_expand(template,
    version = version, branch = .template_branch(head %||% ""),
    uuid = stop("No recorded @uuid for ", title, ". Supply its concrete destination.", call. = FALSE)
  )
}

# Read metadata from one commit. An update to the producer's branch while a
# download is in progress cannot change which configuration describes the build.
.get_github_project <- function(repo, branch, version, label, workspace) {
  .dep_install_only("httr")
  url <- paste0(.github_api_base(NULL), "/repos/", repo)
  token <- .auth_get_github_pat_find()
  get <- function(endpoint, file = FALSE, ref = NULL, optional = file) {
    headers <- c(Accept = if (file) "application/vnd.github.raw+json" else "application/vnd.github+json")
    if (.is_string(token)) headers["Authorization"] <- paste("Bearer", token)
    response <- httr::GET(paste0(url, endpoint), httr::add_headers(.headers = headers),
      query = if (!is.null(ref)) list(ref = ref)
    )
    if (optional && httr::status_code(response) == 404L) {
      return(NULL)
    }
    httr::stop_for_status(response, task = paste("read", endpoint, "from", repo))
    if (file) httr::content(response, "text", encoding = "UTF-8") else httr::content(response, "parsed")
  }
  if (is.null(branch)) branch <- get("")$default_branch
  commit <- get(paste0("/commits/", utils::URLencode(branch, reserved = TRUE)))$sha
  .assert_detect_single(commit, "^[a-f0-9]{40}$", TRUE)
  read <- function(path) get(paste0("/contents/", path), file = TRUE, ref = commit)
  log <- read(".projr/BUILDLOG.md") %||% read("BUILDLOG.md") %||% ""
  record <- .get_build_record(strsplit(log, "\n", fixed = TRUE)[[1L]], version)
  config <- read("_projr.yml") %||% read(".projr/config/_projr.yml") %||% read("_projr/config/_projr.yml")
  yml <- if (is.null(config)) record$yml else yaml::yaml.load(config, eval.expr = FALSE)
  if (is.null(yml)) {
    stop("No projr configuration in ", repo, " on ", branch,
      ". Supply tag to read a release directly.",
      call. = FALSE
    )
  }
  version <- version %||% record$version
  if (is.null(version)) {
    value <- trimws(read("VERSION") %||% "")
    if (grepl("^v?[0-9]+([.][0-9]+)*$", value)) version <- .version_v_add(value)
  }
  metadata <- NULL
  if (!is.null(version)) {
    store <- file.path(workspace, "producer", "manifests")
    csv <- file.path(workspace, "producer", "manifest.csv")
    fs::dir_create(store)
    # Only fetch the indexes and immutable snapshots for the requested build.
    # A release may contain an earlier matching payload without a new index row.
    prefix <- ".projr/manifests"
    marker <- read(paste0(prefix, "/FORMAT"))
    if (is.null(marker)) {
      prefix <- "_projr/manifests"
      marker <- read(paste0(prefix, "/FORMAT"))
    }
    if (!is.null(marker)) {
      writeBin(charToRaw(marker), file.path(store, "FORMAT"))
      if (is.null(label)) {
        listing <- get(paste0("/contents/", prefix), ref = commit)
        directories <- vapply(
          Filter(function(x) identical(x$type, "dir"), listing),
          function(x) x$name, character(1)
        )
        directories <- grep("^[a-z0-9_-]+$", directories, value = TRUE)
      } else {
        directories <- vapply(label, .path_encode_component, character(1))
      }
      fetch <- function(file) {
        value <- read(paste(prefix, file, sep = "/"))
        if (!is.null(value)) {
          target <- file.path(store, file)
          fs::dir_create(dirname(target))
          writeBin(charToRaw(value), target)
        }
      }
      for (dir in directories) fetch(file.path(dir, "versions.csv"))
      index <- .manifest_store_index(store, label)
      index <- index[index$version == version, , drop = FALSE]
      if (nrow(index)) for (file in .remote_manifest_snapshots(index)) fetch(file)
    } else {
      legacy <- read("manifest.csv")
      if (!is.null(legacy)) writeBin(charToRaw(legacy), csv)
    }
    metadata <- list(type = "metadata", store = store, csv = csv)
  }
  list(
    repo = repo, metadata = metadata, head = paste0("ref: refs/heads/", branch),
    yml = yml, record = record, version = version
  )
}
