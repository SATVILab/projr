.template_token_pattern <- "@(version|branch|uuid)(?=$|[-_./ \\\\])"

.template_substitute <- function(x, replacement, literal = identity) {
  .assert_chr_min(x)
  if (!length(x)) {
    return(x)
  }
  matches <- gregexpr(.template_token_pattern, x, perl = TRUE)
  parts <- regmatches(x, matches, invert = NA)
  x[] <- vapply(
    parts,
    function(part) {
      if (anyNA(part)) {
        return(NA_character_)
      }
      # regmatches alternates literal text and matched tokens, starting with text.
      text <- seq.int(1L, length(part), by = 2L)
      part[text] <- vapply(part[text], literal, character(1))
      part[-text] <- vapply(part[-text], replacement, character(1))
      paste0(part, collapse = "")
    },
    character(1)
  )
  x
}

.template_expand <- function(
  x,
  version = .template_version(),
  branch = .template_branch(),
  uuid = .template_uuid()
) {
  if (!length(x) || !any(grepl(.template_token_pattern, x, perl = TRUE))) {
    return(x)
  }
  .template_substitute(x, function(token) {
    switch(
      token,
      "@version" = .version_v_add(
        version %||%
          stop(
            "@version requires a project version. Set one with projr_version_set() or run a production build.",
            call. = FALSE
          )
      ),
      "@branch" = branch,
      "@uuid" = uuid
    )
  })
}

.template_version <- function() {
  version <- Sys.getenv("PROJR_BUILD_VERSION")
  if (nzchar(version)) version else .version_get_v(required = FALSE)
}

.template_uuid <- function() {
  uuid <- Sys.getenv("PROJR_BUILD_UUID")
  if (nzchar(uuid)) {
    return(uuid)
  }
  uuid <- getOption("projr.session_uuid")
  if (is.null(uuid)) {
    uuid <- uuid::UUIDgenerate(use.time = FALSE)
    options(projr.session_uuid = uuid)
  }
  uuid
}

# Read the checkout's HEAD, including the gitdir pointer in linked worktrees.
.template_checkout_head <- function() {
  root <- tryCatch(
    rprojroot::find_root(rprojroot::is_git_root, path = .path_get()),
    error = function(e) NULL
  )
  if (is.null(root)) {
    return("")
  }
  git_dir <- file.path(root, ".git")
  if (!dir.exists(git_dir)) {
    pointer <- readLines(git_dir, n = 1L, warn = FALSE)
    git_dir <- fs::path_abs(sub("^gitdir: ", "", pointer), start = root)
  }
  path <- file.path(git_dir, "HEAD")
  if (!file.exists(path)) {
    return("")
  }
  head <- readLines(path, n = 1L, warn = FALSE)
  if (length(head)) head else ""
}

.template_head <- function() {
  head <- Sys.getenv("PROJR_BUILD_HEAD", unset = NA_character_)
  if (is.na(head)) .template_checkout_head() else head
}

.template_branch_raw <- function(head = .template_head()) {
  if (!startsWith(head, "ref: refs/heads/")) {
    reason <- if (nzchar(head)) {
      "HEAD is detached"
    } else {
      "no Git checkout was found"
    }
    stop(
      "@branch needs a named Git branch for this operation (",
      reason,
      ").",
      call. = FALSE
    )
  }
  substring(head, nchar("ref: refs/heads/") + 1L)
}

.template_branch <- function(head = .template_head()) {
  if (grepl("^([0-9a-f]{40}|[0-9a-f]{64})$", head)) {
    return(paste0("__detached-", head))
  }
  .path_encode_component(.template_branch_raw(head))
}

.path_encode_component <- function(x) {
  bytes <- charToRaw(enc2utf8(x))
  plain <- bytes %in% charToRaw("abcdefghijklmnopqrstuvwxyz0123456789-")
  # Preserve distinct names even on case-insensitive filesystems. Escape
  # underscores too, so a literal "_2f" cannot collide with an encoded slash.
  if (grepl("^(con|prn|aux|nul|com[1-9]|lpt[1-9])$|^-", x)) {
    plain[1] <- FALSE
  }
  encoded <- paste0("_", as.character(bytes))
  encoded[plain] <- rawToChar(bytes[plain], multiple = TRUE)
  paste0(encoded, collapse = "")
}

.template_context_start <- function(version = NULL) {
  values <- c(
    PROJR_BUILD_UUID = uuid::UUIDgenerate(use.time = FALSE),
    PROJR_BUILD_LOG = "",
    PROJR_BUILD_HEAD = .template_checkout_head(),
    PROJR_BUILD_VERSION = if (is.null(version)) "" else .version_v_add(version)
  )
  previous <- Sys.getenv(names(values), unset = NA_character_)
  do.call(Sys.setenv, as.list(values))
  function() {
    Sys.unsetenv(names(previous)[is.na(previous)])
    if (any(!is.na(previous))) {
      do.call(Sys.setenv, as.list(previous[!is.na(previous)]))
    }
    invisible(NULL)
  }
}
