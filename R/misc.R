.try_err_msg_get <- function(x, require_try_error = TRUE) {
  if (!inherits(x, "try-error")) {
    if (require_try_error) {
      stop("x must be a try-error object")
    } else {
      return(NULL)
    }
  }
  gsub("^Error in .* \\: \\n\\s*", "", x[1]) |>
    gsub(pattern = "\\n\\s*$", replacement = "")
}

.projr_dep_add <- function(dep) {
  dir_proj <- rprojroot::is_r_package$find_file()
  path_dep <- file.path(dir_proj, "_dependencies.R")
  dep_vec <- readLines(path_dep)
  for (i in seq_along(dep)) {
    dep_txt <- paste0("library(", dep[[i]], ")", collapse = "")
    if (!any(grepl(dep_txt, dep_vec))) {
      dep_vec <- c(dep_vec, dep_txt)
    }
  }
  writeLines(dep_vec, path_dep)
  .projr_newline_append(path_dep)
  invisible(TRUE)
}

# taken from withr::with_dir
with_dir <- function(new, code) {
  old <- setwd(dir = new)
  on.exit(setwd(old))
  force(code)
}
