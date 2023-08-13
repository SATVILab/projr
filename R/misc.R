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
