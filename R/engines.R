.engines_get <- function(file = NULL) {
  if (!is.null(file) && length(file) > 0) {
    .engines_get_from_file(file)
  } else {
    .engines_get_auto()
  }
}

.engines_get_from_file <- function(file) {

}

.engines_get_auto <- function() {
  
}