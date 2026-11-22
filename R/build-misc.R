.is_dev_build <- function(bump_component) {
  is.null(bump_component) || bump_component == "dev"
}
