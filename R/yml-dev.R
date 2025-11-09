# Dev-level YAML configuration functions
# For handling the top-level "dev" key in _projr.yml

.yml_dev_get <- function(profile) {
  .yml_get(profile)[["dev"]] %||% NULL
}

.yml_dev_get_scripts <- function(profile) {
  .yml_dev_get(profile)[["scripts"]] %||% NULL
}
