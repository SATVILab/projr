.projr_source_get <- function(bump_component) {
  # consider early exit
  # ------------------
  if (!.projr_source_get_check(bump_component)) {
    return(invisible(FALSE))
  }

  # loop over remotes
  for (label in .projr_source_get_get()) {
    .projr_source_get_label(
      label = label,
      bump_component = bump_component
    )
  }
}

.projr_source_get_label <- function(label,
                                    bump_component) {
  yml_projr_label <- projr_yml_get_unchecked()[["directories"]][[label]]
}

.projr_source_get_check <- function(bump_component) {
  # output_run
  output_run <- .projr_build_get_output_run(bump_component)
  if (!output_run) {
    return(invisible(FALSE))
  }
  length(.projr_source_get_get()) > 0
}

.projr_source_get_get <- function(label) {
  projr_yml_get_unchecked()[["directories"]]
}
# TODO: okay, so the logic is a bit messed up here,
# but I think we're okay generally.
# to be honest, it seems not that different to before.
# the only thing is now we're going to have to think about uploads
# more and what effect that has on our versioning.

# get from one remote
# --------------------

.projr_source_get_remote <- function(remote_type,
                                     bump_component) {
  yml_projr_remote <- projr_yml_get_unchecked()[["build"]][[remote_type]]
}
