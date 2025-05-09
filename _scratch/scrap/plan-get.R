# =====================
# make plan
# =====================

# unique plans:
# add_all
# add_missing
# change
# add_all_if_missing
# delete_add_all
# delete_add_all_if_change

.dest_send_get_plan <- function(inspect,
                                      strategy,
                                      type,
                                      structure) {
  # get exactly how we decide what to do with the changes
  switch(inspect,
    "none" = .dest_send_get_plan_none(strategy),
    "manifest" = ,
    "file" = .dest_send_get_plan_source(
      structure = structure,
      type = type,
      strategy = strategy
    ),
    stop(
      paste0("inspect '", inspect, "' not supported"),
      call. = FALSE
    )
  )
}

# ------------------------
# version source
# ------------------------

# basic if not using any
.dest_send_get_plan_none <- function(strategy) {
  switch(strategy,
    "upload-all" = ,
    "upload-missing" = "add_all",
    "sync-using-deletion" = ,
    "sync-using-version" = "delete_add_all",
    stop(
      paste0("strategy '", strategy, "' not supported"),
      call. = FALSE
    )
  )
}

# more complex if using one
.dest_send_get_plan_source <- function(structure,
                                             type,
                                             strategy) {
  switch(type,
    "github" = .dest_send_get_plan_flat(
      strategy = strategy
    ),
    "local" = ,
    "osf" = .dest_send_get_plan_hier(
      structure = structure,
      strategy = strategy
    ),
    stop(paste0("type '", type, "' not supported"), call. = FALSE)
  )
}

# ------------------------
# sync approach
# ------------------------

# flat remotes
.dest_send_get_plan_flat <- function(strategy) {
  switch(strategy,
    "upload-all" = "add_all",
    "upload-missing" = "add_all_if_missing",
    "sync-using-deletion" = "delete_add_all",
    "sync-using-version" = "delete_add_all_if_change",
    stop(
      paste0("strategy '", strategy, "' not supported"),
      call. = FALSE
    )
  )
}

# hierarchical remotes
.dest_send_get_plan_hier <- function(structure,
                                           strategy) {
  switch(strategy,
    "upload-all" = "add_all",
    "upload-missing" = .dest_send_get_plan_hier_missing(
      structure = structure
    ),
    "sync-using-deletion" = "delete_add_all",
    "sync-using-version" = .dest_send_get_plan_hier_version(
      structure = structure
    ),
    stop(
      paste0("strategy '", strategy, "' not supported"),
      call. = FALSE
    )
  )
}

# ------------------------
# remote structure
# ------------------------

.dest_send_get_plan_hier_missing <- function(structure) {
  switch(structure,
    "latest" = "add_missing",
    "version" = "add_all",
    stop(paste0("structure '", structure, "' not supported"), call. = FALSE)
  )
}

.dest_send_get_plan_hier_version <- function(structure) {
  switch(structure,
    "latest" = "change",
    "version" = "delete_add_all_if_change",
    stop(paste0("structure '", structure, "' not supported"), call. = FALSE)
  )
}
