# load dev package
devtools::load_all(path = file.path(Sys.getenv("pkg"), "projr"))
# arbitrary repo name
repo_name <- "report"

# local directory
# ---------------

# empty and create project directory
dir_test <- file.path(tempdir(), repo_name)
if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
.projr_dir_create(dir_test)

setwd(dir_test)
Sys.unsetenv("PROJR_TEST")

# GitHub
# ---------------

# check that we've got a GitHub PAT available
(gh_account <- gh::gh_whoami())
(me <- gh_account$login)
# delete repo if it exists
gh::gh(
  "DELETE /repos/:username/:pkg",
  username = me, pkg = repo_name
)

# run projr init
# --------------------
projr_init()
