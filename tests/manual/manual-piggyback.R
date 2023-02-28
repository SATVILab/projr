# what do I need to do?


# initialise, choosing some other repo name
# remove any pre-existing repo

# ==================
# Set-up
# ==================

# load dev package
devtools::load_all(path = file.path(Sys.getenv("pkg"), "projr"))

# arbitrary repo name
repo_name <- "report"

# local directory
# --------------------

# empty and create project directory
dir_test <- file.path(tempdir(), repo_name)
if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)

if (!dir.exists(dir_test)) dir.create(dir_test)
fn_vec <- list.files(testthat::test_path("./project_structure"))

for (x in fn_vec) {
    file.copy(
        file.path(testthat::test_path("./project_structure"), x),
        file.path(dir_test, x),
        overwrite = TRUE
    )
}
desc <- read.dcf("DESCRIPTION")
desc[1] <- repo_name
write.dcf(desc, file.path(dir_test, "DESCRIPTION"))
gitignore <- c(
    "# R", ".Rproj.user", ".Rhistory", ".RData",
    ".Ruserdata", "", "# docs", "docs/*"
)
writeLines(gitignore, file.path(dir_test, ".gitignore"))

rbuildignore <- c("^.*\\.Rproj$", "^\\.Rproj\\.user$", "^docs$")
writeLines(rbuildignore, file.path(dir_test, ".Rbuildignore"))

setwd(dir_test)

# GitHub
# -------------

# check that we've got a GitHub PAT available
(gh_account <- gh::gh_whoami())
(me <- gh_account$login)
# delete repo if it exists
gh::gh(
    "DELETE /repos/:username/:pkg",
    username = me, pkg = repo_name
)
usethis::use_git()
usethis::use_github()

# nothing uploaded
.projr_pb_upload(FALSE)

# initialise project
file.create(projr::projr_path_get("data-raw", "test_data_raw.txt"))
file.create(projr_path_get("cache", "test_cache.txt"))
file.create(projr_path_get("output", "test_output.txt", output_safe = FALSE))
file.create(projr_path_get("docs", "test_docs.txt"))

.projr_pb_upload(TRUE)

# should have three releases
piggyback::pb_releases()
