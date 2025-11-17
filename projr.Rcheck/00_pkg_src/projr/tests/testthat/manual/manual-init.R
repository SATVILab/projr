devtools::load_all()
pkgname_kablamph <- paste0("test-projrsimple-", round(runif(1, 500, 1000)))
path_tmp <- file.path(tempdir(), pkgname_kablamph)
dir.create(path_tmp, recursive = TRUE)

setwd(path_tmp)
# package should exist, and be private
projr_init(
  init_git = TRUE, init_readme = TRUE, init_github = TRUE, github_private = TRUE
)

# delete the GitHub repo
whoami <- gh::gh_whoami()
gh::gh(
  "DELETE /repos/{username}/{pkg}",
  username = whoami$login,
  pkg = pkgname_kablamph
)
