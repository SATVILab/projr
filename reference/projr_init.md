# Initialize a projr Project

This function performs a full initialization of a projr project. It sets
up the project structure by creating directories, generating a README
(in Markdown or R Markdown format), configuring a renv environment,
writing a DESCRIPTION file, applying a license (if provided), setting up
citation files, creating a projr configuration YAML file, establishing
literate documentation, and configuring both Git and GitHub
repositories.

## Usage

``` r
projr_init_github(
  user = NULL,
  org = NULL,
  public = FALSE,
  create_repo = TRUE,
  git_commit = TRUE
)

projr_init(
  git = TRUE,
  git_commit = TRUE,
  github = TRUE,
  github_public = FALSE,
  github_user = NULL,
  github_org = NULL,
  dir = TRUE,
  readme = TRUE,
  readme_rmd = TRUE,
  desc = FALSE,
  license = NULL,
  projr_yml = FALSE,
  lit_doc = NULL
)

projr_init_all(
  github = TRUE,
  github_org = NULL,
  license = NULL,
  lit_doc = NULL
)

projr_init_renv(bioc = TRUE)

projr_init_cite()

projr_init_git(commit = TRUE)

projr_init_license(license, first_name, last_name)

projr_init_ignore()
```

## Arguments

- user:

  Character or \codeNULL (for \codeprojr_init_github). GitHub username
  to use when creating the remote; if \codeNULL the token owner is used.
  Defaults to \codeNULL.

- org:

  Character or \codeNULL (for \codeprojr_init_github). GitHub
  organisation to use when creating the remote. Defaults to \codeNULL.

- public:

  Logical (for `projr_init_github`). If `TRUE`, the GitHub repository
  will be public. Defaults to `FALSE`.

- create_repo:

  Logical. If `TRUE` and the project does not have a local Git
  repository, then in interactive mode it offers to create a GitHub
  repository and in non-interactive mode creates one automatically.
  Defaults to `TRUE`.

- git_commit:

  Logical. If `TRUE`, commits the initial changes to the Git repository.
  Defaults to `TRUE`.

- git:

  Logical. If `TRUE`, initializes a Git repository. Defaults to `TRUE`.

- github:

  Logical. If `TRUE`, attempts to create a GitHub repository for the
  project. Defaults to `TRUE`.

- github_public:

  Logical. If `TRUE`, the GitHub repository will be public. Defaults to
  `FALSE`.

- github_user, github_org:

  Character or `NULL`. The owner of the GitHub repo to create. If both
  are NULL, then creates the repository under the current user's account
  (as implied by the GitHub token). If both are specified, then
  organisation is preferred. Default is `NULL`.

- dir:

  Logical. If `TRUE`, initializes the projr-specified directories (e.g.,
  raw, cache, output). Defaults to `TRUE`.

- readme:

  Logical. If `TRUE`, creates a README file. Defaults to `TRUE`.

- readme_rmd:

  Logical. If `TRUE`, generates a README in R Markdown format
  (`README.Rmd`); otherwise, a Markdown file (`README.md`) is created.
  Defaults to `TRUE`.

- desc:

  Logical. If `TRUE`, creates a DESCRIPTION file for the project.
  Defaults to `FALSE`.

- license:

  Character or `NULL`. Specifies the license to apply (e.g., `"ccby"`,
  `"apache"`, `"cc0"`, `"proprietary"`). Defaults to `NULL`.

- projr_yml:

  Logical. If `TRUE`, creates a `projr.yml` configuration file. Defaults
  to `FALSE`.

- lit_doc:

  Character or `NULL`. Specifies the type of literate documentation to
  create. Supported values are `"bookdown"`, `"project"`, `"quarto"`,
  and `"rmd"`. Defaults to `NULL`.

- bioc:

  Logical (for \codeprojr_init_renv). If \codeTRUE, includes
  Bioconductor packages in the renv setup. Defaults to \codeTRUE.

- commit:

  Logical (for \codeprojr_init_git). If \codeTRUE, commits the initial
  changes to the Git repository. Defaults to \codeTRUE.

- first_name:

  Character (for `projr_init_license`). First name for proprietary
  license. Required when `license = "proprietary"`.

- last_name:

  Character (for `projr_init_license`). Last name for proprietary
  license. Required when `license = "proprietary"`.

- username:

  Character or `NULL` (for `projr_init_github`). The GitHub username or
  organization under which to create the repository. Defaults to `NULL`.

## Value

Invisibly returns `TRUE` if initialization is successful, or `FALSE` if
a particular step is skipped.

## Details

The `projr_init` function is a wrapper that calls several helper
functions to perform the following tasks:

- Prevent working directory errors by ensuring the usethis project is
  set.

- Create project directories.

- Generate a README file (in Markdown or R Markdown format).

- Initialize a renv environment, optionally with Bioconductor support.

- Write a DESCRIPTION file for project metadata.

- Apply a specified license.

- Configure citation files (if a DESCRIPTION file exists).

- Create a projr configuration YAML file.

- Set up literate documentation in the chosen format.

- Initialize Git (and optionally commit initial changes).

- Create a GitHub repository if requested.
