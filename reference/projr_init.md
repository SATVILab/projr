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
projr_init(
  git = TRUE,
  git_commit = TRUE,
  github = TRUE,
  github_public = FALSE,
  github_org = NULL,
  dir = TRUE,
  readme = TRUE,
  readme_rmd = TRUE,
  desc = FALSE,
  license = NULL,
  projr_yml = FALSE,
  lit_doc = NULL
)

projr_init_all(github_org = NULL, license = NULL, lit_doc = NULL)

projr_init_renv(bioc = TRUE)

projr_init_cite()

projr_init_git(commit = TRUE)

projr_init_github(username = NULL, public = FALSE)

projr_init_license(license, first_name, last_name)
```

## Arguments

- git:

  Logical. If `TRUE`, initializes a Git repository. Defaults to `TRUE`.

- git_commit:

  Logical. If `TRUE`, commits the initial changes to the Git repository.
  Defaults to `TRUE`.

- github:

  Logical. If `TRUE`, attempts to create a GitHub repository for the
  project. Defaults to `TRUE`.

- github_public:

  Logical. If `TRUE`, the GitHub repository will be public. Defaults to
  `FALSE`.

- github_org:

  Character or `NULL`. The GitHub organization under which to create the
  repository. Defaults to `NULL`, which creates the repository under the
  user's account (as implied by the GitHub token).

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

  Logical. If `TRUE`, includes Bioconductor packages in the renv setup.
  Defaults to `TRUE`.

- renv:

  Logical. If `TRUE`, initializes a renv environment for dependency
  management. Defaults to `FALSE`.

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
