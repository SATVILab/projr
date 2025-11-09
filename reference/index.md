# Package index

## Core workflow

Main functions for project workflow

- [`projr_init()`](https://satvilab.github.io/projr/reference/projr_init.md)
  [`projr_init_all()`](https://satvilab.github.io/projr/reference/projr_init.md)
  [`projr_init_renv()`](https://satvilab.github.io/projr/reference/projr_init.md)
  [`projr_init_cite()`](https://satvilab.github.io/projr/reference/projr_init.md)
  [`projr_init_git()`](https://satvilab.github.io/projr/reference/projr_init.md)
  [`projr_init_github()`](https://satvilab.github.io/projr/reference/projr_init.md)
  [`projr_init_license()`](https://satvilab.github.io/projr/reference/projr_init.md)
  : Initialize a projr Project
- [`projr_init_prompt()`](https://satvilab.github.io/projr/reference/projr_init_prompt.md)
  : Initialise project
- [`projr_build()`](https://satvilab.github.io/projr/reference/projr_build.md)
  [`projr_build_major()`](https://satvilab.github.io/projr/reference/projr_build.md)
  [`projr_build_minor()`](https://satvilab.github.io/projr/reference/projr_build.md)
  [`projr_build_patch()`](https://satvilab.github.io/projr/reference/projr_build.md)
  : Build project to output
- [`projr_build_dev()`](https://satvilab.github.io/projr/reference/projr_build_dev.md)
  : Build dev project
- [`projr_restore_repo()`](https://satvilab.github.io/projr/reference/projr_restore.md)
  [`projr_restore_repo_wd()`](https://satvilab.github.io/projr/reference/projr_restore.md)
  [`projr_restore()`](https://satvilab.github.io/projr/reference/projr_restore.md)
  : Restore project artefact directories

## Path helpers

Functions for getting project paths

- [`projr_path_get()`](https://satvilab.github.io/projr/reference/projr_path_get.md)
  : Return path

- [`projr_path_get_dir()`](https://satvilab.github.io/projr/reference/projr_path_get_dir.md)
  : Return path to profile-specific directory

- [`projr_path_get_cache_build_dir()`](https://satvilab.github.io/projr/reference/projr_path_get_cache_build.md)
  [`projr_path_get_cache_build()`](https://satvilab.github.io/projr/reference/projr_path_get_cache_build.md)
  :

  Get `projr` build cache directory

## Initialisation helpers

Helper functions for project initialisation

- [`projr_init()`](https://satvilab.github.io/projr/reference/projr_init.md)
  [`projr_init_all()`](https://satvilab.github.io/projr/reference/projr_init.md)
  [`projr_init_renv()`](https://satvilab.github.io/projr/reference/projr_init.md)
  [`projr_init_cite()`](https://satvilab.github.io/projr/reference/projr_init.md)
  [`projr_init_git()`](https://satvilab.github.io/projr/reference/projr_init.md)
  [`projr_init_github()`](https://satvilab.github.io/projr/reference/projr_init.md)
  [`projr_init_license()`](https://satvilab.github.io/projr/reference/projr_init.md)
  : Initialize a projr Project
- [`projr_init_renviron()`](https://satvilab.github.io/projr/reference/projr_init_renviron.md)
  : Set environment variables for projr_init

## Version management

Functions for managing project versions

- [`projr_version_get()`](https://satvilab.github.io/projr/reference/projr_version_get.md)
  : Returns project version
- [`projr_version_set()`](https://satvilab.github.io/projr/reference/projr_version_set.md)
  : Set Project Version

## YAML configuration

Functions for managing \_projr.yml configuration

- [`projr_yml_get()`](https://satvilab.github.io/projr/reference/projr_yml_get.md)
  :

  Get active `projr` settings and checks for validity

- [`projr_yml_check()`](https://satvilab.github.io/projr/reference/projr_yml_check.md)
  :

  Check active `projr` settings.

- [`projr_yml_git_set()`](https://satvilab.github.io/projr/reference/yml-git.md)
  [`projr_yml_git_set_default()`](https://satvilab.github.io/projr/reference/yml-git.md)
  : Set Git options

- [`projr_yml_cite_set()`](https://satvilab.github.io/projr/reference/yml-cite.md)
  [`projr_yml_cite_set_default()`](https://satvilab.github.io/projr/reference/yml-cite.md)
  : Set citation options

- [`projr_yml_dest_add_github()`](https://satvilab.github.io/projr/reference/projr_yml_dest_add_github.md)
  : Add a GitHub release as a destination

- [`projr_yml_dest_add_local()`](https://satvilab.github.io/projr/reference/projr_yml_dest_add.md)
  : Add a local directory as a destination

- [`projr_yml_dest_add_osf()`](https://satvilab.github.io/projr/reference/projr_yml_dest_add_osf.md)
  [`projr_yml_dest_add_osf_proj()`](https://satvilab.github.io/projr/reference/projr_yml_dest_add_osf.md)
  [`projr_yml_dest_add_osf_comp()`](https://satvilab.github.io/projr/reference/projr_yml_dest_add_osf.md)
  : Add an OSF node as a destination

- [`projr_yml_par_add()`](https://satvilab.github.io/projr/reference/projr_yml_par_add.md)
  :

  Add the `parameters` key

- [`projr_yml_script_add()`](https://satvilab.github.io/projr/reference/yml-script.md)
  [`projr_yml_script_rm()`](https://satvilab.github.io/projr/reference/yml-script.md)
  [`projr_yml_script_rm_all()`](https://satvilab.github.io/projr/reference/yml-script.md)
  [`projr_yml_script_add_pre()`](https://satvilab.github.io/projr/reference/yml-script.md)
  [`projr_yml_script_add_post()`](https://satvilab.github.io/projr/reference/yml-script.md)
  : Build script-related functions

## Profile management

Functions for managing project profiles

- [`projr_profile_create()`](https://satvilab.github.io/projr/reference/projr_profile_create.md)
  : Add projr profile file
- [`projr_profile_delete()`](https://satvilab.github.io/projr/reference/projr_profile_delete.md)
  : Delete a projr profile from \_projr.yml
- [`projr_profile_get()`](https://satvilab.github.io/projr/reference/projr_profile_get.md)
  : Get active projr profile

## Parameters

Functions for managing project parameters

- [`projr_par_get()`](https://satvilab.github.io/projr/reference/projr_par_get.md)
  [`projr_param_get()`](https://satvilab.github.io/projr/reference/projr_par_get.md)
  : Get project parameters

## Environment management

Functions for managing environment variables

- [`projr_env_set()`](https://satvilab.github.io/projr/reference/projr_env_set.md)
  : Set environment variables from files

## Git ignore management

Functions for managing .gitignore and .Rbuildignore

- [`projr_ignore()`](https://satvilab.github.io/projr/reference/projr_ignore.md)
  :

  Manually Ignore Files or Directories in `.gitignore` and
  `.Rbuildignore`

- [`projr_ignore_auto()`](https://satvilab.github.io/projr/reference/projr_ignore_auto.md)
  :

  Update `.gitignore` and `.Rbuildignore` with projr-managed ignores

- [`projr_unignore_manual()`](https://satvilab.github.io/projr/reference/projr_unignore_manual.md)
  [`projr_unignore_manual_dir()`](https://satvilab.github.io/projr/reference/projr_unignore_manual.md)
  [`projr_unignore_manual_file()`](https://satvilab.github.io/projr/reference/projr_unignore_manual.md)
  [`projr_unignore_manual_file_git()`](https://satvilab.github.io/projr/reference/projr_unignore_manual.md)
  [`projr_unignore_manual_dir_git()`](https://satvilab.github.io/projr/reference/projr_unignore_manual.md)
  [`projr_unignore_manual_file_rbuild()`](https://satvilab.github.io/projr/reference/projr_unignore_manual.md)
  [`projr_unignore_manual_dir_rbuild()`](https://satvilab.github.io/projr/reference/projr_unignore_manual.md)
  :

  Manually Unignore Files or Directories in `.gitignore` and
  `.Rbuildignore`

## renv helpers

Functions for managing renv dependencies

- [`projr_renv_restore()`](https://satvilab.github.io/projr/reference/projr_renv_restore.md)
  [`projr_renv_update()`](https://satvilab.github.io/projr/reference/projr_renv_restore.md)
  [`projr_renv_restore_and_update()`](https://satvilab.github.io/projr/reference/projr_renv_restore.md)
  : Restore or Update renv Lockfile Packages
- [`projr_renv_test()`](https://satvilab.github.io/projr/reference/projr_renv_test.md)
  : Test renv restore

## Data helpers

Functions for managing project data

- [`projr_use_data()`](https://satvilab.github.io/projr/reference/projr_use_data.md)
  :

  `projr` drop-in replacement for usethis::use_data

## OSF helpers

Functions for Open Science Framework integration

- [`projr_osf_create_project()`](https://satvilab.github.io/projr/reference/projr_osf_create_project.md)
  : Create a new project on OSF
- [`projr_instr_auth_github()`](https://satvilab.github.io/projr/reference/instr_auth.md)
  [`projr_instr_auth_osf()`](https://satvilab.github.io/projr/reference/instr_auth.md)
  : Two-minutes or less authorisation instructions

## GitHub helpers

Functions for GitHub integration

- [`projr_instr_auth_github()`](https://satvilab.github.io/projr/reference/instr_auth.md)
  [`projr_instr_auth_osf()`](https://satvilab.github.io/projr/reference/instr_auth.md)
  : Two-minutes or less authorisation instructions

## Changelog

Functions for managing changelog

- [`projr_cat_changelog()`](https://satvilab.github.io/projr/reference/cat.md)
  : Cat useful information

## Internal utilities

Internal helper functions (not exported)

- [`.yml_get()`](https://satvilab.github.io/projr/reference/dot-yml_get.md)
  :

  Get active `projr` settings and do no check

- [`projr_profile_create_local()`](https://satvilab.github.io/projr/reference/projr_profile_create_local.md)
  :

  Create a local `projr` profile

- [`projr_profile_delete_local()`](https://satvilab.github.io/projr/reference/projr_profile_delete_local.md)
  :

  Delete local `projr` settings file.
