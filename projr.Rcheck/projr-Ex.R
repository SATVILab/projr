pkgname <- "projr"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "projr-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('projr')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("projr_env_set")
### * projr_env_set

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: projr_env_set
### Title: Set environment variables from files
### Aliases: projr_env_set

### ** Examples

# Activate only the local overrides
## Not run: 
##D   projr_env_set("_environment.local")
## End(Not run)
# Activate all available defaults in the standard order
## Not run: 
##D   projr_env_set()
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("projr_env_set", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("projr_ignore")
### * projr_ignore

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: projr_ignore
### Title: Manually Ignore Files or Directories in '.gitignore' and
###   '.Rbuildignore'
### Aliases: projr_ignore projr_ignore_dir projr_ignore_file
###   projr_ignore_file_git projr_ignore_dir_git projr_ignore_file_rbuild
###   projr_ignore_dir_rbuild

### ** Examples

# Manually ignore files and directories
projr_ignore(c("output", "tempfile.log"))

# Specifically ignore directories
projr_ignore_dir("data")

# Specifically ignore files
projr_ignore_file("README.md")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("projr_ignore", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("projr_ignore_auto")
### * projr_ignore_auto

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: projr_ignore_auto
### Title: Update '.gitignore' and '.Rbuildignore' with projr-managed
###   ignores
### Aliases: projr_ignore_auto

### ** Examples

## Not run: 
##D projr_ignore_auto()
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("projr_ignore_auto", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("projr_manifest_file_query")
### * projr_manifest_file_query

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: projr_manifest_file_last_change
### Title: Query When a Specific File Last Changed
### Aliases: projr_manifest_file_last_change projr_manifest_file_changed
###   projr_manifest_file_history projr_manifest_file_first

### ** Examples

## Not run: 
##D # Query when a specific file last changed
##D projr_manifest_file_last_change("data.csv", label = "output")
##D 
##D # Search all directories for a file
##D projr_manifest_file_last_change("report.pdf")
## End(Not run)

## Not run: 
##D # Check if a file changed between versions
##D projr_manifest_file_changed("data.csv", "output", "0.0.1", "0.0.2")
##D 
##D # Check against current version
##D projr_manifest_file_changed("data.csv", "output", "0.0.1")
## End(Not run)

## Not run: 
##D # Get full history for a file
##D projr_manifest_file_history("data.csv", label = "output")
##D 
##D # Search all directories
##D projr_manifest_file_history("config.yml")
## End(Not run)

## Not run: 
##D # Get when a file first appeared
##D projr_manifest_file_first("data.csv", label = "output")
##D 
##D # Search all directories
##D projr_manifest_file_first("README.md")
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("projr_manifest_file_query", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("projr_manifest_query")
### * projr_manifest_query

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: projr_manifest_changes
### Title: Query Files Changed Between Versions
### Aliases: projr_manifest_changes projr_manifest_range
###   projr_manifest_last_change

### ** Examples

## Not run: 
##D # Query changes between v0.0.1 and v0.0.2
##D projr_manifest_changes("0.0.1", "0.0.2")
##D 
##D # Query changes in output directory only
##D projr_manifest_changes("0.0.1", "0.0.2", label = "output")
##D 
##D # Query changes from earliest version to current
##D projr_manifest_changes()
## End(Not run)

## Not run: 
##D # Query all changes from v0.0.1 to current
##D projr_manifest_range("0.0.1")
##D 
##D # Query changes in a specific range
##D projr_manifest_range("0.0.1", "0.0.5")
## End(Not run)

## Not run: 
##D # Query last changes for current version
##D projr_manifest_last_change()
##D 
##D # Query last changes as of v0.0.5
##D projr_manifest_last_change("0.0.5")
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("projr_manifest_query", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("projr_osf_create_project")
### * projr_osf_create_project

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: projr_osf_create_project
### Title: Create a new project on OSF
### Aliases: projr_osf_create_project

### ** Examples

## Not run: 
##D projr_osf_create_project(
##D   title = "My New Project",
##D   description = "This is a description of my new project.",
##D   public = TRUE # because open science
##D )
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("projr_osf_create_project", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("projr_path_get")
### * projr_path_get

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: projr_path_get
### Title: Return path
### Aliases: projr_path_get

### ** Examples

## Not run: 
##D if (interactive()) {
##D   # EXAMPLE1
##D }
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("projr_path_get", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("projr_renv_restore")
### * projr_renv_restore

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: projr_renv_restore
### Title: Restore or Update renv Lockfile Packages
### Aliases: projr_renv_restore projr_renv_update
###   projr_renv_restore_and_update

### ** Examples

## Not run: 
##D # Restore all packages
##D projr_renv_restore()
##D 
##D # Update all packages
##D projr_renv_update()
##D 
##D # Restore and then update all packages
##D projr_renv_restore_and_update()
##D 
##D # Only restore non-GitHub packages
##D projr_renv_restore(github = FALSE)
##D 
##D # Only update GitHub packages
##D projr_renv_update(non_github = FALSE)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("projr_renv_restore", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("projr_restore")
### * projr_restore

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: projr_restore_repo
### Title: Restore project artefact directories
### Aliases: projr_restore_repo projr_restore_repo_wd projr_restore

### ** Examples

## Not run: 
##D   # Restore all raw artefacts in existing local project
##D   projr_restore()
##D   
##D   # Restore specific labels
##D   projr_restore(label = c("raw-data", "cache"))
##D   
##D   # Restore from specific source type
##D   projr_restore(type = "local", title = "archive")
##D 
##D   # Clone repository into subdirectory and restore artefacts
##D   projr_restore_repo("owner/repo")
##D   
##D   # Clone to specific path
##D   projr_restore_repo("owner/repo", path = "my-project")
##D 
##D   # Clone repository into current directory and restore artefacts
##D   projr_restore_repo_wd("owner/repo")
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("projr_restore", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("projr_unignore_manual")
### * projr_unignore_manual

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: projr_unignore_manual
### Title: Manually Unignore Files or Directories in '.gitignore' and
###   '.Rbuildignore'
### Aliases: projr_unignore_manual projr_unignore_manual_dir
###   projr_unignore_manual_file projr_unignore_manual_file_git
###   projr_unignore_manual_dir_git projr_unignore_manual_file_rbuild
###   projr_unignore_manual_dir_rbuild

### ** Examples

# Manually unignore files and directories
projr_unignore_manual(c("output", "tempfile.log"))

# Specifically unignore directories
projr_unignore_manual_dir("data")

# Specifically unignore files
projr_unignore_manual_file("README.md")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("projr_unignore_manual", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("projr_use_data")
### * projr_use_data

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: projr_use_data
### Title: 'projr' drop-in replacement for usethis::use_data
### Aliases: projr_use_data

### ** Examples

## Not run: 
##D x <- 1:10
##D y <- 1:100
##D 
##D projr_use_data(x, y) # For external use
##D projr_use_data(x, y, internal = TRUE) # For internal use
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("projr_use_data", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("yml-cite")
### * yml-cite

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: projr_yml_cite_set
### Title: Set citation options
### Aliases: projr_yml_cite_set projr_yml_cite_set_default

### ** Examples

## Not run: 
##D # set all to TRUE
##D projr_yml_cite_set(all = TRUE)
##D 
##D # set all to FALSE
##D projr_yml_cite_set(all = FALSE)
##D 
##D # set only cff to FALSE
##D projr_yml_cite_set(cff = FALSE)
##D 
##D # revert to defaults
##D projr_yml_cite_set()
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("yml-cite", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("yml-git")
### * yml-git

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: projr_yml_git_set
### Title: Set Git options
### Aliases: projr_yml_git_set projr_yml_git_set_default

### ** Examples

## Not run: 
##D # set all to TRUE
##D projr_yml_git_set(all = TRUE)
##D 
##D # set all to FALSE
##D projr_yml_git_set(all = FALSE)
##D 
##D # set only add_untracked to FALSE
##D projr_yml_git_set(add_untracked = FALSE)
##D 
##D # revert to defaults
##D projr_yml_git_set_default()
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("yml-git", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
