# Prospective Issues for projr

This document outlines well-specified enhancement requests for the projr package. These issues are intended to guide future contributors or automated agents in implementing improvements to the package's functionality.

---

## Issue 1: Enhance Log Cleanup Helpers with Date/Version-Based Filtering

### Summary

The current log management helpers (`log_clear_history()` and `log_clear_output()`) only support clearing logs wholesale. They accept `before_date` and `before_version` parameters but emit informational messages instead of applying the filters for selective deletions. This enhancement request seeks to implement actual filtering functionality to enable retention policy-based log pruning.

### Current Behavior

Location: `R/log.R`

The functions `projr_log_clear()`, `.log_clear_history()`, and `.log_clear_output()` currently:

1. Accept optional `before_date` and `before_version` parameters
2. When these parameters are NULL, delete all logs wholesale
3. When `before_date` is provided to `.log_clear_output()`, partial date-based filtering is implemented via `.log_clear_output_by_date()`
4. When `before_version` is provided, the function displays an informational message: "Version-based log clearing not yet implemented"
5. For `.log_clear_history()`, both date and version filters display: "Filtered history clearing not yet implemented"

Example from `R/log.R` (lines 328-345):

```r
.log_clear_history <- function(build_type, before_date = NULL, before_version = NULL) {
  history_file <- .log_file_get_history(build_type)

  if (!file.exists(history_file)) {
    return(invisible(NULL))
  }

  # If no filters, just delete the file
  if (is.null(before_date) && is.null(before_version)) {
    unlink(history_file)
    return(invisible(NULL))
  }

  # TODO: Implement filtered deletion based on date/version
  # For now, if filters are specified, we don't delete
  cli::cli_alert_info("Filtered history clearing not yet implemented")
  invisible(NULL)
}
```

### Desired Behavior

The log cleanup helpers should respect the optional filtering parameters:

1. **Date-based filtering** (`before_date`):
   - Parse build log entries and date directories to extract timestamps
   - Delete only log entries with timestamps older than the specified date
   - Keep logs from the specified date and newer

2. **Version-based filtering** (`before_version`):
   - Parse build log entries to extract version information
   - Delete only log entries associated with versions strictly before the specified version
   - Keep logs from the specified version and newer
   - Handle version comparison according to the project's version format (as defined in `metadata.version-format`)

3. **Combined filtering**:
   - When both `before_date` and `before_version` are specified, apply both filters (logical AND)
   - Only delete entries that satisfy both conditions

4. **Backward compatibility**:
   - When both parameters are NULL, maintain current behavior (delete all logs)
   - Existing function signatures should not change

### Implementation Considerations

**For `.log_clear_history()`:**

- Parse the `builds.md` history file to extract individual build entries
- Each entry contains metadata including timestamp and version
- Filter entries based on the provided criteria
- Rewrite the history file with only the entries to keep
- If all entries are deleted, remove the file entirely

**For `.log_clear_output()`:**

- Continue using `.log_clear_output_by_date()` for date-based filtering (already partially implemented)
- Implement version-based filtering by:
  - Listing all log files in the output directory structure
  - Parsing log file contents or filenames to determine associated version
  - Deleting files/directories associated with versions before the specified version

**Version comparison:**

- Utilize existing version comparison utilities in the package
- Consider the configured `metadata.version-format` from `_projr.yml`
- Handle development versions (e.g., "0.0.1-dev") appropriately

### Acceptance Criteria

1. `.log_clear_history()` correctly filters history entries by date when `before_date` is provided
2. `.log_clear_history()` correctly filters history entries by version when `before_version` is provided
3. `.log_clear_output()` correctly filters output log directories/files by date when `before_date` is provided
4. `.log_clear_output()` correctly filters output log directories/files by version when `before_version` is provided
5. Both functions handle combined date and version filters correctly
6. Backward compatibility is maintained (NULL parameters clear all logs)
7. Comprehensive tests cover all filtering scenarios:
   - Single parameter (date only, version only)
   - Combined parameters
   - Edge cases (no logs, all logs match filter, no logs match filter)
   - Different date formats and version formats
8. The informational messages "not yet implemented" are removed
9. User documentation is updated to reflect the new filtering capabilities

### Related Files

- `R/log.R` - Primary implementation file
- Tests to be added in `tests/testthat/test-log.R` or similar
- User-facing documentation in function roxygen2 comments

---

## Issue 2: Generalize Quarto Website Navigation List Wrapping

### Summary

The current Quarto website configuration helper (`.yml_quarto_set()`) only wraps a subset of navigation keys as lists. When additional navigation or navbar elements are added to `_quarto.yml`, some keys may remain scalars instead of lists, leading to malformed YAML structures that Quarto cannot parse correctly. This enhancement seeks to generalize list wrapping to all relevant navigation keys.

### Current Behavior

Location: `R/yml-quarto.R`

The function `.yml_quarto_set()` currently wraps specific keys as lists (lines 20-34):

```r
.yml_quarto_set <- function(list_save) {
  path_yml <- .path_get("_quarto.yml")

  # Ensure that specific YAML keys are stored as lists rather than strings
  list_save <- .wrap_in_list_if_string(list_save, c("project", "render"))

  # Ensure 'website' related keys are lists
  list_save <- .wrap_in_list_if_string(list_save, c("website", "other-links"))
  list_save <- .wrap_in_list_if_string(list_save, c("website", "code-links"))
  list_save <- .wrap_in_list_if_string(
    list_save, c("website", "navbar", "right")
  )
  list_save <- .wrap_in_list_if_string(
    list_save, c("website", "navbar", "left")
  )

  # TODO: Extend handling for additional `website` navigation items as needed
  
  # ...
}
```

The TODO comment on line 34 explicitly acknowledges that additional navigation items need handling.

**Specific limitations:**

1. Only `navbar.right` and `navbar.left` are wrapped as lists
2. Other navbar properties (e.g., `navbar.tools`, `navbar.background`, `navbar.search`) are not handled
3. Sidebar navigation elements (e.g., `website.sidebar`, `website.sidebar.contents`) are not wrapped
4. Page navigation (e.g., `website.page-navigation`) is not wrapped
5. Other website configuration arrays (e.g., `website.reader-mode`, `website.repo-actions`) are not wrapped

### Desired Behavior

The Quarto YAML configuration writer should generalize list wrapping to handle all relevant navigation and configuration keys that Quarto expects as arrays/lists, not just the currently hard-coded subset.

**Comprehensive list of keys that should be wrapped as lists:**

**Navbar-related:**

- `website.navbar.left` (currently handled)
- `website.navbar.right` (currently handled)
- `website.navbar.tools`
- `website.navbar.pinned`

**Sidebar-related:**

- `website.sidebar` (when it's an array of sidebar configurations)
- `website.sidebar.contents` (when sidebar is a single object)
- `website.sidebar.tools`

**Other website navigation:**

- `website.page-navigation` (can be boolean or object, but certain sub-keys may need list wrapping)
- `website.repo-actions`
- `website.reader-mode`
- `website.other-links` (currently handled)
- `website.code-links` (currently handled)

**General approach:**

Rather than hard-coding every possible key, the implementation should:

1. Identify all keys in the Quarto YAML specification that accept arrays
2. Apply list wrapping consistently using a configuration-driven approach
3. Handle nested structures appropriately
4. Ensure backward compatibility with existing configurations

### Implementation Considerations

**Option 1: Comprehensive key list**

Maintain an explicit list of all navigation-related keys that should be wrapped:

```r
quarto_list_keys <- list(
  c("project", "render"),
  c("website", "navbar", "left"),
  c("website", "navbar", "right"),
  c("website", "navbar", "tools"),
  c("website", "navbar", "pinned"),
  c("website", "sidebar"),
  c("website", "sidebar", "contents"),
  c("website", "sidebar", "tools"),
  c("website", "other-links"),
  c("website", "code-links"),
  c("website", "repo-actions"),
  c("website", "reader-mode"),
  c("book", "chapters"),
  c("book", "appendices")
)

for (key_path in quarto_list_keys) {
  list_save <- .wrap_in_list_if_string(list_save, key_path)
}
```

**Option 2: Schema-based validation**

Create a schema definition that describes which Quarto YAML keys should be lists, and apply wrapping based on the schema.

**Option 3: Intelligent type inference**

Examine the YAML structure and intelligently determine which keys should be lists based on their content and common Quarto patterns.

### Acceptance Criteria

1. All documented Quarto website navigation keys are correctly wrapped as lists when they are strings
2. Adding any combination of navbar, sidebar, or other navigation elements produces valid `_quarto.yml` files
3. The TODO comment is removed
4. Tests verify correct wrapping for:
   - All navbar variants (left, right, tools, pinned)
   - Sidebar configurations (both single and multiple sidebars)
   - Other website navigation keys
5. Backward compatibility is maintained for existing configurations
6. Edge cases are handled:
   - Keys that are already lists remain unchanged
   - Keys that are NULL or absent are not affected
   - Nested structures are properly preserved
7. Documentation is updated with examples of supported navigation configurations

### Related Files

- `R/yml-quarto.R` - Primary implementation file
- Tests to be added in `tests/testthat/test-yml-quarto.R` or similar
- Helper function `.wrap_in_list_if_string()` may need enhancement

### Reference

- [Quarto Website Documentation](https://quarto.org/docs/websites/)
- [Quarto Website Options Reference](https://quarto.org/docs/reference/projects/websites.html)

---

## Issue 3: Replace Bookdown Template Placeholders with Real Project Metadata

### Summary

The Bookdown configuration templates currently include TODO placeholders for project descriptions and repository links. These placeholders are propagated into generated projects unchanged, resulting in incomplete and unprofessional project scaffolding. This enhancement request seeks to automatically populate these templates with real project metadata during initialization.

### Current Behavior

Location: `R/misc.R` (function `.init_engine_bookdown_contents_bookdown()`)

The Bookdown template generation (lines 781-799) contains hard-coded TODO placeholders:

```r
.init_engine_bookdown_contents_bookdown <- function() {
  c(
    "bookdown::gitbook:",
    "  toc_depth: 6",
    "  css: style.css",
    "  config:",
    "    toc:",
    "      before: |",
    "        <li><a href=\"./\">TODO: ADD SHORT DESCRIPTION</a></li>",
    "      after: |",
    "        <li><a href=\"https://github.com/[GITHUB_USER]/[GITHUB_REPO]\" target=\"blank\">SATVILab/TODO:_ADD_REPO_NAME</a></li>",
    "    download: [\"pdf\", \"epub\"]",
    "bookdown::pdf_book:",
    "  latex_engine: xelatex",
    "  citation_package: natbib",
    "  keep_tex: yes",
    "bookdown::epub_book: default"
  )
}
```

Similar placeholders exist in:

- `init_engine_bookdown_contents_index()` (lines 814-830): Contains `[Title]` and `[Author]` placeholders
- Related Quarto templates in `.init_engine_quarto_projects_content_yml()` (lines 832-850): Contains `[Title]` placeholder

**Specific placeholders:**

1. `TODO: ADD SHORT DESCRIPTION` - Project description in TOC
2. `[GITHUB_USER]/[GITHUB_REPO]` - GitHub repository link
3. `SATVILab/TODO:_ADD_REPO_NAME` - Specific repository name
4. `[Title]` - Project title
5. `[Author]` - Project author/maintainer

These placeholders appear in generated `_bookdown.yml` and `_output.yml` files when users initialize a Bookdown project.

### Desired Behavior

During project initialization, the Bookdown templates should be populated with actual project metadata instead of TODO placeholders. The metadata should be sourced from:

1. **Project title**: From `_projr.yml` configuration or user prompt
2. **Project description**: From `_projr.yml` configuration, `DESCRIPTION` file, or user prompt
3. **Repository information**: From Git remote configuration or `_projr.yml`
4. **Author information**: From Git configuration (`user.name`, `user.email`) or user prompt

**Template population flow:**

1. When `projr_init()` or related initialization functions are called
2. Gather metadata from available sources (config files, Git, user input)
3. Pass metadata to template generation functions as parameters
4. Replace placeholders with actual values in the generated files
5. Gracefully handle missing metadata:
   - Fall back to reasonable defaults
   - Prompt user for critical information if in interactive mode
   - Use package name or directory name as fallback for title
   - Use system user or "Project Author" as fallback for author

### Implementation Considerations

**Metadata gathering:**

Create a metadata collection function that gathers information from multiple sources:

```r
.init_gather_metadata <- function() {
  list(
    title = .init_get_project_title(),      # From _projr.yml or directory name
    description = .init_get_description(),   # From _projr.yml or DESCRIPTION file
    author = .init_get_author(),             # From Git config or prompt
    repo_user = .init_get_repo_user(),       # From Git remote
    repo_name = .init_get_repo_name()        # From Git remote or directory name
  )
}
```

**Template function signatures:**

Update template functions to accept metadata parameters:

```r
.init_engine_bookdown_contents_bookdown <- function(nm_list = NULL) {
  # nm_list should contain: title, description, author, repo_user, repo_name
  
  title <- nm_list$title %||% "Project Title"
  description <- nm_list$description %||% "Project Description"
  repo_user <- nm_list$repo_user %||% "username"
  repo_name <- nm_list$repo_name %||% "repository"
  
  # Generate template with actual values...
}
```

**Integration points:**

The initialization functions that call these templates need to be updated:

- `.init_engine_bookdown()` in `R/init-helper-engine.R`
- Other initialization helpers that generate document templates

**Fallback strategy:**

1. Check `_projr.yml` for project metadata section
2. Check Git configuration for author and remote repository
3. Check `DESCRIPTION` file (if it exists) for package metadata
4. Use directory name as project name fallback
5. In interactive mode, prompt user for missing critical information
6. Use sensible defaults for non-critical information

### Acceptance Criteria

1. Bookdown templates are populated with real project metadata during initialization
2. No TODO placeholders appear in generated `_bookdown.yml` or `_output.yml` files
3. GitHub repository links are correctly formatted with actual user/repo names
4. Project title and description are populated from available sources
5. Author information is populated from Git config or user input
6. Graceful fallback behavior when metadata is not available:
   - Non-interactive mode: Uses defaults
   - Interactive mode: Prompts for critical information
7. Metadata can be configured via `_projr.yml` before initialization
8. Tests verify correct template population:
   - With full metadata available
   - With partial metadata (some sources missing)
   - With no metadata (pure defaults)
   - In both interactive and non-interactive modes
9. Documentation updated to describe:
   - How to pre-configure project metadata
   - What sources are checked for metadata
   - Fallback behavior
10. Similar placeholders in Quarto templates are also addressed

### Related Files

- `R/misc.R` - Template content functions
- `R/init-helper-engine.R` - Template usage during initialization
- `R/init-std.R` - Standard initialization flow
- Tests to be added in `tests/testthat/test-init-engine.R` or similar

### Additional Considerations

**YAML configuration schema:**

Consider adding a `metadata` section to `_projr.yml`:

```yaml
metadata:
  title: "My Research Project"
  description: "Analysis of XYZ dataset"
  author: "Researcher Name"
  repository:
    user: "myusername"
    name: "myrepo"
```

This would provide explicit control over template metadata while maintaining backward compatibility (fallback to auto-detection if not specified).

---

## Contributing

These issues are documented to facilitate future implementation by contributors or automated development agents. When implementing these enhancements:

1. Follow the existing code style and conventions in the projr package
2. Add comprehensive tests for all new functionality
3. Update user-facing documentation (roxygen2 comments, vignettes)
4. Ensure backward compatibility is maintained
5. Run `devtools::check()` to verify package integrity
6. Add entries to `NEWS.md` describing the changes

For questions or clarification on these issues, please open a GitHub issue in the [SATVILab/projr repository](https://github.com/SATVILab/projr/issues).
