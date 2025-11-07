# GitHub Copilot Best Practices Guide

This document provides best practices for using GitHub Copilot coding agent with the projr repository, based on [GitHub's official documentation](https://docs.github.com/en/enterprise-cloud@latest/copilot/tutorials/coding-agent/get-the-best-results).

## Table of Contents

1. [Writing Well-Scoped Issues](#writing-well-scoped-issues)
2. [Choosing the Right Tasks](#choosing-the-right-tasks)
3. [Custom Instructions](#custom-instructions)
4. [Custom Agents](#custom-agents)
5. [Development Environment Setup](#development-environment-setup)
6. [Iterating on Pull Requests](#iterating-on-pull-requests)

## Writing Well-Scoped Issues

GitHub Copilot provides better results when assigned clear, well-scoped tasks. When creating issues for Copilot to work on, think of the issue description as a prompt for an AI agent.

### An ideal task includes:

- **Clear description**: Explain the problem to be solved or the work required
- **Complete acceptance criteria**: What does a good solution look like?
  - Should there be unit tests?
  - Should documentation be updated?
  - Are there specific coding standards to follow?
- **File directions**: Which files need to be changed or where new files should be created

### Example of a well-scoped issue:

```markdown
## Problem
The `projr_build_manifest()` function doesn't handle missing YAML files gracefully.

## Acceptance Criteria
- [ ] Add error handling for missing manifest files
- [ ] Return informative error message with file path
- [ ] Add unit tests in `tests/testthat/test-build-manifest.R`
- [ ] Follow existing error handling patterns in the codebase

## Files to Change
- `R/build-manifest.R` - Add error handling
- `tests/testthat/test-build-manifest.R` - Add tests
```

## Choosing the Right Tasks

### Good tasks for Copilot:

✅ **Simple and focused changes**
- Fix bugs in specific functions
- Update documentation or vignettes
- Improve test coverage for existing functions
- Refactor small, isolated code sections
- Add error handling to existing functions
- Update dependencies in DESCRIPTION

✅ **Medium complexity tasks**
- Add new utility functions with clear specifications
- Implement new methods following existing patterns
- Add new test files for uncovered modules
- Update workflows in `.github/workflows/`

### Tasks to handle yourself:

❌ **Complex and broadly scoped**
- Major refactoring across multiple R files
- Redesigning core package architecture
- Changes requiring deep domain knowledge of the project's scientific context

❌ **Sensitive and critical**
- Authentication and credential handling
- Publishing releases to CRAN
- Changes to CI/CD that affect production deployments

❌ **Ambiguous tasks**
- Open-ended feature requests without clear requirements
- Tasks requiring exploration and design decisions

## Custom Instructions

Custom instructions guide Copilot on how to understand your project and how to build, test, and validate its changes.

### Repository-Wide Instructions

Create a `.github/copilot-instructions.md` file in the root of your repository for instructions that apply to all tasks. This file is also used by Copilot Chat and Copilot code review.

#### Example structure for an R package:

```markdown
# Copilot Instructions for projr

This is an R package for facilitating reproducible and archived projects.

## Code Standards

### Required Before Each Commit
- Run `devtools::document()` to update documentation
- Run `devtools::check()` to ensure package passes R CMD check
- Ensure all tests pass with `devtools::test()`

### Development Flow
- Install dependencies: `renv::restore()`
- Load package: `devtools::load_all()`
- Test: `devtools::test()`
- Check: `devtools::check()`
- Build documentation: `devtools::document()`

## Repository Structure
- `R/`: R source code
- `tests/testthat/`: Unit tests using testthat
- `man/`: Auto-generated documentation (do not edit directly)
- `vignettes/`: Package vignettes
- `.github/workflows/`: GitHub Actions workflows
- `inst/`: Package installation files

## Key Guidelines
1. Follow [tidyverse style guide](https://style.tidyverse.org/)
2. All exported functions must have roxygen2 documentation
3. Write unit tests using testthat for new functionality
4. Use descriptive variable names
5. Add examples to function documentation
6. Update NEWS.md for user-facing changes
```

### Path-Specific Instructions

Create files in `.github/instructions/**/*.instructions.md` for specific file types.

#### Example: R function tests

Create `.github/instructions/r-tests.instructions.md`:

```markdown
---
applyTo: "tests/testthat/test-*.R"
---

## R Package Test Requirements

When writing tests for R functions, follow these guidelines:

1. **File naming**: Use `test-{source-file-name}.R` format
2. **Use testthat 3e**: All tests should use `test_that()` structure
3. **Test organization**: Group related tests in `describe()` blocks
4. **Use expect_* functions**: 
   - `expect_equal()` for exact matches
   - `expect_error()` for error conditions
   - `expect_warning()` for warnings
   - `expect_silent()` for no output
5. **Test edge cases**: Include tests for NULL, NA, empty inputs
6. **Mock external dependencies**: Use testthat mocking when needed
7. **Clean up**: Use `withr` package for temporary state changes
8. **Test isolation**: Each test should be independent
```

#### Example: R documentation

Create `.github/instructions/r-docs.instructions.md`:

```markdown
---
applyTo: "R/*.R"
---

## R Function Documentation Requirements

All exported functions must include roxygen2 documentation:

1. **Title and description**: One-line title, detailed description
2. **@param tags**: Document all parameters with types and descriptions
3. **@return tag**: Describe what the function returns
4. **@examples**: Provide working examples (use \dontrun{} if needed)
5. **@export**: Add for functions that should be exported
6. **@family tags**: Group related functions
7. **@seealso**: Link to related functions or documentation

Example:
```r
#' Build project manifest
#'
#' Creates a manifest file for the project based on configuration.
#'
#' @param path Character. Path to the project root.
#' @param force Logical. Overwrite existing manifest? Default FALSE.
#'
#' @return Invisibly returns the path to the created manifest file.
#'
#' @examples
#' \dontrun{
#' projr_build_manifest("~/my-project")
#' }
#'
#' @export
#' @family build functions
```
```

### Organization-Wide Custom Instructions

If your organization has custom instructions configured, Copilot coding agent will use them. Repository-wide instructions take priority over organization instructions.

## Custom Agents

Custom agents are specialized agents with focused expertise for specific, recurring workflows.

### When to Create Custom Agents

Consider creating custom agents for:

- **Testing specialist**: Focused on R package testing with testthat
- **Documentation expert**: Specialized in roxygen2 documentation and vignettes
- **R style enforcer**: Ensures code follows tidyverse style guide
- **Package maintenance**: Handles DESCRIPTION updates, dependency management

### Creating Custom Agents

Create agent profiles as Markdown files in `.github/agents/`. For more information, see [Creating custom agents](https://docs.github.com/en/enterprise-cloud@latest/copilot/how-tos/use-copilot-agents/coding-agent/create-custom-agents).

#### Example: Testing specialist agent

Create `.github/agents/testing-specialist.md`:

```markdown
---
name: testing-specialist
description: Specialized agent for writing comprehensive R package tests
tools:
  - read
  - search
  - edit
---

You are a testing specialist for R packages. Your expertise includes:

- Writing comprehensive testthat tests
- Following R package testing best practices
- Testing edge cases and error conditions
- Using mocking and fixtures appropriately
- Writing clear test descriptions

When writing tests:
1. Use descriptive test names
2. Test both success and failure cases
3. Include edge cases (NULL, NA, empty inputs)
4. Use appropriate expect_* functions
5. Ensure tests are isolated and reproducible
```

## Development Environment Setup

The `.github/workflows/copilot-setup-steps.yml` file pre-installs dependencies in Copilot's ephemeral development environment.

### Current Setup

The existing `copilot-setup-steps.yml` for this repository:

- Installs system dependencies (libcurl, libgit2, etc.)
- Sets up R (release version)
- Installs R package dependencies using renv
- Installs rcmdcheck for package checking

### Best Practices

1. **Keep it fast**: Pre-install only essential dependencies
2. **Use caching**: Leverage GitHub Actions caching for faster setup
3. **Match CI environment**: Keep similar to other workflow environments
4. **Test the setup**: Ensure the setup steps actually work

### Updating copilot-setup-steps.yml

If you need to add new dependencies or tools:

1. Add system dependencies in the apt-get install step
2. Add R packages to DESCRIPTION or renv.lock
3. Test that the setup works in a clean environment

## Iterating on Pull Requests

Working with Copilot on a pull request is like working with a human developer.

### How to Iterate

1. **Use @copilot mentions**: In PR comments, mention @copilot and explain what needs to change
2. **Batch comments**: Use "Start a review" to submit multiple comments at once
3. **Be specific**: Explain exactly what's incorrect or could be improved
4. **Review changes**: Copilot updates the PR title and body to reflect changes

### Example PR Comment

```markdown
@copilot The error handling in `build_manifest()` needs improvement:

1. The error message should include the file path that's missing
2. Add a suggestion for how to fix the problem
3. Use `cli::cli_abort()` instead of `stop()` for better error formatting
```

### Who Can Use @copilot

Only users with write access to the repository can trigger Copilot changes via comments.

## Tips for Success

1. **Start small**: Begin with simple tasks to understand how Copilot works
2. **Be explicit**: Clear instructions get better results
3. **Provide context**: Include relevant information about the codebase
4. **Review carefully**: Always review Copilot's changes before merging
5. **Iterate**: Don't expect perfection on the first try
6. **Use custom instructions**: They significantly improve results
7. **Pre-install dependencies**: Faster iterations with `copilot-setup-steps.yml`

## Additional Resources

- [GitHub Copilot documentation](https://docs.github.com/en/copilot)
- [About GitHub Copilot coding agent](https://docs.github.com/en/enterprise-cloud@latest/copilot/concepts/about-copilot-coding-agent)
- [Adding repository custom instructions](https://docs.github.com/en/enterprise-cloud@latest/copilot/customizing-copilot/adding-repository-custom-instructions-for-github-copilot)
- [Creating custom agents](https://docs.github.com/en/enterprise-cloud@latest/copilot/how-tos/use-copilot-agents/coding-agent/create-custom-agents)
- [Tidyverse Style Guide](https://style.tidyverse.org/)
- [R Packages Book](https://r-pkgs.org/)
