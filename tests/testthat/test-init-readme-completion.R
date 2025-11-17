test_that("README placeholders are replaced after .init_prompt_readme", {
  skip_if(.is_test_select())
  dir_test <- file.path(tempdir(), "testReadmePlaceholders")
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  .dir_create(dir_test)
  .test_set()
  withr::defer(.test_unset())
  withr::defer(unlink(dir_test, recursive = TRUE))

  usethis::with_project(
    path = dir_test,
    code = {
      nm_list_metadata <- list(
        pkg = "mytestpackage",
        first = "Test",
        last = "User",
        email = "test@example.com",
        title = "My Test Project"
      )

      # Call .init_prompt_readme
      nm_list_readme <- .init_prompt_readme(nm_list_metadata)

      # Verify README.md exists
      expect_true(file.exists("README.md"))

      # Read README content
      readme <- readLines("README.md", warn = FALSE)

      # Check that placeholders are replaced
      has_placeholders <- any(grepl("\\{\\{", readme))
      expect_false(has_placeholders)

      # Check that package name appears in README
      has_package_name <- any(grepl("mytestpackage", readme))
      expect_true(has_package_name)

      # Check that the purpose line is correct
      has_purpose <- any(grepl("The purpose of mytestpackage is to", readme))
      expect_true(has_purpose)

      # Check that answer_readme is numeric, not a list
      expect_true(is.numeric(nm_list_readme$answer_readme))
      expect_false(is.list(nm_list_readme$answer_readme))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_init_prompt creates README without placeholders", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- file.path(tempdir(), "testProjrInitPrompt")
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  .dir_create(dir_test)
  .test_set()
  withr::defer(.test_unset())
  withr::defer(unlink(dir_test, recursive = TRUE))

  usethis::with_project(
    path = dir_test,
    code = {
      # Call projr_init_prompt
      result <- tryCatch({
        projr_init_prompt()
        TRUE
      }, error = function(e) {
        FALSE
      })

      # Verify it completed successfully
      expect_true(result)

      # Verify README.md exists
      expect_true(file.exists("README.md"))

      # Read README content
      readme <- readLines("README.md", warn = FALSE)

      # Check that placeholders are replaced
      has_placeholders <- any(grepl("\\{\\{", readme))
      expect_false(has_placeholders)

      # The package name should be the directory name
      pkg_name <- basename(dir_test)
      has_package_name <- any(grepl(pkg_name, readme))
      expect_true(has_package_name)
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".init_prompt_readme_create returns numeric not list", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- file.path(tempdir(), "testReadmeCreateReturn")
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  .dir_create(dir_test)
  .test_set()
  withr::defer(.test_unset())
  withr::defer(unlink(dir_test, recursive = TRUE))

  usethis::with_project(
    path = dir_test,
    code = {
      # Call .init_prompt_readme_create in fresh directory
      result <- .init_prompt_readme_create()

      # Should return numeric, not list
      expect_true(is.numeric(result))
      expect_false(is.list(result))
      expect_equal(length(result), 1)

      # Should return 2 in test mode (markdown)
      expect_equal(result, 2)
    },
    force = TRUE,
    quiet = TRUE
  )
})
