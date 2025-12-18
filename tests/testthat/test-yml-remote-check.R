test_that(".yml_remote_check validates required parameters", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Missing role should fail
  expect_error(
    .yml_remote_check(
      role = NULL,
      type = "local",
      content = "output"
    ),
    "role"
  )

  # Missing type should fail
  expect_error(
    .yml_remote_check(
      role = "build",
      type = NULL,
      content = "output"
    ),
    "type"
  )

  # Missing content should fail
  expect_error(
    .yml_remote_check(
      role = "build",
      type = "local",
      content = NULL
    ),
    "content"
  )

  # Valid required parameters should pass
  expect_true(
    .yml_remote_check(
      role = "build",
      type = "local",
      content = "output",
      path = "/tmp/test",
      get_list = list(),
      send_list = list()
    )
  )
})

test_that(".yml_remote_check validates optional string parameters", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Valid title should pass
  expect_true(
    .yml_remote_check(
      role = "build",
      type = "local",
      content = "output",
      path = "/tmp/test",
      title = "my-title",
      get_list = list(),
      send_list = list()
    )
  )

  # Valid description should pass
  expect_true(
    .yml_remote_check(
      role = "build",
      type = "local",
      content = "output",
      path = "/tmp/test",
      description = "A test description",
      get_list = list(),
      send_list = list()
    )
  )

  # Invalid title (not string) should fail
  expect_error(
    .yml_remote_check(
      role = "build",
      type = "local",
      content = "output",
      path = "/tmp/test",
      title = c("one", "two"),
      get_list = list(),
      send_list = list()
    )
  )

  # Invalid description (not string) should fail
  expect_error(
    .yml_remote_check(
      role = "build",
      type = "local",
      content = "output",
      path = "/tmp/test",
      description = 123,
      get_list = list(),
      send_list = list()
    )
  )
})

test_that(".yml_remote_check validates content parameter", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Valid content should pass
  expect_true(
    .yml_remote_check(
      role = "build",
      type = "local",
      content = "output",
      path = "/tmp/test",
      get_list = list(),
      send_list = list()
    )
  )

  expect_true(
    .yml_remote_check(
      role = "build",
      type = "local",
      content = c("output", "cache"),
      path = "/tmp/test",
      get_list = list(),
      send_list = list()
    )
  )

  # GitHub type allows "code" content
  expect_true(
    .yml_remote_check(
      role = "build",
      type = "github",
      content = "code",
      get_list = list(),
      send_list = list()
    )
  )

  # Invalid content should fail
  expect_error(
    .yml_remote_check(
      role = "build",
      type = "local",
      content = "invalid_label",
      path = "/tmp/test",
      get_list = list(),
      send_list = list()
    ),
    "invalid_label"
  )
})

test_that(".yml_remote_check validates structure parameter", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Valid structures should pass
  expect_true(
    .yml_remote_check(
      role = "build",
      type = "local",
      content = "output",
      path = "/tmp/test",
      structure = "archive",
      get_list = list(),
      send_list = list()
    )
  )

  expect_true(
    .yml_remote_check(
      role = "build",
      type = "local",
      content = "output",
      path = "/tmp/test",
      structure = "latest",
      get_list = list(),
      send_list = list()
    )
  )

  # Invalid structure should fail
  expect_error(
    .yml_remote_check(
      role = "build",
      type = "local",
      content = "output",
      path = "/tmp/test",
      structure = "invalid",
      get_list = list(),
      send_list = list()
    ),
    "invalid"
  )
})

test_that(".yml_remote_check validates path parameter for local type", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Path required for local type
  expect_error(
    .yml_remote_check(
      role = "build",
      type = "local",
      content = "output",
      path = NULL,
      get_list = list(),
      send_list = list()
    ),
    "path"
  )

  # Valid path for local type should pass
  expect_true(
    .yml_remote_check(
      role = "build",
      type = "local",
      content = "output",
      path = "/tmp/test",
      get_list = list(),
      send_list = list()
    )
  )

  # Path not required for github type
  expect_true(
    .yml_remote_check(
      role = "build",
      type = "github",
      content = "output",
      path = NULL,
      get_list = list(),
      send_list = list()
    )
  )

})

test_that(".yml_remote_check validates flag parameters", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Valid path_append_label should pass
  expect_true(
    .yml_remote_check(
      role = "build",
      type = "local",
      content = "output",
      path = "/tmp/test",
      path_append_label = TRUE,
      get_list = list(),
      send_list = list()
    )
  )

  expect_true(
    .yml_remote_check(
      role = "build",
      type = "local",
      content = "output",
      path = "/tmp/test",
      path_append_label = FALSE,
      get_list = list(),
      send_list = list()
    )
  )

  # Valid overwrite should pass
  expect_true(
    .yml_remote_check(
      role = "build",
      type = "local",
      content = "output",
      path = "/tmp/test",
      overwrite = TRUE,
      get_list = list(),
      send_list = list()
    )
  )

  expect_true(
    .yml_remote_check(
      role = "build",
      type = "local",
      content = "output",
      path = "/tmp/test",
      overwrite = FALSE,
      get_list = list(),
      send_list = list()
    )
  )

  # Invalid path_append_label (not logical) should fail
  expect_error(
    .yml_remote_check(
      role = "build",
      type = "local",
      content = "output",
      path = "/tmp/test",
      path_append_label = "yes",
      get_list = list(),
      send_list = list()
    )
  )

  # Invalid overwrite (not logical) should fail
  expect_error(
    .yml_remote_check(
      role = "build",
      type = "local",
      content = "output",
      path = "/tmp/test",
      overwrite = "no",
      get_list = list(),
      send_list = list()
    )
  )
})

test_that(".yml_remote_check validates id and id_parent parameters", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Valid id should pass (exactly 5 characters)
  expect_true(
    .yml_remote_check(
      role = "build",
      type = "github",
      content = "output",
      id = "v1.00",
      get_list = list(),
      send_list = list()
    )
  )

  expect_true(
    .yml_remote_check(
      role = "build",
      type = "github",
      content = "output",
      id = "12345",
      get_list = list(),
      send_list = list()
    )
  )

  # id too short should fail (less than 5 characters)
  expect_error(
    .yml_remote_check(
      role = "build",
      type = "github",
      content = "output",
      id = "v1.0",
      get_list = list(),
      send_list = list()
    )
  )

  # id too long should fail (more than 5 characters)
  expect_error(
    .yml_remote_check(
      role = "build",
      type = "github",
      content = "output",
      id = "v1.0.0",
      get_list = list(),
      send_list = list()
    )
  )

  # Invalid id (not string) should fail
  expect_error(
    .yml_remote_check(
      role = "build",
      type = "github",
      content = "output",
      id = c("12345", "67890"),
      get_list = list(),
      send_list = list()
    )
  )
})

test_that(".yml_remote_check validates get_list parameter", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Empty get_list should pass
  expect_true(
    .yml_remote_check(
      role = "build",
      type = "local",
      content = "output",
      path = "/tmp/test",
      get_list = list(),
      send_list = list()
    )
  )

  # Valid get_list with strategy should pass
  expect_true(
    .yml_remote_check(
      role = "build",
      type = "local",
      content = "output",
      path = "/tmp/test",
      get_list = list(strategy = "upload-missing"),
      send_list = list()
    )
  )

  expect_true(
    .yml_remote_check(
      role = "build",
      type = "local",
      content = "output",
      path = "/tmp/test",
      get_list = list(strategy = "sync-diff"),
      send_list = list()
    )
  )

  # Valid get_list with conflict should pass
  expect_true(
    .yml_remote_check(
      role = "build",
      type = "local",
      content = "output",
      path = "/tmp/test",
      get_list = list(conflict = "overwrite"),
      send_list = list()
    )
  )

  expect_true(
    .yml_remote_check(
      role = "build",
      type = "local",
      content = "output",
      path = "/tmp/test",
      get_list = list(conflict = "skip"),
      send_list = list()
    )
  )

  # Valid get_list with both strategy and conflict should pass
  expect_true(
    .yml_remote_check(
      role = "build",
      type = "local",
      content = "output",
      path = "/tmp/test",
      get_list = list(
        strategy = "upload-all",
        conflict = "error"
      ),
      send_list = list()
    )
  )

  # Invalid get_list (not a list) should fail
  expect_error(
    .yml_remote_check(
      role = "build",
      type = "local",
      content = "output",
      path = "/tmp/test",
      get_list = "not-a-list",
      send_list = list()
    )
  )

  # Invalid get_list with wrong names should fail
  expect_error(
    .yml_remote_check(
      role = "build",
      type = "local",
      content = "output",
      path = "/tmp/test",
      get_list = list(invalid_name = "value"),
      send_list = list()
    ),
    "invalid_name"
  )

  # Invalid get_list with invalid strategy should fail
  expect_error(
    .yml_remote_check(
      role = "build",
      type = "local",
      content = "output",
      path = "/tmp/test",
      get_list = list(strategy = "invalid_strategy"),
      send_list = list()
    ),
    "invalid_strategy"
  )

  # Invalid get_list with invalid conflict should fail
  expect_error(
    .yml_remote_check(
      role = "build",
      type = "local",
      content = "output",
      path = "/tmp/test",
      get_list = list(conflict = "invalid_conflict"),
      send_list = list()
    ),
    "invalid_conflict"
  )
})

test_that(".yml_remote_check validates send_list parameter", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Empty send_list should pass
  expect_true(
    .yml_remote_check(
      role = "build",
      type = "local",
      content = "output",
      path = "/tmp/test",
      get_list = list(),
      send_list = list()
    )
  )

  # Valid send_list with cue should pass
  expect_true(
    .yml_remote_check(
      role = "build",
      type = "local",
      content = "output",
      path = "/tmp/test",
      get_list = list(),
      send_list = list(cue = "if-change")
    )
  )

  expect_true(
    .yml_remote_check(
      role = "build",
      type = "local",
      content = "output",
      path = "/tmp/test",
      get_list = list(),
      send_list = list(cue = "always")
    )
  )

  # Valid send_list with strategy should pass
  expect_true(
    .yml_remote_check(
      role = "build",
      type = "local",
      content = "output",
      path = "/tmp/test",
      get_list = list(),
      send_list = list(strategy = "sync-diff")
    )
  )

  expect_true(
    .yml_remote_check(
      role = "build",
      type = "local",
      content = "output",
      path = "/tmp/test",
      get_list = list(),
      send_list = list(strategy = "sync-purge")
    )
  )

  # Valid send_list with inspect should pass
  expect_true(
    .yml_remote_check(
      role = "build",
      type = "local",
      content = "output",
      path = "/tmp/test",
      get_list = list(),
      send_list = list(inspect = "manifest")
    )
  )

  expect_true(
    .yml_remote_check(
      role = "build",
      type = "local",
      content = "output",
      path = "/tmp/test",
      get_list = list(),
      send_list = list(inspect = "file")
    )
  )

  expect_true(
    .yml_remote_check(
      role = "build",
      type = "local",
      content = "output",
      path = "/tmp/test",
      get_list = list(),
      send_list = list(inspect = "none")
    )
  )

  # Valid send_list with conflict should pass
  expect_true(
    .yml_remote_check(
      role = "build",
      type = "local",
      content = "output",
      path = "/tmp/test",
      get_list = list(),
      send_list = list(conflict = "skip")
    )
  )

  # Valid send_list with all parameters should pass
  expect_true(
    .yml_remote_check(
      role = "build",
      type = "local",
      content = "output",
      path = "/tmp/test",
      get_list = list(),
      send_list = list(
        cue = "always",
        strategy = "upload-all",
        conflict = "overwrite",
        inspect = "file"
      )
    )
  )

  # Invalid send_list (not a list) should fail
  expect_error(
    .yml_remote_check(
      role = "build",
      type = "local",
      content = "output",
      path = "/tmp/test",
      get_list = list(),
      send_list = "not-a-list"
    )
  )

  # Invalid send_list with wrong names should fail
  expect_error(
    .yml_remote_check(
      role = "build",
      type = "local",
      content = "output",
      path = "/tmp/test",
      get_list = list(),
      send_list = list(invalid_name = "value")
    ),
    "invalid_name"
  )

  # Invalid send_list with invalid cue should fail
  expect_error(
    .yml_remote_check(
      role = "build",
      type = "local",
      content = "output",
      path = "/tmp/test",
      get_list = list(),
      send_list = list(cue = "invalid_cue")
    ),
    "invalid_cue"
  )

  # Invalid send_list with invalid strategy should fail
  expect_error(
    .yml_remote_check(
      role = "build",
      type = "local",
      content = "output",
      path = "/tmp/test",
      get_list = list(),
      send_list = list(strategy = "invalid_strategy")
    ),
    "invalid_strategy"
  )

  # Invalid send_list with invalid conflict should fail
  expect_error(
    .yml_remote_check(
      role = "build",
      type = "local",
      content = "output",
      path = "/tmp/test",
      get_list = list(),
      send_list = list(conflict = "invalid_conflict")
    ),
    "invalid_conflict"
  )

  # Invalid send_list with invalid inspect should fail
  expect_error(
    .yml_remote_check(
      role = "build",
      type = "local",
      content = "output",
      path = "/tmp/test",
      get_list = list(),
      send_list = list(inspect = "invalid_inspect")
    ),
    "invalid_inspect"
  )
})

test_that(".yml_remote_check validates combinations for different remote types", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Valid local remote with all options
  expect_true(
    .yml_remote_check(
      role = "build",
      type = "local",
      title = "local-test",
      content = "output",
      structure = "archive",
      path = "/tmp/test",
      path_append_label = TRUE,
      overwrite = FALSE,
      get_list = list(),
      send_list = list(
        cue = "if-change",
        strategy = "sync-diff",
        inspect = "manifest"
      )
    )
  )

  # Valid github remote with all options
  expect_true(
    .yml_remote_check(
      role = "build",
      type = "github",
      title = "github-test",
      content = c("output", "code"),
      structure = "latest",
      id = "v1.00",
      description = "Test release",
      get_list = list(
        strategy = "upload-missing",
        conflict = "skip"
      ),
      send_list = list()
    )
  )
})

test_that(".yml_remote_check allows NULL for optional parameters", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # All optional parameters as NULL should pass (except required ones)
  expect_true(
    .yml_remote_check(
      role = "build",
      type = "github",
      content = "output",
      title = NULL,
      structure = NULL,
      path = NULL,
      path_append_label = NULL,
      overwrite = FALSE,
      public = NULL,
      category = NULL,
      description = NULL,
      id = NULL,
      id_parent = NULL,
      get_list = list(),
      send_list = list()
    )
  )
})
