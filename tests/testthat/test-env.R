context("Initializing Charts within an Environment")

test_that("initializing charts within an environment works", {
  test_data <- make_test_data()

  env <- epivizEnv()
  env_mgr <- env$get_data_mgr()

  blocks_chart <- epivizChart(test_data$blocks, parent=env)
  expect_that(blocks_chart$get_data_mgr(), is_identical_to(env_mgr))

  line_chart <- epivizChart(test_data$line, type="bp", parent=env)
  expect_that(line_chart$get_data_mgr(), is_identical_to(env_mgr))

  eset_plot <- epivizChart(test_data$eset, parent=env)
  expect_that(eset_plot$get_data_mgr(), is_identical_to(env_mgr))

  se_plot <- epivizChart(test_data$se, parent=env)
  expect_that(se_plot$get_data_mgr(), is_identical_to(env_mgr))

  env_charts <- lapply(env$charts, function(chart) chart$get_id())

  expect_true(blocks_chart$get_id() %in% env_charts)
  expect_true(line_chart$get_id() %in% env_charts)
  expect_true(eset_plot$get_id() %in% env_charts)
  expect_true(se_plot$get_id() %in% env_charts)
})

test_that("removing charts from an environment works", {
  test_data <- make_test_data()

  env <- epivizEnv()

  blocks_chart <- epivizChart(test_data$blocks, parent=env)
  line_chart <- epivizChart(test_data$line, type="bp", parent=env)
  eset_plot <- epivizChart(test_data$eset, parent=env)
  se_plot <- epivizChart(test_data$se, parent=env)

  env_charts <- lapply(env$charts, function(chart) chart$get_id())

  expect_true(blocks_chart$get_id() %in% env_charts)
  expect_true(line_chart$get_id() %in% env_charts)
  expect_true(eset_plot$get_id() %in% env_charts)
  expect_true(se_plot$get_id() %in% env_charts)

  env$remove_all_charts()
  
  env_charts <- lapply(env$charts, function(chart) chart$get_id())

  expect_false(blocks_chart$get_id() %in% env_charts)
  expect_false(line_chart$get_id() %in% env_charts)
  expect_false(eset_plot$get_id() %in% env_charts)
  expect_false(se_plot$get_id() %in% env_charts)
})
