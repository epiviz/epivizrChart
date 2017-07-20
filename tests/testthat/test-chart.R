context("Initializing Charts")

test_that("initializing charts works with data objects", {
  test_data <- make_test_data()

  # EpivizBlockData --------------------------------------------------------------
  blocks_track <- epivizChart(test_data$blocks, chr="chr1")
  expect_that(blocks_track, is_a("EpivizChart"))

  blocks_ms <- blocks_track$get_measurements()[[1]]
  expect_that(blocks_ms, is_a("EpivizMeasurement"))

  blocks_mgr <- blocks_track$get_data_mgr()
  expect_that(blocks_mgr, is_a("EpivizChartDataMgr"))

  blocks_ms_obj <- blocks_mgr$.get_ms_object(blocks_ms@datasourceId)
  expect_that(blocks_ms_obj, is_a("EpivizBlockData"))

  blocks_default <- blocks_ms_obj$get_default_chart_type_html()
  expect_that(blocks_track$get_name(), equals(blocks_default))

  # EpivizBpData --------------------------------------------------------------
  line_track <- epivizChart(test_data$line, type="bp",
    columns="score", chr="chr1")
  expect_that(line_track, is_a("EpivizChart"))

  line_ms <- line_track$get_measurements()[[1]]
  expect_that(line_ms, is_a("EpivizMeasurement"))

  line_mgr <- line_track$get_data_mgr()
  expect_that(line_mgr, is_a("EpivizChartDataMgr"))

  line_ms_obj <- line_mgr$.get_ms_object(line_ms@datasourceId)
  expect_that(line_ms_obj, is_a("EpivizBpData"))

  line_default <- line_ms_obj$get_default_chart_type_html()
  expect_that(line_track$get_name(), equals(line_default))

  # ExpressionSet -------------------------------------------------------------
  eset_plot <- epivizChart(test_data$eset)
  expect_that(eset_plot, is_a("EpivizChart"))

  eset_ms <- eset_plot$get_measurements()[[1]]
  expect_that(line_ms, is_a("EpivizMeasurement"))

  eset_mgr <- eset_plot$get_data_mgr()
  expect_that(eset_mgr, is_a("EpivizChartDataMgr"))

  eset_ms_obj <- eset_mgr$.get_ms_object(eset_ms@datasourceId)
  expect_that(eset_ms_obj, is_a("EpivizFeatureData"))

  eset_default <- eset_ms_obj$get_default_chart_type_html()
  expect_that(eset_plot$get_name(), equals(eset_default))

  # SummarizedExperiment ------------------------------------------------------
  se_plot <- epivizChart(test_data$se)
  expect_that(se_plot, is_a("EpivizChart"))

  se_ms <- se_plot$get_measurements()[[1]]
  expect_that(se_ms, is_a("EpivizMeasurement"))

  se_mgr <- se_plot$get_data_mgr()
  expect_that(se_mgr, is_a("EpivizChartDataMgr"))

  se_ms_obj <- se_mgr$.get_ms_object(se_ms@datasourceId)
  expect_that(se_ms_obj, is_a("EpivizFeatureData"))

  se_default <- se_ms_obj$get_default_chart_type_html()
  expect_that(se_plot$get_name(), equals(se_default))
})

test_that("initializing charts works with measurements", {
  test_data <- make_test_data()

  env <- epivizEnv()

  eset_plot <- epivizChart(test_data$eset, parent=env)
  expect_that(eset_plot, is_a("EpivizChart"))

  eset_ms <- eset_plot$get_measurements()
  heatmap_plot <- epivizChart(measurements=eset_ms, chart="HeatmapPlot",
    parent=env)

  expect_that(heatmap_plot, is_a("EpivizChart"))
  expect_that(heatmap_plot$get_name(), equals("epiviz-json-heatmap-plot"))
})

test_that("revisualizing charts as a different chart type works", {
  test_data <- make_test_data()

  eset_plot <- epivizChart(test_data$eset)
  test_that(eset_plot$get_name(), equals("epiviz-json-scatter-plot"))

  eset_plot$revisualize("HeatmapPlot")
  test_that(eset_plot$get_name(), equals("epiviz-json-heatmap-plot"))
})

test_that("navigating chart to different region works", {
  test_data <- make_test_data()

  blocks_track <- epivizChart(test_data$blocks)
  test_that(blocks_track$get_start(), equals(NULL))
  test_that(blocks_track$get_end(), equals(NULL))

  blocks_track$navigate(chr="chr1", start=10, end=50)
  test_that(blocks_track$get_start(), equals(10))
  test_that(blocks_track$get_end(), equals(50))
})



