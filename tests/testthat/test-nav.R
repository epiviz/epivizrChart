context("Initializing Charts within a Navigation")

test_that("initializing charts within a navigation works", {
  skip_on_bioc()
  test_data <- make_test_data()

  nav <- epivizNav(chr="chr1", start=1, end=100)
  nav_mgr <- nav$get_data_mgr()

  blocks_chart <- epivizChart(test_data$blocks, chr="chr1", parent=nav)
  expect_that(blocks_chart$get_data_mgr(), is_identical_to(nav_mgr))
  expect_that(blocks_chart$get_chr(), equals(nav$get_chr()))
  expect_that(blocks_chart$get_start(), equals(nav$get_start()))
  expect_that(blocks_chart$get_end(), equals(nav$get_end()))


  line_chart <- epivizChart(test_data$line, chr="chr1", type="bp", parent=nav)
  expect_that(line_chart$get_data_mgr(), is_identical_to(nav_mgr))
  expect_that(line_chart$get_chr(), equals(nav$get_chr()))
  expect_that(line_chart$get_start(), equals(nav$get_start()))
  expect_that(line_chart$get_end(), equals(nav$get_end()))

  nav_charts <- lapply(nav$charts, function(chart) chart$get_id())

  expect_true(blocks_chart$get_id() %in% nav_charts)
  expect_true(line_chart$get_id() %in% nav_charts)
})

test_that("navigating charts within a navigation works", {
  skip_on_bioc()
  test_data <- make_test_data()

  nav <- epivizNav(chr="chr1", start=1, end=100)
  expect_that(nav$get_chr(), equals("chr1"))
  expect_that(nav$get_start(), equals(1))
  expect_that(nav$get_end(), equals(100))

  blocks_chart <- epivizChart(test_data$blocks, chr="chr1", parent=nav)
  expect_that(blocks_chart$get_chr(), equals(nav$get_chr()))
  expect_that(blocks_chart$get_start(), equals(nav$get_start()))
  expect_that(blocks_chart$get_end(), equals(nav$get_end()))

  line_chart <- epivizChart(test_data$line, chr="chr1", type="bp", parent=nav)
  expect_that(line_chart$get_chr(), equals(nav$get_chr()))
  expect_that(line_chart$get_start(), equals(nav$get_start()))
  expect_that(line_chart$get_end(), equals(nav$get_end()))

  nav$navigate(chr="chr1", start=10, end=50)
  expect_that(nav$get_start(), equals(10))
  expect_that(nav$get_end(), equals(50))

  expect_that(blocks_chart$get_chr(), equals(nav$get_chr()))
  expect_that(blocks_chart$get_start(), equals(nav$get_start()))
  expect_that(blocks_chart$get_end(), equals(nav$get_end()))

  expect_that(line_chart$get_chr(), equals(nav$get_chr()))
  expect_that(line_chart$get_start(), equals(nav$get_start()))
  expect_that(line_chart$get_end(), equals(nav$get_end()))
})
