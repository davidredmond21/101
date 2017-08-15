library(climr)

context("checks that load_climr loads ok")

test_that("load_climr produces valid data", {
  expect_output(str(load_clim('SH')), 'List of 4')
  expect_error(load_clim('BLA'))
  expect_is(load_clim('NH'), 'climr')
  # Here's one that fails
  #expect_output(str(load_clim('SH')), 'List of 7')
})
