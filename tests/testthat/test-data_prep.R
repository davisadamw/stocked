test_that("basic center and scale works", {
  # check on very basic data
  expect_identical(center_and_scale(1:3), c(-1, 0, 1))

  # check on random data
  ck_data <- stats::rnorm(100, mean = 10, sd = 4)
  ck_data_cs <- center_and_scale(ck_data)
  ck_data_manual <- (ck_data - mean(ck_data)) / stats::sd(ck_data)

  # first, check the resulting distribution
  expect_equal(mean(ck_data_cs), 0)
  expect_equal(stats::sd(ck_data_cs), 1)

  expect_equal(ck_data_cs, ck_data_manual)

})

test_that("center and scale works across multiple variables", {

  test_data <- tibble(x1 = runif(100, min = 10, max = 20),
                      x2 = rnorm(100, mean = 10, sd = 2))

  test_data_cs <- test_data %>%
    center_and_scale_vars(x1, x2)

  expect_equal(mean(test_data_cs$x1), 0)
  expect_equal(sd(test_data_cs$x1), 1)
  expect_equal(mean(test_data_cs$x2), 0)
  expect_equal(sd(test_data_cs$x2), 1)
})

test_that("center and scale validation does what it's supposed to", {
  test_data <- tibble(x1 = runif(100, min = 100, max = 110))

  vald_data <- tibble(x1 = 1:3)

  test_data_cs <- test_data %>%
    center_and_scale_validation(training = vald_data, x1)

  # range of test_data x1 should go from (100-110) to (98-108)
  expect_gte(min(test_data_cs$x1), 98)
  expect_lte(min(test_data_cs$x1), 108)
  expect_gte(max(test_data_cs$x1), 98)
  expect_lte(max(test_data_cs$x1), 108)
})

test_that("data prep works as intended", {
  data_for_prep <- tibble(uid = LETTERS[1:10],
                          targ1 = runif(10),
                          targ2 = targ1 + 2,
                          pred1 = rnorm(10) * 2 + 1,
                          pred2 = rnorm(10) * 1 + 3,
                          col4l = targ2 + 10,
                          excess = runif(10))

  data_prepped <- data_for_prep %>%
    prep_data(id_cols = uid,
              targ_cols = c(targ1, targ2),
              predictor_cols = c(pred1, pred2),
              ml_col = col4l)

  # make sure market limit went over correctly
  expect_equal(data_prepped$market_limit, data_for_prep$col4l)
  # and that the old column got dropped
  expect_false('col4l' %in% names(data_prepped))
  # make sure excess column got dropped
  expect_false('excess' %in% names(data_prepped))


})

test_that("the target calculation works", {
  tc_data <- tibble(x1 = 1:10, x2 = x1 + 1, x3 = x2 + 2)
  tc_ests <- calculate_target(tc_data, x1:x3)

  expect_equal(tc_ests$x2, 10)
  expect_equal(tc_ests$x3, 20)

})



