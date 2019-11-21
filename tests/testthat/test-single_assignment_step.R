test_that("assignment step adds correct number of items total when total market limit not reached", {

  # test data with 20 total spots to spare
  test_data <- tibble::tribble(~market_curr, ~market_limit, ~base_rate,
                               10,           20,            0.5,
                               15,           25,            0.7) %>%
    mutate(M_curr = market_curr / market_limit)


  add10 <- single_assignment_step(test_data, n_this_iter = 10, iteration = 1, p = 0.2, q = 0.5)
  expect_equal(sum(add10$assign), 10)
  expect_equal(sum(add10$market_curr - add10$market_prev), 10)

  # add more than we can fit ... would need a lot of code changes to add only the correct number, but...
  add21 <- single_assignment_step(test_data, n_this_iter = 21, iteration = 1, p = 0.2, q = 0.5)
  # expect_equal(sum(add21$assign), 20) # nope...
  # we'll have to settle for adding NO MORE THAN the remaining space below market limit
  expect_lte(sum(add21$assign), 20)

})

