test_that("correct number gets assigned", {
  md_bp <- minimal_data %>%
    calculate_bp(fixed_predictor = fixed_pred,
                 other_predictors = c(other_pred1, other_pred2),
                 coefficients = c("intercept" = 10, other_pred1 = 1, other_pred2 = 1),
                 id_col = location) %>%
    mutate(market_curr = val_start,
           M_curr      = val_start / market_limit)

  # old method
  assignment_check1 <- md_bp %>%
    run_assignment_old(n_to_add = 100,
                       tot_iters = 40,
                       p = 0.1, q = 0.4) %>%
    pull(market_curr)

  expect_equal(sum(assignment_check1), sum(md_bp$market_curr) + 100)

  # new method
  assignment_check2 <- md_bp %>%
    mutate(market_pred =
             run_assignment(market_current = val_start,
                            market_limit = market_limit,
                            base_rate = base_rate,
                            n_to_add = 100,
                            tot_iters = 40,
                            p = 0.1, q = 0.4)) %>%
    pull(market_pred)

  expect_equal(sum(assignment_check2), sum(md_bp$market_curr) + 100)

  expect_identical(assignment_check1, assignment_check2)

})

test_that("assignment performance is independent of evaluation namespace", {
  md_bp <- minimal_data %>%
    calculate_bp(fixed_predictor = fixed_pred,
                 other_predictors = c(other_pred1, other_pred2),
                 coefficients = c("intercept" = 10, other_pred1 = 1, other_pred2 = 1),
                 id_col = location)

  # new method on its own
  assignment_check1 <- run_assignment(market_current = md_bp$val_start,
                                      market_limit = md_bp$market_limit,
                                      base_rate = md_bp$base_rate,
                                      n_to_add = 100,
                                      tot_iters = 40,
                                      p = 0.1, q = 0.4)

  # new method in mutate
  assignment_check2 <- md_bp %>%
    mutate(market_pred =
             run_assignment(market_current = val_start,
                            market_limit = market_limit,
                            base_rate = base_rate,
                            n_to_add = 100,
                            tot_iters = 40,
                            p = 0.1, q = 0.4)) %>%
    pull(market_pred)

  expect_identical(assignment_check1, assignment_check2)

})

test_that('tot_iters thrown correctly', {
  expect_error(run_assignment2(tot_iters = 0))
  expect_error(run_assignment2(tot_iters = -1))
  expect_error(run_assignment2(tot_iters = 4.1))
})
