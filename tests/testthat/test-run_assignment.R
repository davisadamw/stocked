test_that("correct number gets assigned", {
  md_bp <- minimal_data %>%
    calculate_bp(fixed_predictor = fixed_pred,
                 other_predictors = c(other_pred1, other_pred2),
                 coefficients = c("intercept" = 10, other_pred1 = 1, other_pred2 = 1),
                 id_col = location) %>%
    mutate(market_curr = val_start,
           M_curr      = val_start / market_limit)

  assignment_check <- md_bp %>%
    run_assignment(n_to_add = 100,
                   tot_iters = 40,
                   p = 0.1, q = 0.4)

  expect_equal(sum(assignment_check$market_curr), sum(md_bp$market_curr) + 100)

})
