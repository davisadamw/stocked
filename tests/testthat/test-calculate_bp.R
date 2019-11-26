test_that("simple bp calculation works", {
  # first, calcualte bp on minimal data
  bp_results <- minimal_data %>%
    calculate_bp(fixed_predictor = fixed_pred,
                 other_predictors = c(other_pred1, other_pred2),
                 coefficients = c("intercept" = 10, other_pred1 = 2, other_pred2 = 3),
                 id_col = location)

  # compare the results from the bp calculation with all pred effs = 1 and intercept = 10
  # intercept large to ensure frame never comes into play
  expect_equal(sum(bp_results$base_rate),
               sum(10 + minimal_data$fixed_pred + 2*minimal_data$other_pred1 + 3*minimal_data$other_pred2))
})

test_that("bp floor is fixed", {
  # first check against general floor of 0.001
  bp_floor_1 <- minimal_data %>%
    calculate_bp(fixed_predictor = fixed_pred,
                 other_predictors = c(other_pred1, other_pred2),
                 coefficients = c("intercept" = -10, other_pred1 = 1, other_pred2 = 1),
                 id_col = location)
  expect_equal(bp_floor_1$base_rate, rep(0.001, 20))

  # ensure that the floor comes into play to fix extreme values
  bp_floor_2 <- minimal_data %>%
    calculate_bp(fixed_predictor = fixed_pred,
                 other_predictors = rare_pred,
                 coefficients = c("intercept" = 10, "rare_pred" = -1000))
  expect_gte(bp_floor_2$base_rate[1], 0.001)

  # a check for specific cases against the median * 0.1
  bp_floor_3 <- minimal_data %>%
    calculate_bp(fixed_predictor = fixed_pred,
                 other_predictors = rare_pred,
                 coefficients = c("intercept" = 10, "rare_pred" = -9))
  expect_equal(bp_floor_3$base_rate[1], median(bp_floor_3$base_rate) / 10)

  # finally, a general check against the floor
  bp_floor_4 <- minimal_data %>%
    calculate_bp(fixed_predictor = fixed_pred,
                 other_predictors = c(other_pred1, other_pred2),
                 coefficients = c("intercept" = -10, other_pred1 = 3, other_pred2 = -1),
                 id_col = location)
  expect_gte(min(bp_floor_4$base_rate), median(bp_floor_4$base_rate) / 10)
})

test_that("bp ceiling is fixed", {
  # ensure that the floor comes into play in general
  bp_ceil_2 <- minimal_data %>%
    calculate_bp(fixed_predictor = fixed_pred,
                 other_predictors = c(other_pred1, other_pred2),
                 coefficients = c("intercept" = 10, "other_pred1" = 5, "other_pred2" = -1))
  expect_lte(max(bp_ceil_2$base_rate), median(bp_ceil_2$base_rate) * 10)

  # finally a specific check for extreme values
  bp_ceil_3 <- minimal_data %>%
    calculate_bp(fixed_predictor = fixed_pred,
                 other_predictors = rare_pred,
                 coefficients = c("intercept" = 10, "rare_pred" = 1000))
  expect_equal(bp_ceil_3$base_rate[1], median(bp_ceil_3$base_rate) * 10)
})
