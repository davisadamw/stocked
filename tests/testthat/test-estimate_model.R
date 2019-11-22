test_that("the function works-ish and mse increases when you add more years", {
  p_start <- c('intercept' = 1,
               'other_pred1' = 1, 'other_pred2' = 1,
               'p' = 0.1, 'q' = 0.4)

  model1yrA <- mse_model(par = p_start,
                         prepped_data = minimal_data,
                         targ_cols = c(val_start, val_s1),
                         fixed_predictor = fixed_pred,
                         other_predictors = c(other_pred1, other_pred2),
                         id_col = location,
                         params_order = names(p_start))

  model1yrB <- mse_model(par = p_start,
                         prepped_data = minimal_data,
                         targ_cols = c(val_s1, val_s2),
                         fixed_predictor = fixed_pred,
                         other_predictors = c(other_pred1, other_pred2),
                         id_col = location,
                         params_order = names(p_start))

  model2yrs <- mse_model(par = p_start,
                        prepped_data = minimal_data,
                        targ_cols = c(val_start:val_s2),
                        fixed_predictor = fixed_pred,
                        other_predictors = c(other_pred1, other_pred2),
                        id_col = location,
                        params_order = names(p_start))

  expect_gt(model2yrs, model1yrA)
  expect_gt(model2yrs, model1yrB)

})

test_that("estimate_iterbass can get the right values from predict_iterbass", {
  # set the predictor values
  p_start <- c('intercept'=1, 'other_pred1'=0.25, 'other_pred2'=0.25, 'p'=0.1, 'q'=0.5)

  # prep data
  md_prepped <- minimal_data %>%
    prep_data(location, c(val_start:val_s2), c(fixed_pred:rare_pred), market_limit)

  # forcast the addition of 40 items
  md_one_forward <- md_prepped %>%
    predict_iterbass(id_col = location,
                     start_col = val_s2,
                     n_to_add = 40,
                     coeffs = p_start,
                     fixed_predictor = fixed_pred,
                     other_predictors = c(other_pred1, other_pred2),
                     prediction_col = val_s3)

  # and use estimate to see what parameters produced that
  estimate_one_forward <- md_one_forward %>%
    estimate_iterbass(id_col = location,
                      targ_cols = c(val_s2, val_s3),
                      fixed_predictor = fixed_pred,
                      other_predictors = c(other_pred1, other_pred2))

  # check to see if resulting predictions approximately equal
  expect_equal(estimate_one_forward$coeffs, p_start)
})
