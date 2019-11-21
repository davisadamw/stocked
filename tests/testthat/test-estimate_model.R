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
