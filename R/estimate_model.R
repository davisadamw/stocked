#' Calculate summed error over arbitrary number of years of additions
#'
#' This function calculates the overall mean squared error from a set of projections over various years given a set of coefficients.
#' Should work as long as at least two columns are included in targ_cols
#'
#' @param par numeric vector used by optimx
#' @param prepped_data data frame formatted correctly for this model
#' @param targ_cols Vector of unquoted names of cols with EVs/Whatever present each year
#' @param fixed_predictor unquoted name of explanatory variable that will have constant effect of 1 (usually income)
#' @param other_predictors vector of unquoted variable names of explanatory variables for which coefficient will be estimated
#' @param id_col unquoted name of column containing observation unique IDs
#' @param params_order character vector, names corresponding to par (necessary because optimx strips names)
#' @param return_preds logical, should this method return predictions only (stop before calculating mse)
#' @param frame numeric; maximum divergence allowed for calculated BP. By default, the smallest BP will be 0.1\*mean, and the largest will be 10\*mean)
#'
#' @return Combined mean squared error for a multiple assignment runs over successive years
#' @export
#' @importFrom rlang set_names as_name
#' @importFrom purrr map2 map2_dbl pluck reduce
#' @importFrom tibble as_tibble
#' @importFrom dplyr pull select
#' @importFrom tidyselect everything
#' @importFrom magrittr "%>%"
#'
#' @examples
#' p_start <- c('intercept'=1, 'other_pred1'=1, 'other_pred2'=1, 'p'=0.1, 'q'=0.4)
#' mse_model(par = p_start,
#'           prepped_data = minimal_data,
#'           targ_cols = c(val_start:val_s2),
#'           fixed_predictor = fixed_pred,
#'           other_predictors = c(other_pred1, other_pred2),
#'           id_col = location,
#'           params_order = names(p_start))
mse_model <- function(par, prepped_data,
                      targ_cols,
                      fixed_predictor,
                      other_predictors,
                      id_col,
                      params_order,
                      return_preds = FALSE,
                      frame = 10) {

  params <- set_names(par, params_order)

  # first, calculate the base rate
  prepped_data_bp <- prepped_data %>%
    calculate_bp(fixed_predictor = {{ fixed_predictor }},
                 other_predictors = {{ other_predictors }},
                 coefficients = params,
                 frame = frame)

  # then grab the target value columns and calculate the number of evs to add
  targets <- select(prepped_data, {{ targ_cols }})
  n_to_add <- calculate_target(targets, everything())

  # loop over the targ columns using them as starting values, excluding last
  # (loops over columns)
  # produces list of prediction vectors in chron order
  preds <- map2(select(targets, -ncol(targets)),
                n_to_add,
                run_assignment,
                market_limit = pull(prepped_data, .data$market_limit),
                base_rate = pull(prepped_data_bp, .data$base_rate),
                p = pluck(params, 'p'), q = pluck(params, 'q')) %>%
    set_names(paste('pred', names(targets)[-1], sep = '_'))

  if (return_preds) return(preds)

  # calculate error by comparing predictions with targ columns, excluding first
  # then calculate average by year and sum errors over the years (might wanna get mean? won't matter tho)
  map2_dbl(select(targets, -1),
           preds,
           ~ mean((.x - .y)^2)) %>%
    sum()
}


#' Estimate model coefficients for multiple years of additions
#'
#' @param prepped_data data frame formatted correctly for this model (mostly just needs a market_limit column)
#' @param id_col unquoted name of column containing observation unique IDs
#' @param targ_cols Vector of unquoted names of cols with EVs/Whatever present each year
#' @param fixed_predictor unquoted name of explanatory variable that will have constant effect of 1 (usually income)
#' @param other_predictors vector of unquoted variable names of explanatory variables for which coefficient will be estimated
#' @param starting_values named vector of starting values that will override default parameter starts, this can include any variable in \code{other_predictors}, 'intercept', 'p', and 'q
#'
#' @return a list containing the following elements:
#' * \code{coeffs} a named vector of estimated coefficients
#' * \code{MSE} numeric combined MSE from all years (FUTURE: will have this split out by year)
#' * \code{iterations} number of iterations before convergence
#' * \code{preds} named list of prediction vectors
#'
#' @export
#' @importFrom dplyr select
#' @importFrom rlang set_names
#'
#' @examples
estimate_iterbass <- function(prepped_data, id_col, targ_cols,
                              fixed_predictor, other_predictors,
                              starting_values = NULL) {
  # create parameter list and starting values
  # ... specific values don't matter so much, but it helps to have them in the right ballpark ...
  # intercept will start at 1 (equal weight to fixed predictor)
  # other predictors will start at 0.25 ... the awkwardness here is because vectors of names only works in tidyselect
  # p and q will start at 0.1 and 0.5 respectively
  param_names <- c('intercept', names(select(prepped_data, {{ other_predictors }})), 'p', 'q')
  n_other_preds <- length(param_names) - 3

  params_start <- set_names(c(1, rep(0.25, times = n_other_preds), 0.1, 0.5),
                            param_names)

  # update with starting values if any are provided
  if (!is.null(starting_values)) {
    params_start <- replace(params_start, names(starting_values), starting_values)
  }

  #return(list(params_start, params_start2))

  # feed this into optimx with the function mse_model in order to find the coefficients that minimize mse
  # run optimization on the model for multi-year mse, start with parameter starting values
  optimx_result <- optimx::optimx(params_start,
                                  mse_model,
                                  lower = c(rep(-Inf, n_other_preds + 1), 0, 0),
                                  upper = c(rep( Inf, n_other_preds + 1), 1, 1),
                                  method = 'L-BFGS-B',
                                  prepped_data = prepped_data,
                                  targ_cols = {{ targ_cols }},
                                  fixed_predictor = {{ fixed_predictor }},
                                  other_predictors = {{ other_predictors }},
                                  id_col = {{ id_col }},
                                  params_order = param_names,
                                  frame = 10)

  # extract the coefficients from the optimization result
  coeffs_estimated <- df_row_to_vec(optimx_result, one_of(param_names))

  # get the final predictions
  preds <- mse_model(par = coeffs_estimated,
                     prepped_data = prepped_data,
                     targ_cols = {{ targ_cols }},
                     fixed_predictor = {{ fixed_predictor }},
                     other_predictors = {{ other_predictors }},
                     id_col = {{ id_col }},
                     params_order = names(coeffs_estimated),
                     return_preds = TRUE,
                     frame = 10)

  list(coeffs = coeffs_estimated,
       MSE = optimx_result$value,
       iterations = optimx_result$fevals,
       preds = preds)
}
