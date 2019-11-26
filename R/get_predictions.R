#' Get predictions from iterative bass model
#'
#' Predicts values given a correctly formatted dataset, set of parameters, starting value, and number of items to distribute
#' FUTURE: if I can figure out how to get BP to work without having whole dataset as input, this will also input/output vectors
#'
#' @param prepped_data data frame formatted correctly for this model
#' @param id_col unquoted name of column containing observation unique IDs
#' @param start_col unquoted names of column used as starting value for predictions
#' @param n_to_add numeric, total number of things to assign across all zones
#' @param coeffs named vector of model parameters, must include every variable in \code{other_predictors}, as well as 'intercept', 'p', and 'q
#' @param fixed_predictor unquoted name of explanatory variable that will have constant effect of 1 (usually income)
#' @param other_predictors vector of unquoted variable names of explanatory variables for which coefficient will be estimated
#' @param prediction_col unquoted or quated name for column to assing predictions to, defaults to \code{pred}
#' @param frame numeric; maximum divergence allowed for calculated BP. By default, the smallest BP will be 0.1\*mean, and the largest will be 10\*mean)
#'
#' @return Data frame formatted as \code{prepped_data} but with an added column of predictions
#' @export
#' @importFrom magrittr "%>%"
#' @importFrom dplyr pull mutate
#' @importFrom rlang ":="
#'
#'
#' @examples
predict_iterbass <- function(prepped_data,
                             id_col, start_col,
                             n_to_add, coeffs,
                             fixed_predictor, other_predictors,
                             prediction_col = "pred",
                             frame = 10) {

  # first, calculate the base rate
  prepped_data_bp <- prepped_data %>%
    calculate_bp(fixed_predictor = {{ fixed_predictor }},
                 other_predictors = {{ other_predictors }},
                 coefficients = coeffs,
                 frame = frame)

  # use run_assign to get predictions from using this base rate
  preds <- run_assignment(market_current = pull(prepped_data_bp, {{ start_col }}),
                          market_limit   = pull(prepped_data_bp, .data$market_limit),
                          base_rate      = pull(prepped_data_bp, .data$base_rate),
                          n_to_add       = n_to_add,
                          p = coeffs['p'], q = coeffs['q'])

  prepped_data %>%
    mutate({{ prediction_col }} := preds)
}






