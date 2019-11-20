#' Calculate error from a single year run
#'
#' This'll mostly get called from within mse_multiple
#'
#' @param prepped_data_bp data frame formatted correctly for this model, with bp column added
#' @param starting_vals Unquoted name of starting value column (number in year n)
#' @param target_vals Unquoted name of target value column (number in year n+1)
#' @param n_to_add Number of EVs/whatever to add this year
#' @param p Innovation parameter, numeric 0-1
#' @param q Immitation parameter, numeric 0-1
#'
#'
#' @return Mean squared error for a single assignment run
#' @export
#' @importFrom dplyr mutate select summarize pull
#' @importFrom magrittr "%>%"
mse_single <- function(prepped_data_bp, starting_vals, target_vals, n_to_add, p, q) {

  estimated_future_vals <- prepped_data_bp %>%
    select(market_curr = {{ starting_vals }},
           market_target = {{ target_vals }},
           .data$market_limit, .data$base_rate) %>%
    mutate(M_curr = .data$market_curr / .data$market_limit) %>%
    run_assignment(n_to_add = n_to_add, tot_iters = 40, p = p, q = q)

  # and get the mse for that year
  estimated_future_vals %>%
    summarize(mse = mean((.data$market_target - .data$market_curr)^2)) %>%
    pull(.data$mse)
}

#' Calculate summed error over arbitrary number of years of additions
#'
#' @param par numeric vector used by optimx
#' @param prepped_data data frame formatted correctly for this model
#' @param targ_cols Vector of unquoted names of cols with EVs/Whatever present each year
#' @param n_to_add Vector of number of EVs/whatever to add each year year (length 1 - length of targ_cols)
#' @param fixed_predictor unquoted name of explanatory variable that will have constant effect of 1 (usually income)
#' @param other_predictors vector of unquoted variable names of explanatory variables for which coefficient will be estimated
#' @param id_col unquoted name of column containing observation unique IDs
#' @param params_order character vector, names corresponding to par
#' @param frame numeric; maximum divergence allowed for calculated BP. By default, the smallest BP will be 0.1\*mean, and the largest will be 10\*mean)
#'
#' @return Combined mean squared error for a multiple assignment runs over successive years
#' @export
#' @importFrom rlang set_names as_name
#' @importFrom purrr pmap_dbl
#'
#' @examples
mse_multiple <- function(par, prepped_data,
                         targ_cols, n_to_add,
                         fixed_predictor,
                         other_predictors,
                         id_col,
                         params_order,
                         frame = 10) {

    params <- set_names(par, params_order) %>% as.list()

    # first, calculate the base rate
    prepped_data_bp <- prepped_data %>%
      calculate_bp(fixed_predictor = {{ fixed_predictor }},
                   other_predictors = {{ other_predictors }},
                   coefficients = par,
                   id_col = {{ id_col }},
                   frame = frame)

    # then loop over the predictor columns in order with them successively taking the target and then starting columns
    list(starting_values = targ_cols[1:(length(targ_cols) - 1)],
         target_values   = targ_cols[2:length(targ_cols)],
         n_to_add        = n_to_add) %>%
      pmap_dbl(mse_single,
               prepped_data_bp = prepped_data_bp,
               p = params$p, q = params$q) %>%
      sum()
}


#' Estimate model coefficients for multiple years of additions
#'
#' @param prepped_data data frame formatted correctly for this model
#' @param id_col unquoted name of column containing observation unique IDs
#' @param targ_cols Vector of unquoted names of cols with EVs/Whatever present each year
#' @param n_to_add Vector of number of EVs/whatever to add each year year (length 1 - length of targ_cols)
#' @param fixed_predictor unquoted name of explanatory variable that will have constant effect of 1 (usually income)
#' @param other_predictors vector of unquoted variable names of explanatory variables for which coefficient will be estimated
#'
#' @return Named vector of coefficient estimates
#' @export
#' @importFrom purrr map_chr
#' @importFrom rlang as_name set_names
#'
#' @examples
estimate_model <- function(prepped_data, id_col,
                           targ_cols, n_to_add,
                           fixed_predictor, other_predictors) {

  # create parameter starting values
  # ... these don't matter so much, but it helps to have them in the right ballpark ...
  # intercept will start at 1 (equal weight to fixed predictor)
  # other predictors will start at 0.25
  # p and q will start at 0.1 and 0.5 respectively
  # TODO check if we want named list or named vector here
  param_names <- c('intercept' = 1, map_chr(other_predictors, as_name), 'p', 'q')
  params_start <- set_names(c(1, rep(0.25, times = length(other_predictors)), 0.1, 0.5),
                            param_names)

}
