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
#' @return
#' @export
#' @importFrom dplyr mutate select summarize pull
#' @importFrom magrittr "%>%"
mse_single <- function(prepped_data_bp, starting_vals, target_vals, n_to_add, p, q) {

  estimated_future_vals <- prepped_data_bp %>%
    select(market_curr = {{ starting_vals }},
           market_target = {{ target_vals }},
           market_limit, base_rate) %>%
    mutate(M_curr = .data$market_curr / .data$market_limit) %>%
    run_assignment(n_to_add = n_to_add, tot_iters = 40, p = p, q = q)

  # and get the mse for that year
  estimated_future_vals %>%
    summarize(mse = mean((market_target - market_curr)^2)) %>%
    pull(.data$mse)
}

#' Calculate summed error over arbitrary number of years of additions
#'
#' @param par numeric vector used by optimx
#' @param prepped_data
#' @param targ_cols
#' @param n_to_add
#' @param fixed_predictor
#' @param other_predictors
#' @param id_col
#' @param params_order character vector, names corresponding to par
#' @param frame
#'
#' @return
#' @export
#' @importFrom rlang set_names
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
                   coefficients = params,
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


estimate_model <- function(prepped_data, id_col,
                           targ_vals, ns_to_add,
                           fixed_predictor, other_predictors,
                           starting_vals = NULL) {

  # create parameter starting values
}
