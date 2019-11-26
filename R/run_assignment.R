#' Iterate over the model once, completing all assignment steps
#'
#' This functions runs all iterations for a single year step ... does it faster with vectors
#'
#' @param market_current numeric, vector of current market values
#' @param market_limit numeric, vector of market limits
#' @param base_rate numeric, vector of base penetration rates
#' @param n_to_add numeric, total number of things to assign across all zones
#' @param p Innovation parameter, numeric 0-1
#' @param q Immitation parameter, numeric 0-1
#' @param tot_iters integer > 0, number of iterations to run
#'
#' @return numeric vector of predictions
#' @export
#' @importFrom purrr reduce
#'
#' @examples
#' md_bp <- minimal_data %>%
#'   calculate_bp(fixed_predictor = fixed_pred,
#'                other_predictors = c(other_pred1, other_pred2),
#'                coefficients = c("intercept" = 10, other_pred1 = 1, other_pred2 = 1))
#'
#' assignment_check2 <- run_assignment(market_current = md_bp$val_start,
#'                                     market_limit = md_bp$market_limit,
#'                                     base_rate = md_bp$base_rate,
#'                                     n_to_add = 100,
#'                                     tot_iters = 40,
#'                                     p = 0.1, q = 0.4)
run_assignment <- function(market_current,
                             market_limit,
                             base_rate,
                             n_to_add, p, q,
                             tot_iters = 40) {

  # calculate the number of items to assign in each iteration
  if (!is.numeric(tot_iters) | as.integer(tot_iters) != tot_iters | tot_iters < 1) {
    stop('tot_iters must be a positive integer or a number interpretable as one')
  }
  n_each_iter <- n_to_add / tot_iters

  # run the assignment and return the final assignment vector
  reduce(1:tot_iters,
         single_assignment_step,
         .init = market_current,
         base_rate = base_rate, market_limit = market_limit,
         n_this_iter = n_each_iter,
         p = p, q = q)
}


#' Iterate over the model once, completing all assignment steps
#'
#' This functions runs all iterations for data that has been prepped WITH BPR COMPUTED
#'
#' @param starting_values Data frame formatted correctly for this model, probably from \code{prep_data()}
#' @param n_to_add Total number objects to assign, numeric
#' @param tot_iters Number of iterations to run, integer > 0
#' @param p Innovation parameter, numeric 0-1
#' @param q Immitation parameter, numeric 0-1
#'
#' @return data frame formatted like starting values with added columns of predictions
#' @importFrom purrr reduce
#' @importFrom purrr accumulate
#' @importFrom magrittr "%>%"
#' @importFrom dplyr bind_rows
run_assignment_old <- function(starting_values,
                               n_to_add, tot_iters, p, q) {

  # first, calculate the number of EVs to assign in each iteration
  n_each_iter <- n_to_add / tot_iters

  reduce(1:tot_iters,
         single_assignment_step_tib,
         .init = starting_values,
         n_this_iter = n_each_iter,
         p = p, q = q)
}

#' Iterate over the model once, completing all assignment steps
#'
#' This functions runs all iterations for a single year step ... does it slowly with tibbles
#'
#' @param market_current numeric, vector of current market values
#' @param market_limit numeric, vector of market limits
#' @param base_rate numeric, vector of base penetration rates
#' @param n_to_add numeric, total number of things to assign across all zones
#' @param p Innovation parameter, numeric 0-1
#' @param q Immitation parameter, numeric 0-1
#' @param tot_iters integer > 0, number of iterations to run
#'
#' @return numeric vector of predictions
#' @importFrom tibble tibble
#' @importFrom purrr reduce
#' @importFrom dplyr pull
#'
#' @examples
run_assignment_tib <- function(market_current,
                           market_limit,
                           base_rate,
                           n_to_add, p, q,
                           tot_iters = 40) {

  # calculate the number of items to assign in each iteration
  if (!is.numeric(tot_iters) | as.integer(tot_iters) != tot_iters | tot_iters < 1) {
    stop('tot_iters must be a positive integer or a number interpretable as one')
  }
  n_each_iter <- n_to_add / tot_iters

  # first, create the input data as a tibble
  starting_values <- tibble(market_curr  = market_current,
                            M_curr       = market_current / market_limit,
                            market_limit = market_limit,
                            base_rate    = base_rate)

  # run the assignment and return the final assignment vector
  reduce(1:tot_iters,
         single_assignment_step_tib,
         .init = starting_values,
         n_this_iter = n_each_iter,
         p = p, q = q) %>%
    pull(.data$market_curr)
}



