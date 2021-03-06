#' Run a single assignment step for the model
#'
#' Run a single round of the model, assigning units equal to total_to_assign / n_steps distributed among the regions included in the model
#' This should only be called by [run_assignment()]
#'
#' @param market_curr numeric, vector of current market values
#' @param iteration teration step, integer > 0 ... ignored silently, used only in reduce
#' @param base_rate numeric, vector of base penetration rates
#' @param market_limit numeric, vector of market limits
#' @param n_this_iter Number to assign this step, probably total / n_steps
#' @param p Innovation parameter, numeric 0-1
#' @param q Immitation parameter, numeric 0-1
#'
#' @return Numeric vector of ending market_current values
single_assignment_step <- function(market_curr, iteration,
                                     base_rate, market_limit, n_this_iter, p, q) {

  W <- base_rate * market_limit * bass(market_curr/market_limit, p, q)

  pmin(market_curr + n_this_iter * W / sum(W),
       market_limit)
}



#' Run a single assignment step for the model
#'
#' Run a single round of the model, assigning units equal to total_to_assign / n_steps distributed among the regions included in the model
#' This should only be called by [run_assignment_tib()] ... runs much slower and uses tibbles
#'
#' @param data Data frame output from previous step
#' @param n_this_iter Number to assign this step, probably total / n_steps
#' @param iteration Iteration step, integer > 0 ... pretty much ignored silently
#' @param p Innovation parameter, numeric 0-1
#' @param q Immitation parameter, numeric 0-1
#'
#' @return Data frame matching format of \code{data}.
#'
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#'
single_assignment_step_tib <- function(data, n_this_iter, iteration, p, q) {

  # first, drop columns that get updated each round, ignoring warning about missings
  # suppressWarnings({
  #   data_trimmed <- data %>%
  #     select(-one_of('market_prev', 'M_prev', 'W', 'W_share', 'assign'))
  # })

  # rename _current vars to _prev
  # rename the values of market and M from previous round
  mutate(data,
         #iter_no = iteration,
         #A       = bass(.data$M_curr, p, q),
         W       = .data$base_rate * .data$market_limit * bass(.data$M_curr, p, q),
         #W_share = .data$W / sum(.data$W),
         # assignment is weight as share of total weight or remaining capacity, whichever is smaller
         # assign      = pmin(.data$W / sum(.data$W) * n_this_iter,
         #                    .data$market_limit - .data$market_curr),
         market_curr = .data$market_curr +
           pmin(.data$W / sum(.data$W) * n_this_iter,
                .data$market_limit - .data$market_curr),
         M_curr      = .data$market_curr / .data$market_limit)

}

