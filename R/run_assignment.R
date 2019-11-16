#' Iterate over the model once, completing all assignment steps
#'
#' This functions runs all iterations for data that has been prepped WITH BPR COMPUTED
#'
#' @param starting_values Data frame formatted correctly for this model, probably from <link to setup function>
#' @param n_to_add Total number objects to assign, numeric
#' @param tot_iters Number of iterations to run, integer > 0
#' @param p Innovation parameter, numeric 0-1
#' @param q Immitation parameter, numeric 0-1
#' @param .keep_all if \code{TRUE} keep results of all iterations, otherwise only keep final assignment
#'
#' @return Data frame matching format of \code{starting_values} with all objects assigned.
#' @export
#' @importFrom purrr reduce
#' @importFrom purrr accumulate
#' @importFrom magrittr "%>%"
#' @importFrom dplyr bind_rows
#'
#' @examples
run_assignment <- function(starting_values,
                           n_to_add, tot_iters, p, q,
                           .keep_all = FALSE) {

  # first, calculate the number of EVs to assign in each iteration
  n_each_iter <- n_to_add / tot_iters

  if (.keep_all) {
    # if you want to keep all iteration results, will run accumulate (list reduce while keeping intermediates)
    assignment_result <-
      accumulate(
        1:tot_iters,
        single_assignment_step,
        .init = starting_values,
        n_this_iter = n_each_iter,
        p = p, q = q) %>%
      bind_rows()

  } else {
    # if not, use reduce, which works exactly the same but only keep the final result
    assignment_result <-
      reduce(
        1:tot_iters,
        single_assignment_step,
        .init = starting_values,
        n_this_iter = n_each_iter,
        p = p, q = q)

  }

  return(assignment_result)
}


#' Run the assignment with a given set of parameters
#'
#' [run_assignment()] assumes BPR is fixed / precalculated, so this first computes it for a given set of parameters then runs the assignment.
#' This function also keeps a frame on BPR, by default an order of magnitude in either direction
#'
#' @param prepped_data Data frame formatted correctly for this model, probably from <link to setup function>
#' @param params Named list or vector of parameters, as of right now must include all of BP_intercept, BP_coef_commute, BP_coef_gravity, BP_coef_carpool, innov_coef_p, and innov_coef_p
#' @param tot_evs Total number objects to assign, numeric
#' @param bpr_frame Maximum divergence allowed for calculated BPR. By default (at 10, the smallest BPR will be 0.1\*mean, and the largest will be 10\*mean)
#' @param tot_iters Number of iterations to run, integer > 0
#'
#' @return Data frame matching format of \code{prepped_data} with all objects assigned.
#' @export
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom dplyr case_when
#' @importFrom rlang .data
#'
#' @examples
run_assignment_with_variable_bp <- function(prepped_data,
                                            params, tot_evs,
                                            bpr_frame  = 10,
                                            tot_iters = 40) {

  # calculate base penetration rate
  starting_vals_init <- prepped_data %>%
    mutate(base_rate =
             1                    * params$BP_intercept +
             .data$med_income     * 1 +
             .data$commute        * params$BP_coef_commute +
             .data$EV_2014_nb     * params$BP_coef_gravity +
             .data$share_with_cpl * params$BP_coef_carpool)

  baseline_bpr <- max(mean(pull(starting_vals_init, .data$base_rate)), 0.1) * c(1 / bpr_frame, bpr_frame)

  # ensure that base_rate never drops negative
  starting_vals <- starting_vals_init %>%
    mutate(base_rate = case_when(.data$base_rate < baseline_bpr[1] ~ baseline_bpr[1],
                                 .data$base_rate > baseline_bpr[2] ~ baseline_bpr[2],
                                 TRUE                              ~ .data$base_rate))

  run_assignment(starting_vals,
                 tot_evs, tot_iters,
                 params$innov_coef_p, params$innov_coef_q,
                 .keep_all = FALSE)
}

