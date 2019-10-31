
# calculate A, run assignment
#' Title
#'
#' @param data
#' @param evs_this_iter
#' @param iteration
#' @param p
#' @param q
#'
#' @return
#' @export
#' @importFrom magrittr "%>%"
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#'
#' @examples
assign_PEVs_step <- function(data, evs_this_iter, iteration, p, q) {

  # first, drop columns that get updated each round, ignoring warning about missings
  suppressWarnings({
    data_trimmed <- data %>%
      select(-one_of('market_prev', 'M_prev', 'W', 'W_share', 'assign'))
  })

  # rename _current vars to _prev
  data_trimmed %>%
    # rename the values of market and M from previous round
    rename(market_prev = market_curr,
           M_prev      = M_curr) %>%
    mutate(iter_no = iteration,
           A       = bass(M_prev, p, q),
           W       = base_rate * market_limit * A,
           W_share = W / sum(W),
           # assignment is weight as share of total weight or remaining capacity, whichever is smaller
           assign      = pmin(W_share * evs_this_iter,
                              market_limit - market_prev),
           market_curr = market_prev + assign,
           M_curr      = market_curr / market_limit)

}
