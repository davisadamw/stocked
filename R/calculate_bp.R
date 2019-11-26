#' Calculate base penetration rate for arbitrary variables
#'
#' ToDo: fix the error handling a little, it's gross rn
#'
#' @param prepped_data data frame formatted correctly for this model, probably from \code{prep_data()}
#' @param fixed_predictor unquoted name of explanatory variable that will have constant effect of 1 (usually income)
#' @param other_predictors vector of unquoted variable names of explanatory variables for which coefficient will be estimated
#' @param coefficients named vector of parameters, including everything in other_predictors, as well as "intercept" "p" and "q"
#' @param id_col unquoted name of column containing observation unique IDs
#' @param frame numeric; maximum divergence allowed for calculated BP. By default, the smallest BP will be 0.1\*mean, and the largest will be 10\*mean)
#'
#' @return Data frame formatted same as input with base_rate column added
#' @export
#' @importFrom tibble tibble enframe
#' @importFrom dplyr select mutate bind_rows row_number group_by summarize left_join case_when
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr "%>%"
#' @importFrom rlang as_name quo
#' @importFrom stats median
#'
#' @examples
#' # calculates bp on example dataset with all coefficients set to 1
#' calculate_bp(minimal_data,
#'              fixed_predictor = fixed_pred,
#'              other_predictors = c(other_pred1, other_pred2),
#'              coefficients = c("intercept" = 1, other_pred1 = 1, other_pred2 = 1),
#'              id_col = location)
calculate_bp <- function(prepped_data, fixed_predictor, other_predictors,
                         coefficients, id_col, frame = 10) {

  # grab the name of the fixed variable ... need to clean this code up a bit
  var1 <- grab_single_symbol({{ fixed_predictor }})

  # create data frame of coefficients, add one for fixed predictor
  cd <- coefficients
  cd[var1] <- 1

  # first, grab predictor columns and add a column of ones for the intercept
  pd <- prepped_data %>%
    mutate(intercept = 1) %>%
    select(.data$intercept, {{ fixed_predictor }}, {{ other_predictors }}) %>%
    as.matrix()

  # then, use matrix multiplication to efficiently calculate initial bp values
  # doin
  bps_first <- pd %*% as.matrix(cd[colnames(pd)])

  # correct the base rate to within the frame (all bps must be positive and within a factor of frame from the median)
  med_bp <- median(bps_first)
  bp_lo  <- max(med_bp / frame, 0.001)
  bp_hi  <- max(med_bp * frame, 0.001)

  prepped_data %>%
    mutate(base_rate = case_when(bps_first < bp_lo ~ bp_lo,
                                 bps_first > bp_hi ~ bp_hi,
                                 TRUE              ~ bps_first))
}

#' Try-Catch to make sure function receives only a single variable name when needed
#'
#' I have no idea if this works right.
#'
#' @param arg an argument passed from a previous function
#'
#' @return Either variable name as text or error
#' @importFrom rlang as_string ensym
#'
grab_single_symbol <- function(arg) {
  tryCatch(
    {
      ideally <- as_string(ensym(arg))
      return(ideally)
    },
    error = function(error_message) {
      message("fixed_predictor should be a single variable name either unquoted or as string")
    }
  )
}
