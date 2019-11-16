#' Calculate base penetration rate for arbitrary variables
#'
#' ToDo: fix the error handling a little, it's gross rn
#'
#' @param prepped_data data frame formatted correctly for this model, probably from <link to setup function>
#' @param fixed_predictor unquoted name of explanatory variable that will have constant effect of 1 (usually income)
#' @param other_predictors vector of
#' @param coefficients named list or vector of parameters
#' @param id_col unquoted name of column containing observation unique IDs
#' @param frame numeric; maximum divergence allowed for calculated BP. By default, the smallest BP will be 0.1\*mean, and the largest will be 10\*mean)
#'
#' @return
#' @export
#' @importFrom tibble tibble enframe
#' @importFrom dplyr select mutate bind_rows row_number group_by summarize left_join case_when
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr "%>%"
#' @importFrom rlang as_string quo
#' @importFrom stats median
#'
#' @examples
calculate_bp <- function(prepped_data, fixed_predictor, other_predictors,
                         coefficients, id_col, frame = 10) {

  # grab the name of the fixed variable ... need to clean this code up a bit
  var1 <- grab_single_symbol({{ fixed_predictor }})
  if (is.null(var1)) stop("fixed_predictor should be a single variable name either unquoted or as string")

  # create data frame of coefficients, add one for fixed predictor
  cd <- bind_rows(enframe(coefficients, name = 'var', value = 'coef'),
                  tibble(var = var1, coef = 1))

  # if there isn't already an id_col, create one using row number
  if (missing(id_col)) {
    prepped_data <- prepped_data %>% mutate(fake_id = row_number())
    id_col <- quo(fake_id)
  }

  # first, grab predictor columns and add a column of ones for the intercept
  pd <- prepped_data %>%
    select({{ id_col }}, {{ fixed_predictor }}, {{ other_predictors }}) %>%
    mutate(intercept = 1)

  # then, convert to long format, attach the coefficients and calculate the initial bp values
  bps_first <- pd %>%
    # convert to long form
    pivot_longer(cols = -{{ id_col }},
                 names_to = 'var',
                 values_to = 'value') %>%
    # attach coefficients
    left_join(cd, by = 'var') %>%
    # calculate bp rate for each unique id
    group_by({{ id_col }}) %>%
    summarize(base_rate = sum(.data$coef * .data$value))

  # correct the base rate to within the frame (all bps must be positive and within a factor of frame from the median)
  med_bp <- median(bps_first$base_rate)
  bp_lo  <- max(med_bp / frame, 0.001)
  bp_hi  <- med_bp * frame

  bps_corrected <- bps_first %>%
    mutate(base_rate2 = case_when(base_rate < bp_lo ~ bp_lo,
                                  base_rate > bp_hi ~ bp_hi,
                                  TRUE              ~ base_rate))

  # finally, reattach the base rate to the original data and return the result
  prepped_data %>%
    left_join(bps_corrected, by = names(bps_first)[1])
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