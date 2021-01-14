#' Center and scale a single vector
#'
#' Create a vector of values with mean 0 and sd 1, which can be useful for model estimation
#'
#' @param x numeric, a vector containing the values to be centered and scaled
#' @param na.rm a logical value indicating whether NA values should be stripped before the computation proceeds.
#'
#' @return A vector of the same length as \code{x}, but with mean=0 and sd=1
#' @export
#' @importFrom stats sd
#'
#' @examples
#' center_and_scale(1:3) # = c(-1, 0, 1)
#' center_and_scale(1:4) # = c(-1.1618950, -0.3872983,  0.3872983,  1.1618950)
center_and_scale <- function(x, na.rm = TRUE) {
  (x - mean(x, na.rm = na.rm)) / sd(x, na.rm = na.rm)
}

#' Center and scale a selection of variables in a dataset place
#'
#' Apply \code{\link{center_and_scale}} over the variables named in \code{...}
#'
#' @param data data frame from which variables in \code{...} are selected
#' @param ... variables in data to center and scale
#' @param na.rm a logical value indicating whether NA values should be stripped before the computation proceeds.
#'
#' @return data frame with same columns as \code{data} but with the variables in \code{...} centered and scaled
#' @export
#' @importFrom dplyr mutate_at vars
#'
#' @examples
#' # center and scale the fixed predictor column in the minimal_data example dataset
#' minimal_data_predsCS <- center_and_scale_vars(minimal_data, fixed_pred)
#' mean(minimal_data_predsCS$fixed_pred) # = 0
#' sd(minimal_data_predsCS$fixed_pred)   # = 1
center_and_scale_vars <- function(data, ..., na.rm = TRUE) {
   mutate_at(data, vars(...), center_and_scale, na.rm = na.rm)
}


#' Run center-and-scale operation on one dataset using parameters from another
#'
#' again, dots are variables to center and scale, but they're centered and scaled according to training data chars
#'
#' @param validation data frame of data that will be used
#' @param training data frame from which means and sds will be drawn, must have at least all cols in \code{...}
#' @param ... variables present in both \code{validation} and \code{training}
#' @param na.rm a logical value indicating whether NA values should be stripped before the computation proceeds.
#'
#' @return data frame with same rows and columns as \code{validation} but with the variables in \code{...} centered and scaled according to distribution of same variables in \code{training}
#' @export
#' @importFrom dplyr select group_by summarize mutate left_join transmute bind_cols row_number
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom magrittr "%>%"
#' @importFrom stats sd
#' @importFrom tidyselect everything one_of
#'
#' @examples
center_and_scale_validation <- function(validation, training, ..., na.rm = TRUE) {
  # first, grab the distribution from the training data
  training_mean_sd <- training %>%
    select(...) %>%
    pivot_longer(everything(), names_to = 'variable', values_to = 'value') %>%
    group_by(.data$variable) %>%
    summarize(mean_training = mean(.data$value, na.rm = na.rm),
              sd_training   = sd(.data$value, na.rm = na.rm))

  # then grab the relevant vars from validation data
  validation_rescaled <- validation %>%
    select(...) %>%
    mutate(temp = row_number()) %>%
    pivot_longer(-.data$temp, names_to = 'variable', values_to = 'value') %>%
    left_join(training_mean_sd, by = 'variable') %>%
    transmute(.data$temp,
              .data$variable,
              value = (.data$value - .data$mean_training) / .data$sd_training) %>%
    pivot_wider(id_cols = .data$temp, names_from = .data$variable, values_from = .data$value) %>%
    select(-.data$temp)

  # smush the rescaled values back on
  validation %>%
    # replace predictor with rescaled versions
    select(-one_of(names(validation_rescaled))) %>%
    bind_cols(validation_rescaled) %>%
    # return columns to original order
    select(one_of(names(validation)))

}



#' Prepare data for stocked bass modeling
#'
#' This will center and scale predictor variables, add market limit, and remove everything else
#'
#' @param raw_data data frame
#' @param id_cols unquoted name of column containing observation unique IDs as well as any other variables to preserve
#' @param targ_cols vector of unquoted names of cols with EVs/Whatever present each year
#' @param predictor_cols vector of unquoted variable names of explanatory variables for which coefficient will be estimated
#' @param ml_col unquoted name of column containing the market limit for each unit
#' @param bump_ml logical, should market limit be set to highest targ value if that is higher than market limit? Will throw error if any target value is higher than market limit otherwise ... Currently just always throws an error if set to FALSE
#' @param training_data training data to use, if \code{NULL} data will be prepped as training data
#'
#' @return data frame prepped for use with the rest of the functions in this package
#' @export
#' @importFrom purrr reduce
#' @importFrom dplyr mutate select pull
#' @importFrom magrittr "%>%"
#'
#' @examples
prep_data <- function(raw_data, id_cols, targ_cols, predictor_cols, ml_col,
                      bump_ml = FALSE,
                      training_data = NULL) {

  # first calculate the highest value reached in targ cols
  targ_max <- raw_data %>%
    select({{ targ_cols }}) %>%
    reduce(pmax)

  if (isFALSE(bump_ml) & any(targ_max > pull(raw_data, {{ ml_col }}))) {
    stop('At least one target / market level is higher than the stated market limit.
         If you would like to bump market limit up to cover this error, set bump_ml = TRUE')
  }

  # identify current year data and calculate ML ... also ensure that the market limit >= max market level per unit
  ml_vars <- raw_data %>%
    mutate(market_limit = pmax({{ ml_col }}, targ_max))

  # center and scale predictors
  if (is.null(training_data)) {
    prepped_data <- ml_vars %>%
      center_and_scale_vars({{ predictor_cols }})
  } else {
    prepped_data <- ml_vars %>%
      center_and_scale_validation(training = training_data,
                                  {{ predictor_cols }})
  }

  prepped_data %>%
    select({{ id_cols }}, {{ targ_cols }}, {{ predictor_cols }}, .data$market_limit)

}


#' Calculate target number to add from target value columns
#'
#' Documentation needs to be better here
#'
#' @param prepped_data data frame prepped for use elsewhere in this model
#' @param ... variables with
#'
#' @return list of number added in each time step
#' @export
#' @importFrom magrittr "%>%"
#' @importFrom purrr set_names map2
#' @importFrom dplyr summarize_at
#' @importFrom stringr str_extract
#'
#' @examples
calculate_target <- function(prepped_data, ...) {
  tot_evs <- prepped_data %>%
    summarize_at(vars(...), sum) %>%
    as.list()

  map2(tot_evs[-1], tot_evs[1:(length(tot_evs) - 1)], `-`)
}
