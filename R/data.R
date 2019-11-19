#' Rough simulated dataset for easy function testing only
#'
#' Dataset with basically the minimal possible info to test all the functions in this package.
#' Future efforts will probably include using a data generation script in raw_data.
#'
#' @format A tibble with 20 rows and 8 columns
#' \describe{
#'   \item{location}{location id field}
#'   \item{val_start}{number of units in this location in this location in year 1}
#'   \item{val_s1}{number of units in this location in year 2}
#'   \item{val_s2}{number of units in this location in year 3}
#'   \item{market_limit}{maximum number of units that can be assigned to each location}
#'   \item{fixed_pred}{fixed predictor variable (effect = 1 always)}
#'   \item{other_pred1}{dummy variable predictor}
#'   \item{other_pred2}{continuous variable predictor}
#'   \item{rare_pred}{predictor with only one value of 1, rest = 0}}
"minimal_data"
