#' Extract info from one row of a data frame as a named vector
#'
#'  This is mainly being used for
#'
#' @param data A data.frame or similar containing the row and columns you want to extract
#' @param ... One or more unquoted expressions separated by commas that can be passed
#' @param .row_no integer, which row to grab from data
#'
#' @return Named vector
#' @importFrom dplyr slice select
#' @importFrom magrittr "%>%"
df_row_to_vec <- function(data, ..., .row_no = 1) {
  if (!is.numeric(.row_no) | as.integer(.row_no) != .row_no | .row_no < 0 | .row_no > nrow(data)) {
    stop('.row_no must be an integer (or a number interpretable as one) corresponding to a row in data')
  }

  data %>%
    slice(.row_no) %>%
    select(...) %>%
    unlist()
}
