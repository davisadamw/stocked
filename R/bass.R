#' Compute Bass slope coefficient / A component
#'
#' Compute Bass slope coefficient given current market share and bass parameters p, q
#'
#' @param M Current market share, numeric 0-1
#' @param p Innovation parameter, numeric 0-1
#' @param q Immitation parameter, numeric 0-1
#'
#' @return Slope of bass curve with paramers p, q at market penetration M (numeric)
#' @export
#'
#' @examples
#' # compute the slope of the bass curve with parameters (0.05, 0.4) when half the market limit has been reached:
#' bass(0.5, 0.05, 0.4)
bass <- function(M, p, q) {
  (p + q * M) * (1 - M)
}

