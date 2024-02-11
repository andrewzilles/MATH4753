#' Quadratic function
#'
#' @param x numeric vector
#' @param lm_model linear regression model object
#'
#'
#'
#' @return A quadratic equation
#' @export
#'
#'
#' @examples
#' \dontrun{myquad(x = 1:10, df.lm)}
#'
#'
myquad <- function(x,lm_model){
  coef <- coef(lm_model) #Extract coefficients from the linear model
coef[1] + coef[2] * x + coef[3] * x^2
}
