#' Shaded Normal Distribution Curve
#'
#' A function that returns a graph of the normal distribution curve and the probability of a specified shaded area
#'
#' @param mu Mean of the distribution
#' @param sigma Standard deviation of the  distribution
#' @param a Upper limit for shaded area
#' @param b Lower limit for shaded area (default is negative infinity)
#' @param color Shaded area color
#'
#' @return A graph of the normal distribution curve and the probability of the shaded area
#'
#' @export
#'
#' @examples
#' # Example 1: Shading area under the curve from negative infinity to 1
#' myncurve(mu = 0, sigma = 1, a = 1)
#'
#' # Example 2: Shading area under the curve from 1 to 2
#' myncurve(mu = 0, sigma = 1, a = 2, b = 1)
#'
#' # Example 3: Shading area under the curve from -1 to 2 with a certain color
#' myncurve(mu = 0, sigma = 1, a = 2, b = -1, color = "green")
#'
#'@importFrom stats dnorm pnorm
#' @export
#'

myncurve = function(mu, sigma, a, b = -Inf, color="blue"){
  #plot the curve
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))

  #calculate the probability
  prob <- round(pnorm(a, mean=mu, sd=sigma)-pnorm(b, mean=mu, sd=sigma),4)

  #shade the curve
  if (is.infinite(b)) {
    b2 <- mu - 3 * sigma
  }
  else {
    b2 <- b
  }
  xcurve=seq(b2,a,length=1000)
  ycurve=dnorm(xcurve,mean=mu,sd=sigma)
  polygon(c(b2,xcurve,a),c(0,ycurve,0),col=color)

  list(mu = mu, sigma = sigma, prob = prob)
}
