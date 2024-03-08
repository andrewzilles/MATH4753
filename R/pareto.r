#' Pareto chart
#'
#' @param x numeric vector
#' @param mn Name for Pareto Chart
#'
#' @return numeric vector with a Pareto chart
#' @export
#'
#' @examples
#' data <- c(10, 15, 7, 25, 8)
#' pareto(data, mn = "Pareto Chart Example")
#'
pareto <- function(x, mn = "Pareto barplot", ...) {  # x is a vector
  x.tab = table(x)
  xx.tab = sort(x.tab, decreasing = TRUE, index.return = FALSE)
  cumsum(as.vector(xx.tab)) -> cs
  length(x.tab) -> lenx
  bp <- barplot(xx.tab, ylim = c(0,max(cs)),las = 2)
  lb <- seq(0,cs[lenx], l = 11)
  axis(side = 4, at = lb, labels = paste(seq(0, 100, length = 11), "%", sep = ""), las = 1, line = -1, col = "Blue", col.axis = "Red")
  for(i in 1:(lenx-1)){
    segments(bp[i], cs[i], bp[i+1], cs[i+1], col = i, lwd = 2)
  }
  title(main = mn, ...)
}

<<<<<<< HEAD
=======

>>>>>>> 215ffd90555de75c9f3e82c25bc8dcb60a5e9e7e
