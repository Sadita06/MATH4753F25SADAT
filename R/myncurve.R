#' Title: Plot Normal Distribution Curve with Shaded Area
#'
#' @param mu The mean of the normal distribution
#' @param sigma The standard deviation of the normal distribution
#' @param a The upper bound for the shaded area
#' @importFrom graphics curve polygon text
#' @importFrom stats dnorm pnorm
#'
#' @returns A list containing the input parameters and the calculated probability
#' @export
#'
#' @examples
#' result <- myncurve(mu = 10, sigma = 5, a = 6)
#' print(result)
myncurve = function(mu, sigma, a){
  # Plot the normal curve
  curve(dnorm(x, mean=mu, sd=sigma), xlim=c(mu-3*sigma, mu+3*sigma),
        main=paste("Normal: N(", mu, ",", sigma, ")"), ylab="Density")

  # Shade the area from -infinity to a
  xcurve = seq(mu-3*sigma, a, length=1000)
  ycurve = dnorm(xcurve, mean=mu, sd=sigma)
  polygon(c(mu-3*sigma, xcurve, a), c(0, ycurve, 0), col="LightBlue")

  # Calculate probability P(X <= a)
  prob = pnorm(a, mean=mu, sd=sigma)
  prob = round(prob, 4)

  # Add text to plot
  text(mu, dnorm(mu, mu, sigma)/2, paste("P(X <=", a, ") =", prob))

  # Return results as list
  return(list(mu=mu, sigma=sigma, probability=prob, a=a))
}
