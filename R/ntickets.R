#' Calculate Number of Tickets for Airline Overbooking
#'
#' This function calculates the optimal number of tickets to sell on an airplane using discrete and continuous approaches.
#'
#' @param N Number of seats available on the flight.
#' @param gamma Probability of overbooking.
#' @param p Probability that a passenger shows up.
#' @importFrom graphics abline axis grid
#' @importFrom stats pbinom
#'
#' @return A named list containing:
#' \describe{
#'   \item{nd}{Optimal number of tickets using discrete distribution}
#'   \item{nc}{Optimal number of tickets using normal approximation}
#'   \item{N}{Number of seats}
#'   \item{p}{Probability of showing up}
#'   \item{gamma}{Overbooking probability}
#' }
#'
#' @details The function finds n such that pbinom(N, n, p) = 1 - gamma for the discrete case,
#' and pnorm(N, n*p, sqrt(n*p*(1-p))) = 1 - gamma for the continuous case.
#' It also creates a plot showing the objective functions for both approaches.
#'
#' @export
ntickets = function(N, gamma, p) {

  # Discrete distribution
  n_max <- N * 1.1
  n_values <- N:n_max

  discrete <- function(n) {
    return(pbinom(N, n, p) - (1 - gamma))
  }
  discrete_vals <- sapply(n_values, discrete)

  nd_idx <- which.min(abs(discrete_vals))
  nd <- n_values[nd_idx]


  # Normal approximation
  continuous <- function(n) {
    mu_n <- n * p
    sigma_n <- sqrt(n * p * (1 - p))
    if(sigma_n > 0) {
      return(pnorm(N, mean = mu_n, sd = sigma_n) - (1 - gamma))
    } else {
      return(0)
    }
  }

  cont_vals <- sapply(n_values, continuous)

  nc_idx <- which.min(abs(cont_vals))
  nc <- n_values[nc_idx]


  # Create plots
  par(mfrow = c(2, 1))

  # Discrete distribution plot
    plot(n_values, abs(discrete_vals),
       type = "b",
       pch = 21,
       bg = "black",
       cex = 0.5,
       lwd = 2,
       col = "black",
       xlab = "n",
       ylab = "Objective",
       main = paste("Objective Vs n to find optimal tickets sold (", nd, ") gamma= ", gamma, " N=", N, " discrete", sep = ""))

  abline(v = nd, col = "red", lty = 1, lwd = 2)

  abline(h = 0, col = "red", lty = 1, lwd = 2)

  axis(side = 1,
       at = nd,
       labels = nd,
       col.ticks = "red",
       lwd.ticks = 2)

  grid()

  # Normal approximation plot
  n_seq <- seq(N, n_max, length.out = 200)
  cont_vals_seq <- sapply(n_seq, continuous)

  plot(n_seq, abs(cont_vals_seq),
       type = "l",
       lwd = 2,
       col = "black",
       xlab = "n",
       ylab = "Objective",
       main = paste("Objective Vs n to find optimal tickets sold (", nc, ") gamma= ", gamma, " N=", N, " continuous", sep = ""))

  nc_idx_seq <- which.min(abs(cont_vals_seq))

  abline(v = nc, col = "blue", lty = 1, lwd = 2)

  abline(h = 0, col = "blue", lty = 1, lwd = 2)

  axis(side = 1,
       at = nc,
       labels = round(nc, 0),
       col.ticks = "blue",
       lwd.ticks = 2)

  grid()

  par(mfrow = c(1, 1))

  # Print the named list
  result <- list(nd = nd, nc = nc, N = N, p = p, gamma = gamma)

  return(result)
}
