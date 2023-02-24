#' One-Sided HP Filter
#'
#' hp1 applies a one-sided Hodrick–Prescott filter derived using the Kalman
#' filter to separate a time series into trend and cyclical components. The
#' smoothing parameter should reflect the cyclical duration or frequency of
#' the data.
#'
#' @param y a dataframe of size Txn, where “T” is the number of observations
#'   for each variable (number of rows) and “n” - the number of variables in
#'   the dataframe (number of columns).
#'
#' @param lambda the smoothing parameter; a numeric scalar which takes the
#'   default value of 1600, if unspecified by the user.
#'
#' @param x_user user defined initial values of the state estimate for each
#'   variable in y. Takes the form of a 2xn matrix. Since the underlying state
#'   vector is 2x1, two values are needed for each variable in y. By default:
#'   if no values are provided, backwards extrapolations based on the first two
#'   observations are used.
#'
#' @param P_user a structural array with n elements, each of which being a 2x2
#'   matrix of initial MSE estimates for each variable in y. By default: if no
#'   values are provided, a matrix with relatively large variances is used.
#'
#' @param discard the number of discard periods, expressed as a numeric scalar.
#'   The user specified amount of values will be discarded from the start of the
#'   sample, resulting in output matrices of size (T-discard)xn. By default: if
#'   no values are provided, is set to 0.
#'
#' @return a (T-discard)xn dataframe, containing the trend data
#'
#' @details The length of the time series should be greater than four and the
#'   value of the smoothing parameter greater than zero for the code to function.
#'   Of course, having a sufficiently long time series is paramount to achieving
#'   meaningful results.
#'
#' @usage hp1(y, lambda = 1600, x_user = NA, P_user = NA, discard = 0)
#'
#' @keywords hp1 one-sided hp filter kalman basel ccyb gap buffer
#'
#' @seealso [hp2()]
#'
#' @author Alexandru Monahov, <https://www.alexandrumonahov.eu.org/>
#'
#' @references Balcilar, M. (2019). Miscellaneous Time Series Filters ‘mFilter’. CRAN R Package Library.
#'
#' Drehmann, M., and Yetman, J. (2018). Why You Should Use the Hodrick-Prescott Filter – at Least to Generate Credit Gaps. BIS Working Paper No. 744.
#'
#' Eurostat (2023), Real Gross Domestic Product for European Union (28 countries) [CLVMNACSCAB1GQEU28], National Accounts - GDP.
#'
#' Hamilton, J. D. (2017). ‘Why You Should Never Use the Hodrick-Prescott Filter’. Working Paper Series. National Bureau of Economic Research, May 2017.
#'
#' Hodrick, R. J., and Prescott, E. C. (1997). Postwar U.S. Business Cycles: An Empirical Investigation. Journal of Money, Credit, and Banking 29: 1-16.
#'
#' Hyeongwoo, K. (2004). "Hodrick–Prescott Filter". Notes, Auburn University.
#'
#' Mcelroy, T. (2008). Exact formulas for the Hodrick-Prescott Filter. Econometrics Journal. 11. 209-217.
#'
#' Meyer-Gohde, A. (2010). Matlab code for one-sided HP-filters. QM&RBC Codes 181, Quantitative Macroeconomics & Real Business Cycles.
#'
#' Ravn, M., and Uhlig, H. (2002). On adjusting the Hodrick-Prescott filter for the frequency of observations, The Review of Economics and Statistics 2002; 84 (2): 371–376.
#'
#' Shea, J. (2021). neverhpfilter: An Alternative to the Hodrick-Prescott Filter. CRAN R Package Library.
#'
#' @examples
#' # Generate the data and plot it
#' set.seed(10)
#' y <- as.data.frame(rev(diffinv(rnorm(100)))[1:100])+30
#' colnames(y) <- "gdp"
#' plot(y$gdp, type="l")
#'
#' # Apply the HP filter to the data
#' ytrend = hp1(y)
#' ycycle = y - ytrend
#'
#' # Plot the three resulting series
#' plot(y$gdp, type="l", col="black", lty=1, ylim=c(-10,30))
#' lines(ytrend$gdp, col="#066462")
#' polygon(c(1, seq(ycycle$gdp), length(ycycle$gdp)), c(0, ycycle$gdp, 0), col = "#E0F2F1")
#' legend("bottom", horiz=TRUE, cex=0.75, c("y", "ytrend", "ycycle"), lty = 1,
#'        col = c("black", "#066462", "#75bfbd"))
#'
#' @export

hp1 <- function(y,lambda=1600,x_user=NA,P_user=NA,discard=0) {

  kalman_update = function(y,F,H,Q,R,j,k,x,P) {
    # Updates the Kalman filter estimation of the state and MSE
    # For details, see Chapter 13 of Hamilton, J.D. (1994). Time Series Analysis. Princeton University Press.
    S = H %*% P %*% t(H) + R
    K = F %*% P %*% t(H)
    K = K / as.double(S) # Kalman gain
    x = F %*% x + K * as.double(y[j,k] - H %*% x) # State estimate
    Temp = F - K %*% H
    P = Temp %*% P %*% t(Temp)
    P = P + Q + K %*% R %*% t(K) # MSE estimate
    return(list("x" = x, "P" = P)) # Store the results to be returned when calling the function as a list (we do this because R functions return one result)
  }

  ytrend <- matrix(data=NA,nrow=nrow(y),ncol=ncol(y)) # Create an empty matrix of the same size as "y", where the Kalman filtration results will be stored
  T = nrow(y)
  n = ncol(y)
  # Kalman preliminaries. The notation follows Chapter 13 of Hamilton, J.D.
  # (1994). Time Series Analysis. with the exception of H, which is equivalent
  # to his H'.
  q = 1 / lambda # the signal-to-noise ration: i.e. var eta_t / var epsilon_t
  F = rbind(c(2,-1),c(1,0)) # The state transition matrix
  H = rbind(c(1,0)) # The observation matrix
  Q = rbind(c(q,0),c(0,0)) # The variance-covariance matrix of the errors in the state equation
  R = 1 # The variance of the error in the observation equation

  for (k in 1:n) { # Run the Kalman filter for each variable
    if (is.na(x_user)) {x = rbind(unlist(c(2*y[1,k]-y[2,k])), unlist(c(3*y[1,k]-2*y[2,k])))} else {x = x_user[,k]} # If the user didn't provide an initial value for state estimate, extrapolate back two periods from the observations
    if (is.na(P_user)) {P = rbind(c(1e5, 0), c(0,1e5))} else {P = P_user[k]} # If the user didn't provide an intial value for the MSE, set a relatively high one
    for (j in 1:T) { # Get the estimates for each period
      klm = kalman_update(y,F,H,Q,R,j,k,x,P) # store the results returned by the function as a list called "klm"
      x = klm$x # get element "x" from the list called "klm" and store it as "x" so that it can be used outside the function kalman_update
      P = klm$P # similarly to x
      ytrend[j,k]=x[2] # The second element of the state is the estimate of the trend
    }
  }

  ycycle = y - ytrend # Deviations from the HP trend

  if (!missing(discard)) { # If the user provided a discard parameter
    if (discard!=0) {
      ytrend = ytrend[-1:-discard,] # Remove the first "discard" periods from the trend series
      ycycle = ycycle[-1:-discard,]
    }
  }

  ytrend <- as.data.frame(ytrend) # Store "ytrend" as a tibble
  colnm <- colnames(y) # Get column names from "y"
  colnames(ytrend) <- colnm # Name the columns of "ytrend" so that they have the same names as "y"
  return(ytrend)

}
