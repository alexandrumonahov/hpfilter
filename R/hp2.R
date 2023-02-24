#' Two-Sided HP Filter
#'
#' hp2 applies a standard two-sided Hodrick–Prescott filter using sparse
#' matrices to help reduce the compute time for large datasets. The smoothing
#' parameter should reflect the cyclical duration or frequency of the data.
#'
#' @param y a dataframe of size Txn, where “T” is the number of observations
#'   for each variable (number of rows) and “n” - the number of variables in
#'   the dataframe (number of columns).
#'
#' @param lambda the smoothing parameter; a numeric scalar which takes the
#'   default value of 1600, if unspecified by the user.
#'
#' @return a Txn dataframe, containing the trend data
#'
#' @details The length of the time series should be greater than four and the
#'   value of the smoothing parameter greater than zero for the code to function.
#'   Of course, having a sufficiently long time series is paramount to achieving
#'   meaningful results.
#'
#' @usage hp2(y, lambda = 1600)
#'
#' @keywords hp2 two-sided hp filter gdp macroeconomic smoothing time series
#'
#' @seealso [hp1()]
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
#' @import Matrix
#'
#' @examples
#' # Generate the data and plot it
#' set.seed(10)
#' y <- as.data.frame(rev(diffinv(rnorm(100)))[1:100])+30
#' colnames(y) <- "gdp"
#' plot(y$gdp, type="l")
#'
#' # Apply the HP filter to the data
#' ytrend = hp2(y)
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

hp2 <- function(y,lambda=1600) {

  ytrend <- matrix(data=NA,nrow=nrow(y),ncol=ncol(y)) # Create an empty matrix of the same size as "y", where the results will be stored
  T = nrow(y)
  n = ncol(y)
  # Preliminary calculations
  x1=rbind(c(1+lambda, -2*lambda, lambda)) # The non-zero elements of the first row of A
  x2=rbind(c(-2*lambda, 1+5*lambda, -4*lambda, lambda)) # The non-zero elements of the second row of A
  x3=rbind(c(lambda, -4*lambda, 1+6*lambda, -4*lambda, lambda)) # The non-zero elements of the j'th row of A, 2<j<T-2
  x2rev=x2[,ncol(x2):1] # Flip columns of matrix # The non-zero elements of the second-to-last row of A
  # x2rev=x2[nrow(x2):1,] # Flip rows of matrix # The non-zero elements of the last row of A
  x1rev=x1[,ncol(x1):1]
  x2rev <- t(as.matrix(x2rev))
  x1rev <- t(as.matrix(x1rev))

  # Make a list (a column vector) containing at position i the row of the i'th non-zero element in A
  I = matrix(3:(T-2), nrow = 1)
  I = do.call(rbind, replicate(5, I, simplify=FALSE)) # |5| (replicate 5 times)
  I = cbind(c(I))
  I = rbind(1,1,1,2,2,2,2,I,T-1,T-1,T-1,T-1,T,T,T)

  J = matrix(1:(T-4), nrow = 1)
  J = do.call(rbind, replicate(5, J, simplify=FALSE)) # |5| (replicate 5 times)
  J = cbind(c(J))

  Temp = matrix(0:4, nrow = 1)
  Temp = t(do.call(rbind, replicate(T-4, Temp, simplify=FALSE)))
  Temp = cbind(c(Temp))

  J = J + Temp
  J = rbind(1,2,3,1,2,3,4,J,T-3,T-2,T-1,T,T-2,T-1,T)

  # Make a list (a column vector) containing at position i the i'th non-zero element in A
  X = t(do.call(rbind, replicate(T-4, x3, simplify=FALSE))) # |5| (replicate 5 times)
  X = cbind(c(X))
  X = rbind(t(x1),t(x2),X,t(x2rev),t(x1rev))

  # Replicate vectors by the number of columns in the input dataframe
  MX = t(do.call(cbind, replicate(ncol(ytrend), X, simplify=FALSE)))
  MX <- t(as.matrix(MX))
  MI = t(do.call(cbind, replicate(ncol(ytrend), I, simplify=FALSE)))
  MI <- t(as.matrix(MI))
  MJ = t(do.call(cbind, replicate(ncol(ytrend), J, simplify=FALSE)))
  MJ <- t(as.matrix(MJ))

  A = sparseMatrix(i=as.vector(MI),j=as.vector(MJ),x=as.vector(MX))
  ytrend <- solve(A, matrix(unlist(y), ncol=ncol(y)), sparse = TRUE)

  ifelse(ncol(y) == 1,
         ytrend <- as.data.frame(ytrend),
         ytrend <- as.data.frame(matrix(ytrend@x, ncol=ncol(y))) * ncol(y) # Store "ytrend" as a tibble
  )
  colnm <- colnames(y) # Get column names from "y"
  colnames(ytrend) <- colnm # Name the columns of "ytrend" so that they have the same names as "y"
  return(ytrend)

}
