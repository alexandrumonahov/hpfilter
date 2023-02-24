#' @keywords internal
#' @details
#'   hp1 - runs the one-sided HP filter
#'
#'   hp2 - runs the two-sided HP filter
#'
#'   GDPEU - contains a sample GDP dataset
#'
#'   This package implements in R a version of the HP filter very close to that
#'   specified by Meyer-Gohde, A. (2010). Please see the "References" section
#'   for more details.
#'
#' @examples
#' # Generate sample data
#' set.seed(10)
#' y <- as.data.frame(rev(diffinv(rnorm(100)))[1:100])+30
#'
#' # Run hpfilter functions
#' hp1(y, lambda = 400000) # Calculate the one-sided HP filter using a
#'                         # smoothing parameter of 400000
#'
#' hp2(y, lambda = 1600)   # Calculate the two-sided HP filter using a
#'                         # smoothing parameter of 1600
#'
#' @author Alexandru Monahov
#' @references Balcilar, M. (2019). Miscellaneous Time Series Filters ‘mFilter’.
#'  CRAN R Package Library.
#'
#' Drehmann, M., and Yetman, J. (2018). Why You Should Use the Hodrick-Prescott
#'  Filter – at Least to Generate Credit Gaps. BIS Working Paper No. 744.
#'
#' Eurostat (2023), Real Gross Domestic Product for European Union (28 countries)
#'  [CLVMNACSCAB1GQEU28], National Accounts - GDP.
#'
#' Hamilton, J. D. (2017). ‘Why You Should Never Use the Hodrick-Prescott Filter’.
#'  Working Paper Series. National Bureau of Economic Research, May 2017.
#'
#' Hodrick, R. J., and Prescott, E. C. (1997). Postwar U.S. Business Cycles: An
#'  Empirical Investigation. Journal of Money, Credit, and Banking 29: 1-16.
#'
#' Hyeongwoo, K. (2004). "Hodrick–Prescott Filter". Notes, Auburn University.
#'
#' Mcelroy, T. (2008). Exact formulas for the Hodrick-Prescott Filter.
#'  Econometrics Journal. 11. 209-217.
#'
#' Meyer-Gohde, A. (2010). Matlab code for one-sided HP-filters. QM&RBC Codes 181,
#'  Quantitative Macroeconomics & Real Business Cycles.
#'
#' Ravn, M., and Uhlig, H. (2002). On adjusting the Hodrick-Prescott filter for
#'  the frequency of observations, The Review of Economics and Statistics 2002;
#'  84 (2): 371–376.
#'
#' Shea, J. (2021). neverhpfilter: An Alternative to the Hodrick-Prescott Filter.
#'  CRAN R Package Library.
#'
#' @source Copyright Alexandru Monahov, 2023. You may use, modify and redistribute
#'  this code, provided that you give credit to the author and make any derivative
#'  work available to the public for free.
"_PACKAGE"
