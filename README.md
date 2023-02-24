# hpfilter <img src="man/figures/logo.png" align="right" height="136" />

## An R Implementation of the One- and Two-Sided Hodrick-Prescott Filter

### **hpfilter** is a package that implements the one-sided and two-sided versions of the Hodrick-Prescott filter in R.

The two-sided implementation uses sparse matrices to enhance the efficiency of calculations when using large datasets. Its traditional use has been in macroeconomics, to smooth out the time series of variables such as the GDP. The one-sided version is based on the Kalman filter. An important use of the one-sided HP filter is for the calculation of the credit gap, according to the Basel III methodology for setting the Countercyclical Capital Buffer (CCyB) guide.

The Hodrick-Prescott filter is a technique commonly used to smooth macroeconomic data such as the GDP. It consists in separating short-term, cyclical movements in the data from the long-term trend. Although the HP filter has received numerous critiques, recent studies show that alternative methods do not necessarily perform better. Furthermore, the Basel committee, an international body responsible for setting international guidelines for prudential regulation, has recommended the usage of the one-sided HP filter for calculations pertaining to the analysis of financial cycles.

### [Paper (ResearchGate)](http://dx.doi.org/10.13140/RG.2.2.12251.85282)

### v1.0.0

## About hpfilter

**hpfilter** consists of the following functions:

### hp1 - the One-Sided HP Filter
This function applies the one-sided Hodrick-Prescott filter to the selected data, for the given smoothing parameter value.

In R, simply call the function:

**hp1(_y_, _lambda_, _x_user_, _P_user_, _discard_ )**

_y_	–	a dataframe of size Txn, where “T” is the number of observations for each variable (number of rows) and “n” - the number of variables in the dataframe (number of columns).

_λ_	–	the smoothing parameter; a numeric scalar which takes the default value of 1600, if unspecified by the user.

_x_user_	–	user defined initial values of the state estimate for each variable in y. Takes the form of a 2xn matrix. Since the underlying state vector is 2x1, two values are needed for each variable in y.
By default: if no values are provided, by default, backwards extrapolations based on the first two observations are used.

_P_user_	–	a structural array with n elements, each of which being a 2x2 matrix of initial MSE estimates for each variable in y.
By default: If no values are provided, the default matrix with large variances is used.

_discard_	–	the number of discard periods, expressed as a numeric scalar. The user specified amount of values will be discarded from the start of the sample, resulting in output matrices of size (T-discard)xn.
By default: If no values are provided, the value of 0 is used.

 ### hp2 - the Two-Sided HP Filter
 This function applies the two-sided Hodrick-Prescott filter to the selected data, for the given smoothing parameter value.

**hp2(_y_, _lambda_)**

_y_	–	a dataframe of size Txn, where “T” is the number of observations for each variable (number of rows) and “n” - the number of variables in the dataframe (number of columns).

_λ_	–	the smoothing parameter; a numeric scalar which takes the default value of 1600, if unspecified by the user.

## Technical details

The value of the smoothing parameter should be positive. Furthermore, the code needs at least four observations in order to run. That said, the quality of the estimation will improve with the length of the time series.

## Download and installation of development version

#### Online, from Github:

You can download **hpfilter** directly from Github. To do so, you need to have the **devtools** package installed and loaded. Once you are in **R**, run the following commands:

> install.packages("devtools")
>
> library("devtools")
>
> install_github("alexandrumonahov/hpfilter")

You may face downloading errors from Github if you are behind a firewall or there are https download restrictions. To avoid this, you can try running the following commands:

> options(download.file.method = "libcurl")
>
> options(download.file.method = "wininet")

Once the package is installed, you can run it using the: **library(hpfilter)** command.

## Author details

Alexandru Monahov, 2023

