## ----setup, include = FALSE, echo=FALSE---------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  error = TRUE,
  comment = "#>"
)

## ---- echo=FALSE, results='asis'----------------------------------------------
data(GDPEU)
knitr::kable(head(GDPEU, 10))

## -----------------------------------------------------------------------------
# R CODE - Smoothing GDP data with the two-sided HP filter
# Load library
library(hpfilter)
# Load dataset
data(GDPEU)
# Keep only the y series and store in df object
y = GDPEU[,-1]
# Run the two-sided HP filter with the default value of 1600
# for lambda
ytrend = hp2(y)
# Calculate cyclical component
ycycle = y - ytrend

## ---- out.width="100%", fig.width=10, fig.height=7.5--------------------------
# Plot
plot(y$gdp, type="l", col="black", lty=1)
lines(ytrend$gdp, col="#066462")
legend("bottom", horiz=TRUE, cex=0.75, c("y", "ytrend"), lty = 1, col = c("black", "#066462"))

## ---- out.width="100%", fig.width=10, fig.height=7.5--------------------------
plot(ycycle$gdp, type = "l")
abline(h=0)

## ---- out.width="100%", fig.width=10, fig.height=7.5--------------------------
# Generate the data and plot it
set.seed(10)
y <- as.data.frame(rev(diffinv(rnorm(100)))[1:100])+30
colnames(y) <- "gdp"
plot(y$gdp, type="l")

# Apply the HP filter to the data
ytrend = hp1(y)
ycycle = y - ytrend

# Plot the three resulting series
plot(y$gdp, type="l", col="black", lty=1, ylim=c(-10,30))
lines(ytrend$gdp, col="#066462")
polygon(c(1, seq(ycycle$gdp), length(ycycle$gdp)), c(0, ycycle$gdp, 0), col = "#E0F2F1")
legend("bottom", horiz=TRUE, cex=0.75, c("y", "ytrend", "ycycle"), lty = 1,
       col = c("black", "#066462", "#75bfbd"))


