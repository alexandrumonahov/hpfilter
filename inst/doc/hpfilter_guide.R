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
# Keep only the y series and store in df object
y = data.frame(gdp=c(2402903.9, 2416576.3, 2428126.1, 2437407.9, 2443883.8, 2459908.3, 2474557.8, 2487734.5, 2502900.4, 2531872, 2550913.6, 2579582.2, 2598307.2, 2611524.5, 2628335, 2636377, 2662108.5, 2673788.6, 2706474.2, 2739615.4, 2771013.4, 2796237.5, 2810427.4, 2827455.4, 2854217.7, 2860625.6, 2868091.8, 2873472.3, 2879547.6, 2895157.8, 2908087.9, 2918434.9, 2917344.7, 2925034.7, 2945193.3, 2968681, 2985760.1, 3003359.6, 3011430.9, 3024493.3, 3034608.1, 3057625.7, 3082932, 3107995.9, 3134685.3, 3167958.6, 3184861.1, 3215225.8, 3239777.4, 3262175.7, 3280780.3, 3300040, 3318443, 3308233.2, 3285991.3, 3224235.3, 3137585.4, 3134888.1, 3145754.4, 3158823.5, 3173245.5, 3205158.6, 3222254.3, 3239486.8, 3266120.1, 3268551.9, 3275857.1, 3266151.1, 3264311.3, 3257251.5, 3260049.6, 3247444.9, 3243545.5, 3261329.2, 3275659.2, 3288417.7, 3304747.7, 3315578, 3334340.8, 3348942.3, 3372453.9, 3390320.1, 3407279, 3424800.8, 3440244.9, 3451983.1, 3467597.7, 3494562.1, 3519862.5, 3546513.9, 3572479.2, 3598925.7, 3603964.3, 3624883.5, 3630997.8, 3652886.3, 3676573.4, 3689104.4, 3699971.7, 3702073.3))
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


