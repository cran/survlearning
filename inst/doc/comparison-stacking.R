## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(survlearning)

## ----data---------------------------------------------------------------------
set.seed(42)
x <- rnorm(400)
z <- rnorm(400)
failure <- exp(2 - .4 * x + .2 * z + rnorm(400, sd = .8))
censor <- rexp(400, .035)
d <- data.frame(time = pmin(failure, censor),
  event = as.integer(failure <= censor), x, z)
train <- d[1:240, ]
test <- d[241:400, ]
f <- Surv(time, event) ~ x + z
cox <- surv_fit(f, train, "cox")
aft <- surv_fit(f, train, "aft", params = list(distribution = "lognormal"))

## ----paired-------------------------------------------------------------------
surv_compare(list(cox = cox, aft = aft), test,
  reference = "cox", n_boot = 100, seed = 9)

## ----calibration--------------------------------------------------------------
surv_calibrate(aft, test, times = 5, type = "metrics",
  n_boot = 100, seed = 9)

## ----stack--------------------------------------------------------------------
stack <- surv_stack(f, train, methods = c("cox", "aft"),
  params = list(aft = list(distribution = "lognormal")),
  times = c(3, 5, 8), folds = 3, seed = 9)
stack$weights
surv_predict(stack, test[1:2, ], "survival", times = c(3, 5, 8))
surv_compare(list(cox = cox, aft = aft, stack = stack), test,
  metrics = c("harrell", "brier"), times = 5)

