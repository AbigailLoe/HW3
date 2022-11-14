## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(myHW3)

## -----------------------------------------------------------------------------
X = matrix(rnorm(666), ncol = 3)
Y = sample(1:nrow(X)) +rnorm(nrow(X), mean = 0, sd = .2)

## -----------------------------------------------------------------------------
model1 = linearModel(X, Y)

## -----------------------------------------------------------------------------
dat = cbind.data.frame(X, Y)
names(dat) = c("x1", "x2", "x3", "y")
test1 = lm(y~., data = dat)

## -----------------------------------------------------------------------------
summary(test1)
model1

## -----------------------------------------------------------------------------
lmPval = summary(test1)$coefficients[,4]
names(lmPval) = NULL
all.equal(unlist(round(model1["p-value"], 3), use.names = FALSE),
          round(lmPval, 3))

## -----------------------------------------------------------------------------
lmEst = summary(test1)$coefficients[,1]
names(lmEst) = NULL
all.equal(unlist(round(model1["Estimate"], 5), use.names = FALSE),
          round(lmEst, 5))

## -----------------------------------------------------------------------------
lmT = summary(test1)$coefficients[,3]
names(lmT) = NULL
all.equal(unlist(round(model1["t-value"], 3), use.names = FALSE),
          round(lmT, 3))

## -----------------------------------------------------------------------------
lmSHat = summary(test1)$coefficients[,2]
names(lmSHat) = NULL
all.equal(unlist(round(model1["Std. Error"], 5), use.names = FALSE),
          round(lmSHat, 5))

