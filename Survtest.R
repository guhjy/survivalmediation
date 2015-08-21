library(medflex)
library(survival)

## WEIGHTING-BASED APPROACH
weightData <- neWeight(age ~ factor(sex) + ph.ecog, data = lung)
head(weightData)

neMod1 <- coxph(Surv(time, status) ~ sex0 + sex1 + ph.ecog, data = weightData, weights = weights(weightData))
coef(neMod1)

qqnorm(attr(weightData, "model")$residuals)
qqline(attr(weightData, "model")$residuals)


## IMPUTATION-BASED APPROACH

# debug(survival:::survreg)
impFit <- survreg(Surv(time, status) ~ factor(sex) + age + ph.ecog, data = lung)
# debug(medflex:::neImpute.default)
impData <- neImpute(impFit)

summary(impFit)
impData <- impData[rep(seq.int(dim(impData)[1]), each = 10), ]
impData$sex <- impData$sex0

### graphical illustration ###
plot(predict(impFit, newdata = impData[2, ], type = "quantile", p=seq(0,1,by=.01)), 1-seq(0,1,by=.01), type = "l")
p1 <- runif(1, 0, 1)
abline(h=p1, lty = 2)
t1 <- predict(impFit, newdata = impData[2, ], type = "quantile", p=1-p1)
abline(v=t1, lty = 2)
lines(predict(impFit, newdata = impData[1, ], type = "quantile", p=seq(0,1,by=.01)), 1-seq(0,1,by=.01), col = "red")
p2 <- runif(1, 0, 1)
abline(h=p2, lty = 2, col = "red")
t2 <- predict(impFit, newdata = impData[1, ], type = "quantile", p=1-p2)
abline(v=t2, lty = 2, col = "red")
abline(v=max(lung$time))
###

p <- runif(dim(impData)[1], 0, 1)
for (i in seq.int(dim(impData)[1])) {
  pred <- predict(impFit, newdata = impData[i, ], type = "quantile", p = 1-p[i])
  if (is.na(pred)) {
    impData[i, "status"] <- impData[i, "time"] <- NA
  } else if (pred > max(lung$time)) {
    impData[i, "status"] <- 1
    impData[i, "time"] <- max(lung$time)
  } else {
    impData[i, "status"] <- 2
    impData[i, "time"] <- pred
  }  
}

# impData <- neImpute(Surv(time, status) ~ factor(sex) + age + ph.ecog, data = lung, FUN = survreg)

neMod2 <- coxph(Surv(time, status) ~ sex0 + sex1 + ph.ecog, data = impData)
coef(neMod2)