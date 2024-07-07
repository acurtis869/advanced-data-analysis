RSS <- function(yObs, yFit) {
  # RSS: Compute the residual sums of squares 
  # yObs: the observed response y
  # yFit: the fitted response y 
  return(sum((yObs - yFit)^2))
}

ASE <- function(yTruth, yFit) {
  # ASE: Compute the mean average squared error
  # yTruth: the true response y
  # yFit: the fitted response y
  return(mean((yTruth - yFit)^2))
}

# Libraries
library(mgcv) # penalised regression splines
library(MRSea) # SALSA

# Read simulated dataset from Moodle
df <- read.csv("SimulatedData.csv")
head(df)

## Polynomial
RSSpoly <- rep(NA, 100)
ASEpoly <- rep(NA, 100)
for (i in 1:100) {
  data <- df[df$ID == i, ]
  mdl <- lm(response ~ poly(x, degree = 6), data = data)
  RSSpoly[i] <- RSS(data$response, mdl$fitted.values)
  ASEpoly[i] <- ASE(data$mu, mdl$fitted.values)
}
data <- df[df$ID == 1, ]
mdl <- lm(response ~ poly(x, degree = 6), data = data)
plot(data$response, mdl$fitted.values)

library(tidyverse)
ggplot() + geom_point(aes(x = data$x, y = data$response)) +
  geom_line(aes(x = data$x, y = mdl$fitted.values, col = "fit")) +
  geom_line(aes(x = data$x, y = data$mu, col = "true"))

## Penalised Regression Splines
RSSprs <- rep(NA, 100)
ASEprs <- rep(NA, 100)
for (i in 1:100) {
  data <- df[df$ID == i, ]
  mdl <- mgcv::gam(response ~ s(x), data = data)
  RSSprs[i] <- RSS(data$response, mdl$fitted.values)
  ASEprs[i] <- ASE(data$mu, mdl$fitted.values)
}
data <- df[df$ID == 1, ]
mdl <- mgcv::gam(response ~ s(x), data = data)
plot(data$response, mdl$fitted.values)
ggplot() + geom_point(aes(x = data$x, y = data$response)) +
  geom_line(aes(x = data$x, y = mdl$fitted.values, col = "fit")) + 
  geom_line(aes(x = data$x, y = data$mu, col = "true"))

## Regression splines with SALSA
RSSsalsa <- rep(NA, 100)
ASEsalsa <- rep(NA, 100)
knots <- rep(NA, 100)
for (i in 1:100) {
  data <- df[df$ID == i, ]
  initialModel <- glm(response ~ 1, data = data)
  # Set SALSA arguments
  varList <- c("x")
  salsa1DList <- list(fitnessMeasure = "BIC",
                      minKnots_1d = 2, maxKnots_1d = 40,
                      startKnots_1d = 10, degree = 2,
                      maxIterations = 10, gaps = 0)
  # Run SALSA
  salsa <- MRSea::runSALSA1D(initialModel = initialModel,
                             salsa1dlist = salsa1DList,
                             varlist = varList,
                             factorlist = NULL,
                             datain = data,
                             splineParams = NULL,
                             suppress.printout = TRUE)
  RSSsalsa[i] <- RSS(data$response, salsa$bestModel$fitted.values)
  ASEsalsa[i] <- ASE(data$mu, salsa$bestModel$fitted.values)
  knots[i] <- length(salsa$splineParams[[2]]$knots)
}
data <- df[df$ID == 1, ]
initialModel <- glm(response ~ 1, data = data)
varList <- c("x")
salsa1DList <- list(fitnessMeasure = "BIC",
                    minKnots_1d = 2, maxKnots_1d = 40,
                    startKnots_1d = 10, degree = 2,
                    maxIterations = 10, gaps = 0)
# Run SALSA
salsa <- MRSea::runSALSA1D(initialModel = initialModel,
                           salsa1dlist = salsa1DList,
                           varlist = varList,
                           factorlist = NULL,
                           datain = data,
                           splineParams = NULL,
                           suppress.printout = TRUE)
plot(data$response, salsa$bestModel$fitted.values)
ggplot() + geom_point(aes(x = data$x, y = data$response)) +
  geom_line(aes(x = data$x, y = salsa$bestModel$fitted.values, col = "fit")) + 
  geom_line(aes(x = data$x, y = data$mu, col = "true"))

hist(knots)

ggplot() + geom_point(aes(x = knots, y = RSSsalsa))
ggplot() + geom_point(aes(x = knots, y = ASEsalsa))

mean(RSSpoly) / mean(RSSprs) # 1.368
mean(ASEpoly) / mean(ASEprs) # 1.831
mean(RSSpoly) / mean(RSSsalsa) # 2.251
mean(ASEpoly) / mean(ASEsalsa) # 8.391
mean(RSSprs) / mean(RSSsalsa) # 1.645
mean(ASEprs) / mean(ASEsalsa) # 4.584






