# Libraries
library(tidyverse) # ggplot(.) 
library(car) # vif(.) 
library(lawstat) # runs.test(.)

# Read dataset
df <- read.csv("HornsRev.csv")

# Set Impact as factor
df$Impact <- as.factor(df$Impact)
glmFitOD2 <- glm(Nhat ~ Impact + Depth + XPos + YPos, offset=log(Area), family=quasipoisson, data=df)
summary(glmFitOD2)

glmFitOD3 <- glm(Nhat ~ Impact*XPos + Impact*YPos + Depth, offset=log(Area), family=quasipoisson, data=df)
summary(glmFitOD3)

covariates <- c("XPos", "YPos", "Depth") 
pairs(subset(df, select=covariates),
      upper.panel=NULL, pch=19, cex=0.3)

car::vif(glmFitOD2)
car::vif(glmFitOD3)

xmatrix <- model.matrix(glmFitOD3)
head(xmatrix)
pairs(xmatrix)

# Detecting residual autocorrelation
set.seed(101)
testVals <- rnorm(50)
plot(sign(testVals), type = "l")
# Now for the first 800 of the working model
plot(sign(residuals(glmFitOD3, type = "pearson")[1:800]),
     type = "l", ylab = "Sign of the residuals")
# Now perform a runs test
lawstat::runs.test(residuals(glmFitOD3, type = "pearson"))

# Diagnosing nonlinearities on the link scale
residualPlots(glmFitOD3,
              type = "pearson",
              terms = ~Depth,
              quadratic = TRUE,
              smooth = list(smoother=gamLine, col = "red"),
              fitted = FALSE,
              col.quad = "blue",
              col = "grey",
              pch = 19,
              cex = 0.3,
              ylim = c(-20, 20))

par(mfrow = c(1, 2))
plot(sign(residuals(glmFitOD3, type = "pearson")[order(df$Depth)])[1:100],
     type = "l", main = "100 Pearson residuals in depth order",
     ylab = "Pearson residuals")
plot(sign(rnorm(100)), type = "l", main = "Random values")

lawstat::runs.test(residuals(glmFitOD3, type = "pearson")[order(df$Depth)])

# Examine the Pearson's residuals in relation to XPos
par(mfrow = c(1,1))
plot(xmatrix[, 3], residuals(glmFitOD3, type = "pearson"))
plot(sign(residuals(glmFitOD3, type = "pearson")[order(df$XPos)])[1:10000],
     type = "l", main = "100 Pearson residuals in x-pos order",
     ylab = "Pearson residuals")

lawstat::runs.test(residuals(glmFitOD3, type = "pearson")[order(df$XPos)])

# And the same now for YPos
plot(xmatrix[, 4], residuals(glmFitOD3, type = "pearson"))
plot(sign(residuals(glmFitOD3, type = "pearson")[order(df$YPos)])[1:1000],
     type = "l", main = "100 Pearson residuals in y-pos order",
     ylab = "Pearson residuals")

lawstat::runs.test(residuals(glmFitOD3, type = "pearson")[order(df$YPos)])

residualPlots(glmFitOD3,
              type = "pearson",
              terms = ~Depth + XPos + YPos,
              quadratic = TRUE,
              smooth = list(smoother=gamLine, col = "red"),
              fitted = FALSE,
              col.quad = "blue",
              col = "grey",
              pch = 19,
              cex = 0.3,
              ylim = c(-20, 20))