# Libraries
library(tidyverse) # ggplot(.) 
library(glmnet) # for regularised regression

# Read dataset
df <- read.csv("HornsRev.csv")

# Set Impact as factor
df$Impact <- as.factor(df$Impact)

# Same as glmFitOD3 but covariates are scaled
glmFitOD3Scale <- glm(Nhat ~ Impact*scale(XPos) + Impact*scale(YPos) + 
                        scale(Depth), offset=log(Area), 
                      family=quasipoisson, data=df)

# Create design/model matrix
# Note: ignore the intercept term as glmnet already introduces one by default. 
xmatrix <- model.matrix(glmFitOD3Scale)
xmatrix <- xmatrix[, 2:ncol(xmatrix)]
head(xmatrix)

## Ridge Regression
# Fit the model and use cross validation to select lambda
ridge <- glmnet(xmatrix, df$Nhat, family="poisson", 
                offset=log(df$Area), alpha=0)
cvridge <- cv.glmnet(xmatrix, df$Nhat, family="poisson", 
                     offset=log(df$Area), alpha=0, nfolds=10)
# Examine the results
par(mfrow=c(1, 2))
plot(ridge, xvar="lambda") 
abline(v=log(cvridge$lambda.min)) 
plot(cvridge) 
abline(v=log(cvridge$lambda.min)) 
abline(v=log(cvridge$lambda.1se), lty=2)
# Examine which values of lambda were trialled and which was chosen
log(cvridge$lambda) 
log(cvridge$lambda.min)
# Investigate difference between ridge and glm
# quasi-Poisson fit
coefGLM <- as.data.frame(coef(glmFitOD3Scale)) 
colnames(coefGLM) <- "GLM"
coefGLM$Covariate <- row.names(coefGLM)
# Add 95% confidence intervals
confInt <- as.data.frame(confint(glmFitOD3Scale, level=0.95))
colnames(confInt) <- c("CI_Lower", "CI_Upper") 
confInt$Covariate <- row.names(confInt)
# Merge together
coefGLM <- dplyr::inner_join(coefGLM, confInt, by="Covariate")
# Ridge regression
coefRidge <- as.data.frame(as.matrix(coef(cvridge, s="lambda.min"))) 
colnames(coefRidge) <- "Ridge"
coefRidge$Covariate <- row.names(coefRidge)
# Merge data frames
mdlCoefs <- dplyr::inner_join(coefGLM, coefRidge, by="Covariate")
# Display differences
print(mdlCoefs)
# Plot (ignore intercept term just for scaling reasons) 
# blue = GLM (with CIs)
# red = Ridge regression
ggplot(subset(mdlCoefs, !Covariate %in% "(Intercept)")) +
  geom_point(aes(x=Covariate, y=GLM), col="#377eb8") + 
  geom_linerange(aes(x=Covariate, ymin=CI_Lower, ymax=CI_Upper),
                 col="#377eb8") + 
  geom_point(aes(x=Covariate, y=Ridge), col="#e41a1c") +
  ylab("Compare GLM to ridge coefficients") + 
  theme(axis.text.x=element_text(angle=90), legend.position="none")
# Do the ridge coeffs lie within 95% CIs of the glmOD?
ifelse(mdlCoefs$Ridge > mdlCoefs$CI_Lower & 
         mdlCoefs$Ridge < mdlCoefs$CI_Upper, TRUE, FALSE)
# Yes for all

## LASSO

lasso <- glmnet(xmatrix, df$Nhat, family="poisson", 
                offset=log(df$Area), alpha=1)
cvlasso <- cv.glmnet(xmatrix, df$Nhat, family="poisson", 
                     offset=log(df$Area), alpha=1, nfolds=10)
# Examine the results
par(mfrow=c(1, 2))
plot(lasso, xvar="lambda") 
abline(v=log(cvlasso$lambda.min)) 
plot(cvlasso) 
abline(v=log(cvlasso$lambda.min)) 
abline(v=log(cvlasso$lambda.1se), lty=2)
# Examine which values of lambda were trialled and which was chosen
log(cvlasso$lambda) 
log(cvlasso$lambda.min)
# Investigate difference between lasso and glm
coefLasso <- as.data.frame(as.matrix(coef(cvlasso, s="lambda.min"))) 
colnames(coefLasso) <- "LASSO"
coefLasso$Covariate <- row.names(coefLasso)
# Merge data frames
mdlCoefs <- dplyr::inner_join(mdlCoefs, coefLasso, by="Covariate")
# Display differences
print(mdlCoefs)
# Plot (ignore intercept term just for scaling reasons) 
# blue = GLM (with CIs)
# red = Ridge regression
ggplot(subset(mdlCoefs, !Covariate %in% "(Intercept)")) +
  geom_point(aes(x=Covariate, y=GLM), col="#377eb8") + 
  geom_linerange(aes(x=Covariate, ymin=CI_Lower, ymax=CI_Upper),
                 col="#377eb8") + 
  geom_point(aes(x=Covariate, y=LASSO), col="#e41a1c") +
  ylab("Compare GLM to ridge coefficients") + 
  theme(axis.text.x=element_text(angle=90), legend.position="none")
# Do the ridge coeffs lie within 95% CIs of the glmOD?
ifelse(mdlCoefs$LASSO > mdlCoefs$CI_Lower & 
         mdlCoefs$LASSO < mdlCoefs$CI_Upper, TRUE, FALSE)
# Yes for all again

## Elastic Net

net <- glmnet(xmatrix, df$Nhat, family="poisson", 
                offset=log(df$Area), alpha=0.6)
cvnet <- cv.glmnet(xmatrix, df$Nhat, family="poisson", 
                     offset=log(df$Area), alpha=0.6, nfolds=10)
# Examine the results
par(mfrow=c(1, 2))
plot(net, xvar="lambda") 
abline(v=log(cvnet$lambda.min)) 
plot(cvnet) 
abline(v=log(cvnet$lambda.min)) 
abline(v=log(cvnet$lambda.1se), lty=2)
# Examine which values of lambda were trialled and which was chosen
log(cvnet$lambda) 
log(cvnet$lambda.min)
# Investigate difference between lasso and glm
coefNet <- as.data.frame(as.matrix(coef(cvnet, s="lambda.min"))) 
colnames(coefNet) <- "Net"
coefNet$Covariate <- row.names(coefNet)
# Merge data frames
mdlCoefs <- dplyr::inner_join(mdlCoefs, coefNet, by="Covariate")
# Display differences
print(mdlCoefs)
# Plot (ignore intercept term just for scaling reasons) 
# blue = GLM (with CIs)
# red = Ridge regression
ggplot(subset(mdlCoefs, !Covariate %in% "(Intercept)")) +
  geom_point(aes(x=Covariate, y=GLM), col="#377eb8") + 
  geom_linerange(aes(x=Covariate, ymin=CI_Lower, ymax=CI_Upper),
                 col="#377eb8") + 
  geom_point(aes(x=Covariate, y=Net), col="#e41a1c") +
  ylab("Compare GLM to ridge coefficients") + 
  theme(axis.text.x=element_text(angle=90), legend.position="none")
# Do the ridge coeffs lie within 95% CIs of the glmOD?
ifelse(mdlCoefs$Net > mdlCoefs$CI_Lower & 
         mdlCoefs$Net < mdlCoefs$CI_Upper, TRUE, FALSE)
# Yes for all again

## Compare all

# blue = GLM (with CIs) 
# red = Ridge
# green = LASSO
# purple = Elastic net 
ggplot(subset(mdlCoefs, !Covariate %in% "(Intercept)")) +
  geom_point(aes(x=Covariate, y=GLM), col="#377eb8") + 
  geom_linerange(aes(x=Covariate, ymin=CI_Lower, ymax=CI_Upper),
                 col="#377eb8") + 
  geom_point(aes(x=Covariate, y=Ridge), col="#e41a1c") +
  geom_point(aes(x=Covariate, y=LASSO), col="#4daf4a") + 
  geom_point(aes(x=Covariate, y=Net), col="#984ea3") + 
  ylab("Comparing regression coefficients") + 
  theme(axis.text.x=element_text(angle=90),
        legend.position="none")

mean(cvridge$cvm)
mean(cvlasso$cvm)
mean(cvnet$cvm)

#> mean(cvridge$cvm)
#[1] 297.8179
#> mean(cvlasso$cvm)
#[1] 284.4915
#> mean(cvnet$cvm)
#[1] 285.0103

