# Libraries
library(tidyverse) # ggplot(.) 
library(mgcv) # gam(.) 
library(splines) # bs(.) (B-splines) 
library(MuMIn) # dredge(.) 
library(fields) # quilt.plot(.) 
library(lawstat) # runs.test(.)

# Read dataset
df <- read.csv("HornsRev.csv")

# Set Impact as factor
df$Impact <- as.factor(df$Impact)

PRS <- mgcv::gam(Nhat ~ s(XPos) + s(YPos) + s(Depth) + Impact, 
                 data=df, family=quasipoisson, offset=log(Area))
summary(PRS)

plot(PRS, shade = TRUE , residuals = TRUE, ylim = c(-10,10))
plot(PRS, shade = TRUE , residuals = TRUE)

par(mfrow = c(1, 2))
# Replace s(Depth) with bs(Depth, knots = 20) in the model
PRS_B <- stats::update(PRS, .~. -s(Depth) + splines::bs(Depth, knots = 20))
summary(PRS_B)
stats::termplot(PRS_B, se = TRUE)

# Create poisson to get likelihood
PRSpois <- mgcv::gam(Nhat ~ s(XPos) + s(YPos) + s(Depth) + Impact, 
                     data=df, family=poisson, offset=log(Area))

options(na.action="na.fail") # fail-safe 
dredge(PRSpois, rank="QAIC", chat = PRS$scale)

# Read prediction grid
predData <- read.csv("HornsRevPredictionData.csv")
predData$Impact <- as.factor(predData$Impact)

# Predict on the link scale
NhatPredLink <- predict(PRS, newdata = predData, se = TRUE, type = "link")

# Predict on the response scale
NhatPredRes <- predict(PRS, newdata = predData, se = TRUE, type = "response")

# Produce prediction spatial plots

# On the link scale
par(mfrow = c(1, 2))
fields::quilt.plot(predData$XPos[predData$Impact == 0],
                   predData$YPos[predData$Impact == 0],
                   NhatPredLink$fit[1:(nrow(predData)/2)],
                   zlim=range(NhatPredLink$fit),
                   main = paste0("Impact 0"))
fields::quilt.plot(predData$XPos[predData$Impact == 1],
                   predData$YPos[predData$Impact == 1],
                   NhatPredLink$fit[((nrow(predData)/2)+1):nrow(predData)],
                   zlim=range(NhatPredLink$fit),
                   main = paste0("Impact 1"))

# On the response scale
par(mfrow = c(1, 2))
fields::quilt.plot(predData$XPos[predData$Impact == 0],
                   predData$YPos[predData$Impact == 0],
                   NhatPredRes$fit[1:(nrow(predData)/2)],
                   zlim=range(NhatPredRes$fit),
                   main = paste0("Impact 0"))
fields::quilt.plot(predData$XPos[predData$Impact == 1],
                   predData$YPos[predData$Impact == 1],
                   NhatPredRes$fit[((nrow(predData)/2)+1):nrow(predData)],
                   zlim=range(NhatPredRes$fit),
                   main = paste0("Impact 1"))

# Fit a GAM with an interaction term
PRS_Int <- mgcv::gam(Nhat ~ s(XPos, by = Impact) + s(YPos) + s(Depth) + Impact,
                     data = df, family = quasipoisson, offset = log(Area))
summary(PRS_Int)

# Produce spatial plots for this model

# Predict on the link scale
NhatPredLink_Int <- predict(PRS_Int, newdata = predData, se = TRUE, type = "link")

# Predict on the response scale
NhatPredRes_Int <- predict(PRS_Int, newdata = predData, se = TRUE, type = "response")

# Produce prediction spatial plots

# On the link scale
par(mfrow = c(1, 2))
fields::quilt.plot(predData$XPos[predData$Impact == 0],
                   predData$YPos[predData$Impact == 0],
                   NhatPredLink_Int$fit[1:(nrow(predData)/2)],
                   zlim=range(NhatPredLink_Int$fit),
                   main = paste0("Impact 0"))
fields::quilt.plot(predData$XPos[predData$Impact == 1],
                   predData$YPos[predData$Impact == 1],
                   NhatPredLink_Int$fit[((nrow(predData)/2)+1):nrow(predData)],
                   zlim=range(NhatPredLink_Int$fit),
                   main = paste0("Impact 1"))

# On the response scale
par(mfrow = c(1, 2))
fields::quilt.plot(predData$XPos[predData$Impact == 0],
                   predData$YPos[predData$Impact == 0],
                   NhatPredRes_Int$fit[1:(nrow(predData)/2)],
                   zlim=range(NhatPredRes_Int$fit),
                   main = paste0("Impact 0"))
fields::quilt.plot(predData$XPos[predData$Impact == 1],
                   predData$YPos[predData$Impact == 1],
                   NhatPredRes_Int$fit[((nrow(predData)/2)+1):nrow(predData)],
                   zlim=range(NhatPredRes_Int$fit),
                   main = paste0("Impact 1"))

lawstat::runs.test(residuals(PRS_Int, type = "pearson"))


