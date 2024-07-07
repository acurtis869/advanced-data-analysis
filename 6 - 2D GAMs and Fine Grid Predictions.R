# Libraries
library(mgcv) # gam(.) 
library(fields) # quilt.plot(.)

# Read dataset
df <- read.csv("HornsRev.csv")

# Set Impact as factor
df$Impact <- as.factor(df$Impact)

# Produce plot
quilt.plot(df[df$Impact == 0,]$XPos, df[df$Impact == 0,]$YPos,
           df[df$Impact == 0,]$Nhat)
quilt.plot(df[df$Impact == 1,]$XPos, df[df$Impact == 1,]$YPos,
           df[df$Impact == 1,]$Nhat)

# Create 2D spline model
PRS_2D <- mgcv::gam(Nhat ~ s(XPos, YPos) + s(Depth) + Impact,
                    family = quasipoisson, data = df, offset = log(Area))
summary(PRS_2D)

# Plot partials
plot(PRS_2D, shade = TRUE)

# Use fine grid to make predictions and plot
# Link scale
dfPred <- read.csv("HornsRevPredictionData.csv")
PrePredLink <- predict(PRS_2D, newdata = dfPred[dfPred$Impact == 0,], 
                       type = "link")
PostPredLink <- predict(PRS_2D, newdata = dfPred[dfPred$Impact == 1,],
                        type = "link")

quilt.plot(dfPred[dfPred$Impact == 0,]$XPos,
           dfPred[dfPred$Impact == 0,]$YPos,
           PrePredLink, zlim = range(PrePredLink, PostPredLink),
           main = "link pre")
quilt.plot(dfPred[dfPred$Impact == 1,]$XPos,
           dfPred[dfPred$Impact == 1,]$YPos,
           PostPredLink, zlim = range(PrePredLink, PostPredLink),
           main = "link post")
# Response scale
PrePredResp <- predict(PRS_2D, newdata = dfPred[dfPred$Impact == 0,], 
                       type = "response")
PostPredResp <- predict(PRS_2D, newdata = dfPred[dfPred$Impact == 1,],
                        type = "response")

quilt.plot(dfPred[dfPred$Impact == 0,]$XPos,
           dfPred[dfPred$Impact == 0,]$YPos,
           PrePredResp, zlim = range(PrePredResp, PostPredResp),
           main = "response pre")
quilt.plot(dfPred[dfPred$Impact == 1,]$XPos,
           dfPred[dfPred$Impact == 1,]$YPos,
           PostPredResp, zlim = range(PrePredResp, PostPredResp),
           main = "response post")

# Fit model with an interaction term
PRS_2DInt <- mgcv::gam(Nhat ~ s(XPos, YPos, by = Impact) + s(Depth) + Impact,
                    family = quasipoisson, data = df, offset = log(Area))
summary(PRS_2DInt)

# View partial plots
plot(PRS_2DInt, shade = TRUE)

# Make predictions with this model
PrePredLink <- predict(PRS_2DInt, newdata = dfPred[dfPred$Impact == 0,], 
                       type = "link")
PostPredLink <- predict(PRS_2DInt, newdata = dfPred[dfPred$Impact == 1,],
                        type = "link")

quilt.plot(dfPred[dfPred$Impact == 0,]$XPos,
           dfPred[dfPred$Impact == 0,]$YPos,
           PrePredLink, zlim = range(PrePredLink, PostPredLink),
           main = "interaction link pre")
quilt.plot(dfPred[dfPred$Impact == 1,]$XPos,
           dfPred[dfPred$Impact == 1,]$YPos,
           PostPredLink, zlim = range(PrePredLink, PostPredLink),
           main = "interaction link post")
# Response scale
PrePredResp <- predict(PRS_2DInt, newdata = dfPred[dfPred$Impact == 0,], 
                       type = "response")
PostPredResp <- predict(PRS_2DInt, newdata = dfPred[dfPred$Impact == 1,],
                        type = "response")

quilt.plot(dfPred[dfPred$Impact == 0,]$XPos,
           dfPred[dfPred$Impact == 0,]$YPos,
           PrePredResp, zlim = range(PrePredResp, PostPredResp),
           main = "interaction response pre")
quilt.plot(dfPred[dfPred$Impact == 1,]$XPos,
           dfPred[dfPred$Impact == 1,]$YPos,
           PostPredResp, zlim = range(PrePredResp, PostPredResp),
           main = "interaction response post")

# Set k = 15
PRS_2DInt15 <- mgcv::gam(Nhat ~ s(XPos, YPos, by = Impact, k = 15) + s(Depth) + Impact,
                         family = quasipoisson, data = df, offset = log(Area))
summary(PRS_2DInt15)

# Set k = 40
PRS_2DInt40 <- mgcv::gam(Nhat ~ s(XPos, YPos, by = Impact, k = 40) + s(Depth) + Impact,
                         family = quasipoisson, data = df, offset = log(Area))
summary(PRS_2DInt40)

# Produce spatial plots for these models
# For the k = 15 model
# Make predictions with this model
PrePredLink <- predict(PRS_2DInt15, newdata = dfPred[dfPred$Impact == 0,], 
                       type = "link")
PostPredLink <- predict(PRS_2DInt15, newdata = dfPred[dfPred$Impact == 1,],
                        type = "link")

quilt.plot(dfPred[dfPred$Impact == 0,]$XPos,
           dfPred[dfPred$Impact == 0,]$YPos,
           PrePredLink, zlim = range(PrePredLink, PostPredLink),
           main = "k = 15 link pre")
quilt.plot(dfPred[dfPred$Impact == 1,]$XPos,
           dfPred[dfPred$Impact == 1,]$YPos,
           PostPredLink, zlim = range(PrePredLink, PostPredLink),
           main = "k = 15 link post")
# Response scale
PrePredResp <- predict(PRS_2DInt15, newdata = dfPred[dfPred$Impact == 0,], 
                       type = "response")
PostPredResp <- predict(PRS_2DInt15, newdata = dfPred[dfPred$Impact == 1,],
                        type = "response")

quilt.plot(dfPred[dfPred$Impact == 0,]$XPos,
           dfPred[dfPred$Impact == 0,]$YPos,
           PrePredResp, zlim = range(PrePredResp, PostPredResp),
           main = "k = 15 response pre")
quilt.plot(dfPred[dfPred$Impact == 1,]$XPos,
           dfPred[dfPred$Impact == 1,]$YPos,
           PostPredResp, zlim = range(PrePredResp, PostPredResp),
           main = "k = 15 response post")

# For the k = 40 model
# Make predictions with this model
PrePredLink <- predict(PRS_2DInt40, newdata = dfPred[dfPred$Impact == 0,], 
                       type = "link")
PostPredLink <- predict(PRS_2DInt140, newdata = dfPred[dfPred$Impact == 1,],
                        type = "link")

quilt.plot(dfPred[dfPred$Impact == 0,]$XPos,
           dfPred[dfPred$Impact == 0,]$YPos,
           PrePredLink, zlim = range(PrePredLink, PostPredLink),
           main = "k = 40 link pre")
quilt.plot(dfPred[dfPred$Impact == 1,]$XPos,
           dfPred[dfPred$Impact == 1,]$YPos,
           PostPredLink, zlim = range(PrePredLink, PostPredLink),
           main = "k = 40 link post")
# Response scale
PrePredResp <- predict(PRS_2DInt40, newdata = dfPred[dfPred$Impact == 0,], 
                       type = "response")
PostPredResp <- predict(PRS_2DInt40, newdata = dfPred[dfPred$Impact == 1,],
                        type = "response")

quilt.plot(dfPred[dfPred$Impact == 0,]$XPos,
           dfPred[dfPred$Impact == 0,]$YPos,
           PrePredResp, zlim = range(PrePredResp, PostPredResp),
           main = "k = 40 response pre")
quilt.plot(dfPred[dfPred$Impact == 1,]$XPos,
           dfPred[dfPred$Impact == 1,]$YPos,
           PostPredResp, zlim = range(PrePredResp, PostPredResp),
           main = "k = 40 response post")



