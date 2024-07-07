# Advanced Data Analysis Course Assignments

This repository contains a showcase of my coding assignments from the Advanced Data Analysis course, written in R. Each assignment demonstrates various data analysis techniques and statistical modeling approaches. Below is a brief summary of each practical assignment.

## Practical 1: Exploratory Data Analysis

### Purpose:
The objective of this practical is to perform exploratory data analysis on the Horns Rev dataset to understand the structure, relationships, and basic statistics of the data.

### Libraries Used:
- `tidyverse`: For data manipulation and visualisation.
- `ggplot2`: Main plotting library for creating visualisations.
- `dplyr`: Used for data manipulation and summarisation.
- `ggpubr`: Provides functions to enhance ggplot2 plots.
- `corrplot`: For correlation matrix visualisation.

### Steps and Analysis:

1. **Data Reading and Overview:**
   - Dataset `HornsRev.csv` is read into R.
   - Basic summary statistics (`summary`) and structure (`str`) of the dataset are examined.

2. **Univariate Analysis:**
   - Histograms, boxplots, and density plots are created to visualise the distribution of key variables (`XPos`, `YPos`, `Depth`, `Nhat`, `Area`) using `ggplot2` and `ggpubr` for enhancement.

3. **Bivariate Analysis:**
   - Scatter plots and correlation matrix (`corrplot`) are generated to explore relationships between variables.
   - Correlation coefficients are computed to quantify associations.

4. **Multivariate Analysis:**
   - Conditional plots (`ggplot2`) and stratified summaries (`dplyr`) are used to explore interactions and patterns across multiple variables.

---

## Practical 2: Linear Models and Diagnostic Checks

### Purpose:
This practical focuses on fitting linear regression models to predict `Nhat` using various predictors from the Horns Rev dataset. Diagnostic checks are performed to assess model assumptions and fit.

### Libraries Used:
- `tidyverse`: For data manipulation and visualisation.
- `lmtest`: Provides diagnostic tests for linear models.
- `car`: For diagnostic plots and model validation.
- `broom`: Converts model output to tidy format.
- `ggplot2`: Main plotting library for visualisations.

### Steps and Analysis:

1. **Model Fitting:**
   - Initial linear regression models (`lm`) are fitted to predict `Nhat` using predictors such as `XPos`, `YPos`, `Depth`, and `Area`.
   - Model summaries (`summary`) and diagnostic plots (`plot`, `qqPlot` from `car`) are examined.

2. **Model Assessment:**
   - Diagnostic tests (`resettest`, `bptest` from `lmtest`) are performed to check for model specification and heteroscedasticity.
   - Residual plots (`plot` from `car`) and goodness-of-fit statistics (`summary`) are reviewed.

3. **Model Comparison and Selection:**
   - Stepwise model selection (`step`) or information criteria (e.g., AIC, BIC) are used to compare models and select the best-fitting model.

4. **Visualisation of Results:**
   - Visualisations such as scatter plots with fitted lines (`ggplot2`) and residual plots (`plot` from `car`) are created to interpret model results and check assumptions.

---

## Practical 3: Generalised Linear Models (GLMs)

### Purpose:
This practical extends the linear modeling approach to Generalised Linear Models (GLMs) to account for non-normal error distributions and categorical response variables.

### Libraries Used:
- `tidyverse`: For data manipulation and visualisation.
- `glm`: Main function for fitting GLMs.
- `broom`: Converts model output to tidy format.
- `ggplot2`: Main plotting library for visualisations.

### Steps and Analysis:

1. **Data Preparation:**
   - Dataset `HornsRev.csv` is read, and necessary transformations are applied (e.g., converting categorical variables to factors).

2. **Model Fitting:**
   - GLMs (`glm`) are fitted with different families (e.g., Poisson, Gamma) to predict `Nhat` considering predictors such as `XPos`, `YPos`, `Depth`, and `Area`.
   - Model summaries (`summary`) and diagnostic plots (`plot`, `glm.diag`) are examined for each GLM.

3. **Model Comparison and Selection:**
   - Information criteria (e.g., AIC, BIC) are used to compare GLMs and select the most appropriate model.

4. **Visualisation of Results:**
   - Visualisations such as residual plots (`ggplot2`) and fitted vs. observed plots (`ggplot2`) are created to interpret model results and assess model fit.

---

## Practical 4: Spatial Data Analysis

### Purpose:
This practical introduces spatial data analysis techniques using the Horns Rev dataset. It includes spatial autocorrelation analysis, spatial interpolation, and visualisation of spatial patterns.

### Libraries Used:
- `tidyverse`: For data manipulation and visualisation.
- `spdep`: Provides functions for spatial dependence analysis.
- `gstat`: Used for spatial interpolation.
- `ggplot2`: Main plotting library for visualisations.

### Steps and Analysis:

1. **Spatial Autocorrelation Analysis:**
   - Moran's I statistic is computed to assess spatial autocorrelation among `Nhat`.
   - Spatial dependence is visualised using Moran scatter plots (`spdep`).

2. **Spatial Interpolation:**
   - Ordinary Kriging (`gstat`) is performed to interpolate `Nhat` values across the study area.
   - Interpolated surfaces are visualised using contour plots (`ggplot2`).

3. **Spatial Visualisation:**
   - Spatial distribution of `Nhat` and other variables (`XPos`, `YPos`, `Depth`) is visualised using spatial plots (`ggplot2`).
   - Additional overlays and annotations are added to enhance interpretability.

4. **Modeling Spatial Effects:**
   - Spatial regression models (e.g., SAR, CAR models from `spdep`) are fitted to explore spatially structured relationships.
   - Model diagnostics and spatial prediction are performed to understand spatial variations in `Nhat`.

---

## Practical 5: Generalised Additive Models (GAMs) in Spatial Analysis

### Purpose:
The objective of this practical is to apply Generalised Additive Models (GAMs) for spatial analysis using real-world data from Horns Rev. The practical explores fitting GAMs with different configurations, examining spatial patterns, and making predictions under different model specifications.

### Libraries Used:
- `tidyverse`: For data manipulation and visualisation.
- `mgcv`: Main library for fitting GAMs (`gam`) and handling splines (`s`, `bs`).
- `splines`: Specifically for B-splines.
- `MuMIn`: Used for model selection (`dredge`).
- `fields`: Provides `quilt.plot` for 2D spatial plots.
- `lawstat`: Utilised for statistical tests (`runs.test`).

### Steps and Analysis:

1. **Data Preparation and Initial GAM Fit:**
   - Dataset `HornsRev.csv` is read and `Impact` variable is converted to a factor.
   - Initial GAM (`PRS`) is fitted using `mgcv::gam` with spatial smooth terms (`s(XPos)`, `s(YPos)`, `s(Depth)`) and `Impact` as a factor.
   - Model diagnostics and residual plots are examined (`plot(PRS)`).

2. **Model Modification:**
   - The depth term (`s(Depth)`) is replaced with B-splines using `splines::bs` with 20 knots (`PRS_B`).
   - Model comparison is performed using Poisson GAM (`PRSpois`) to assess likelihood and model fit.

3. **Spatial Prediction:**
   - Prediction is conducted on a new dataset (`HornsRevPredictionData.csv`) for both link and response scales.
   - Predictions are visualised using `fields::quilt.plot`.

4. **Modeling Interaction:**
   - GAM with interaction terms (`PRS_Int`) between `XPos` and `Impact` is fitted to explore interaction effects.
   - Prediction and visualisation are repeated for this model.

5. **Statistical Testing:**
   - A runs test (`lawstat::runs.test`) is performed on the residuals of `PRS_Int` to assess spatial randomness.

---

## Practical 6: 2D GAMs and Fine Grid Predictions

### Purpose:
This practical builds on Practical 5 by extending the analysis to 2D spatial models using GAMs. It focuses on fitting 2D splines, making fine grid predictions, and visualising the spatial patterns under different model specifications.

### Libraries Used:
- `mgcv`: Continued use for fitting 2D GAMs (`gam`).
- `fields`: For `quilt.plot` to visualise 2D spatial predictions.

### Steps and Analysis:

1. **Data Preparation:**
   - The same dataset `HornsRev.csv` is used, with `Impact` again set as a factor.

2. **Initial 2D GAM Fit:**
   - A 2D GAM (`PRS_2D`) is fitted using `mgcv::gam` with spatial smooth terms (`s(XPos, YPos)`) and `Depth`, along with `Impact`.
   - Model summary and partial plots (`plot(PRS_2D)`) are examined.

3. **Spatial Prediction with Fine Grid:**
   - Predictions are made on a finer grid (`HornsRevPredictionData.csv`) for both link and response scales.
   - Visualisations (`quilt.plot`) show spatial patterns for different `Impact` levels.

4. **Modeling Interaction in 2D:**
   - A 2D GAM with interaction terms (`PRS_2DInt`) between `XPos`, `YPos`, and `Impact` is fitted and analysed.
   - Predictions and visualisations are repeated for this interactive model.

5. **Model Sensitivity Analysis:**
   - Additional models (`PRS_2DInt15` and `PRS_2DInt40`) are fitted with different degrees of freedom (`k`) for the spline terms.
   - Spatial plots illustrate the impact of changing `k` on predictions.
