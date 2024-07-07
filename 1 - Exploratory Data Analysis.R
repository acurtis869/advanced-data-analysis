library(tidyverse)
# Task 1
df <- read.csv("HornsRev.csv")
head(df)
# Task 2
df$Impact <- as.factor(df$Impact)
# Create new variable: density - abundance per unit area
df$Density <- df$Nhat / df$Area
ggplot(df) + geom_histogram(aes(x = Density)) + facet_wrap(~Impact)
ggplot(df) + geom_boxplot(aes(x = Density, y = Impact))
# Task 3 - produce 98% CIs for density, pre- and post-impact
pre <- filter(df, Impact == 0)
mu_hat_pre <- mean(pre$Density)
s_pre <- sd(pre$Density)
n_pre <- nrow(pre)
post <- filter(df, Impact == 1)
mu_hat_post <- mean(post$Density)
s_post <- sd(post$Density)
n_post <- nrow(post)
# a - assume response is normally distributed
se_pre <- s_pre / sqrt(n_pre)
CI1_pre <- c(mu_hat_pre - qt(0.99, n_pre - 1) * se_pre,
             mu_hat_pre + qt(0.99, n_pre - 1) * se_pre)
se_post <- s_post / sqrt(n_post)
CI1_post <- c(mu_hat_post - qt(0.99, n_post - 1) * se_post,
              mu_hat_post + qt(0.99, n_post - 1) * se_post)
df1 <- data.frame(Impact = c("pre", "post"),
                  mean = c(mu_hat_pre, mu_hat_post),
                  lower = c(CI1_pre[1], CI1_post[1]),
                  upper = c(CI1_pre[2], CI1_post[2]))
df1
ggplot(df1, aes(x = Impact, y = mean, ymin = lower, ymax = upper)) + 
  geom_pointrange()
# b - use normal approx for large samples, assuming Poisson response
se_pre <- sqrt(mu_hat_pre / n_pre)
CI2_pre <- c(mu_hat_pre - qnorm(0.99) * se_pre,
             mu_hat_pre + qnorm(0.99) * se_pre)
se_post <- sqrt(mu_hat_post / n_post)
CI2_post <- c(mu_hat_post - qnorm(0.99) * se_post,
             mu_hat_post + qnorm(0.99) * se_post)
df2 <- data.frame(Impact = c("pre", "post"),
                  mean = c(mu_hat_pre, mu_hat_post),
                  lower = c(CI2_pre[1], CI2_post[1]),
                  upper = c(CI2_pre[2], CI2_post[2]))
df2
ggplot(df2, aes(x = Impact, y = mean, ymin = lower, ymax = upper)) + 
  geom_pointrange()
# c- normal approx, without assuming Poisson and invoking CLT 
se_pre <- s_pre / sqrt(n_pre)
CI3_pre <- c(mu_hat_pre - qnorm(0.99) * se_pre,
             mu_hat_pre + qnorm(0.99) * se_pre)
se_post <- s_post / sqrt(n_post)
CI3_post <- c(mu_hat_post - qnorm(0.99) * se_post,
             mu_hat_post + qnorm(0.99) * se_post)
df3 <- data.frame(Impact = c("pre", "post"),
                  mean = c(mu_hat_pre, mu_hat_post),
                  lower = c(CI3_pre[1], CI3_post[1]),
                  upper = c(CI3_pre[2], CI3_post[2]))
df3
ggplot(df3, aes(x = Impact, y = mean, ymin = lower, ymax = upper)) + 
  geom_pointrange()
# d - bootstrapping
set.seed(145)
NBOOT <- 1000
alpha <- 0.02
# pre-impact
muHat_pre <- sapply(seq(NBOOT),
                function (x) mean(sample(x = pre$Density,
                                         size = nrow(pre),
                                         replace = TRUE)))
CI4_pre <- quantile(muHat_pre, c(alpha / 2, 1 - (alpha / 2)))
# post-impact
muHat_post <- sapply(seq(NBOOT),
                     function (x) mean(sample(x = post$Density,
                                              size = nrow(post),
                                              replace = TRUE)))
CI4_post <- quantile(muHat_post, c(alpha / 2, 1 - (alpha / 2)))
df4 <- data.frame(Impact = c("pre", "post"),
                  mean = c(mu_hat_pre, mu_hat_post),
                  lower = c(CI4_pre[1], CI4_post[1]),
                  upper = c(CI4_pre[2], CI4_post[2]))
df4
ggplot(df4, aes(x = Impact, y = mean, ymin = lower, ymax = upper)) + 
  geom_pointrange()

# Task 4
ggplot(df) + geom_point(aes(x = XPos, y = YPos, size = Density))

ggplot(df) + geom_point(aes(x = XPos, y = YPos, size = Density)) +
  facet_wrap(~Impact)


## Model Fitting

library(car)

# Impact only
impact_pois <- glm(Nhat ~ Impact, data = df, offset = Area, family = poisson)
summary(impact_pois)
Anova(impact_pois)
impact_quasi <- glm(Nhat ~ Impact, data = df, offset = Area, 
                    family = quasipoisson)
summary(impact_quasi)
Anova(impact_quasi)

# Impact, Depth, XPos, YPos
all_pois <- glm(Nhat ~ Impact + Depth + XPos + YPos,
                   data = df, offset = Area, family = poisson)
summary(all_pois)
Anova(all_pois)
all_quasi <- glm(Nhat ~ Impact + Depth + XPos + YPos,
                 data = df, offset = Area, family = quasipoisson)
summary(all_quasi)
Anova(all_quasi)

# Impact, Depth, XPos, YPos, interactions
int_pois <- glm(Nhat ~ Impact + Depth + XPos + YPos + Impact:XPos + Impact:YPos,
                data = df, offset = Area, family = poisson)
summary(int_pois)
Anova(int_pois)
int_quasi <- glm(Nhat ~ Impact + Depth + XPos + YPos + Impact:XPos + Impact:YPos,
                 data = df, offset = Area, family = quasipoisson)
summary(int_quasi)
Anova(int_quasi)