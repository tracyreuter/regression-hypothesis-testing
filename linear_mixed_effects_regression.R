# Predictive modeling
# Test hypotheses with linear mixed effects regression (LMER)

# Clear environment and load packages
rm(list = ls(all = TRUE))
library("lme4")
library("lmerTest")
library("broom.mixed")
library("kableExtra")

# Wrangle data for modeling
ds <- read.csv("VN.modeling.data.csv", header = T)
ds$subject <- as.factor(ds$subject)
ds$target <- as.factor(ds$target)
ds <- reshape2::dcast(ds,
  subject + group + age + PLS.AC.Raw + Mul.Ratio.IQ + target + timefromnoun ~ condition,
  value.var = "targetlook", fun.aggregate = mean
)
ds <- ds[is.na(ds$Informative) == FALSE, ]
ds <- ds[is.na(ds$Neutral) == FALSE, ]
# Use a difference score DV to reduce model complexity
ds$targetlook <- ds$Informative - ds$Neutral
ds <- droplevels(ds)
ds$group <- as.factor(ds$group)
# Confirm contrasts are coded correctly (ASD = 1, TD = -1)
contrasts(ds$group) <- c(1, -1)
contrasts(ds$group)
# Center and scale model parameters
ds$time <- scale(ds$timefromnoun, center = TRUE)
ds$PLS <- scale(ds$PLS.AC.Raw, center = TRUE)
ds$age <- scale(ds$age, center = TRUE)
ds$IQ <- scale(ds$Mul.Ratio.IQ, center = TRUE)

# Model Selection
# Start with maximal structure, then reduce random and fixed effects
model1 <- lmer(targetlook ~ group * time * PLS + age + (1 + time | subject) + (1 + group * time * PLS | target), data = ds)
# "Singular fit" indicates over-fitting (random effects are too complex)
# Model residuals should be normally-distributed and centered on 0
# Normal residuals show the model under- and over-predict equally
# Skew indicates over-fitting - hence the "is singular" warning:
hist(residuals(model1), xlab = "Residuals")
# Next reduce the complexity of the random effects structure:
model2 <- lmer(targetlook ~ group * time * PLS + age + (1 + time | subject) + (1 + group + time * PLS | target), data = ds)
model3 <- lmer(targetlook ~ group * time * PLS + age + (1 + time | subject) + (1 + group * time + PLS | target), data = ds)
model4 <- lmer(targetlook ~ group * time * PLS + age + (1 + time | subject) + (1 + group * PLS + time | target), data = ds)
model5 <- lmer(targetlook ~ group * time * PLS + age + (1 + time | subject) + (1 + group + time + PLS | target), data = ds)
# If the model still fails to converge, then reduce fixed effects

# List model terms for legible table output
model_terms <- c(
  "Intercept", "Group", "Time", "PLS", "Age",
  "Group x Time", "Group x PLS", "Time x PLS",
  "Group x Time x PLS"
)

# Define a function to display a table for fixed effects
kable_model <- function(model) {
  mod <- tidy(model)
  mod <- subset(mod, mod$effect == "fixed")
  mod$effect <- NULL
  mod$group <- NULL
  mod$df <- NULL
  mod$estimate <- round(mod$estimate, 3)
  mod$std.error <- round(mod$std.error, 3)
  mod$statistic <- round(mod$statistic, 2)
  mod$p.value <- ifelse(mod$p.value < .001, "< 0.001", round(mod$p.value, 3))
  mod$sig <- ifelse(mod$p.value < 0.001, "***",
    ifelse(mod$p.value < 0.01, "**",
      ifelse(mod$p.value < 0.05, "*",
        ifelse(mod$p.value < 0.10, "~", "")
      )
    )
  )
  mod$term <- c(model_terms)
  kable(mod, format = "markdown")
}

# Call the function
kable_model(model5)
