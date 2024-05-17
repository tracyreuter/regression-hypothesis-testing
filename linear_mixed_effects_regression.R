# predictive modeling
# testing hypotheses with linear mixed effects regression (LMER)
rm(list=ls(all=T))
setwd("~/Dropbox/Portfolio/predictive_modeling")
library(lme4)
library(lmerTest)
library(broom.mixed)
ds <- read.csv("VN.modeling.data.csv", header = T)
ds$subject <- as.factor(ds$subject)
ds$target <- as.factor(ds$target)
ds <- reshape2::dcast(ds, subject + group + age + PLS.AC.Raw + Mul.Ratio.IQ + target + timefromnoun ~ condition, value.var="targetlook", fun.aggregate = mean)
ds <- ds[is.na(ds$Informative)==F,]
ds <- ds[is.na(ds$Neutral)==F,]
# use a difference score DV to reduce model complexity
ds$targetlook <- ds$Informative-ds$Neutral
ds <- droplevels(ds)
ds$group <- as.factor(ds$group)
contrasts(ds$group) <- c(1,-1)
# confirm contrasts are coded correctly (ASD = 1, TD = -1)
contrasts(ds$group)
# center and scale model parameters
ds$time <- scale(ds$timefromnoun, center=T)
ds$PLS <- scale(ds$PLS.AC.Raw, center=T)
ds$age <- scale(ds$age, center=T)
ds$IQ <- scale(ds$Mul.Ratio.IQ, center=T)
# # # model selection process: start with maximal structure, then reduce random effects, then reduce fixed effects
model1 <- lmer(targetlook ~ group*time*PLS+age + (1+time|subject) + (1+group*time*PLS|target), data=ds)
# singular fit indicates over-fitting (random effects structure is too complex to be supported by the data)
# residuals should be normally-distributed and centered on 0 (i.e., over-predicting and under-predicting to the same extent)
# skew indicates over-fitting - hence the "is singular" warning:
hist(residuals(model1), xlab="Residuals")
# next reduce the complexity of the random effects structure (removing interactions):
model2 <- lmer(targetlook ~ group*time*PLS+age + (1+time|subject) + (1+group+time*PLS|target), data=ds) # singular fit
model3 <- lmer(targetlook ~ group*time*PLS+age + (1+time|subject) + (1+group*time+PLS|target), data=ds) # singular fit
model4 <- lmer(targetlook ~ group*time*PLS+age + (1+time|subject) + (1+group*PLS+time|target), data=ds) # singular fit
model5 <- lmer(targetlook ~ group*time*PLS+age + (1+time|subject) + (1+group+time+PLS|target), data=ds)
# summary(model5) # see full output
model.terms <- c('Intercept','Group','Time','PLS', 'Age',
                 'Group x Time','Group x PLS','Time x PLS',
                 'Group x Time x PLS')
# display table for fixed effects
library(kableExtra)
kable.model <- function(model) {
  mod <- tidy(model)
  mod <- subset(mod, mod$effect=="fixed")
  mod$effect <- NULL
  mod$group <- NULL
  mod$df <- NULL
  mod$estimate <- round(mod$estimate, 3)
  mod$std.error <- round(mod$std.error, 3)
  mod$statistic <- round(mod$statistic, 2)
  mod$p.value <- ifelse(mod$p.value<.001, "< 0.001", round(mod$p.value,3))
  mod$sig <- ifelse(mod$p.value < 0.001, "***",ifelse(mod$p.value < 0.01, "**",ifelse(mod$p.value < 0.05, "*",ifelse(mod$p.value < 0.10, "~",""))))
  mod$term <- c(model.terms)
  return(kable(mod, format='markdown'))
}
kable.model(model5)
