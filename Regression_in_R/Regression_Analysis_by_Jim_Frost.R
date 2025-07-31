# Regression Analysis by Jim Frost (2019) 
# book: https://a.co/d/5hPjofi
# data available at https://statisticsbyjim.com/regression_book
# https://statisticsbyjim.com/wp-content/uploads/2020/10/RegressionAnalysisDatasets.zip


#### Commonly used libraries and options ####

library(tidyverse)
theme_set(theme_bw())



#### Chapter 1: Correlation and an Introduction to Regression ####

# Load data
HeightWeight <- read_csv("~/Downloads/RegressionAnalysisDatasets/HeightWeight.csv") %>% 
    janitor::clean_names()

# p.6 plot
ggplot(HeightWeight, aes(height_m, weight_kg)) +
    geom_point() +
    labs(x = "Height (m)", y = "Weight (kg)", title = "Scatterplot of Weight by Height")

# Get correlation with hypothesis test
cor.test(~ weight_kg + height_m, data = HeightWeight)


# explore different patterns of correlations
# using the HeightWeight data summary stats

mean_ht <- mean(HeightWeight$height_m)
mean_wt <- mean(HeightWeight$weight_kg)
sd_ht <- sd(HeightWeight$height_m)
sd_wt <- sd(HeightWeight$weight_kg)

corcoef <- 0.6 # specify the correlation of interest here

covariance <- corcoef * sd_wt * sd_ht
Sigma <- matrix(c(sd_ht^2, covariance, covariance, sd_wt^2), nrow = 2)
xy <- MASS::mvrnorm(n = 88, mu = c(mean_ht, mean_wt), Sigma = Sigma)
colnames(xy) <- c("mean_ht", "mean_wt")

plot(xy)
cor.test(xy[,1], xy[,2])



#### Chapter 2: Regression Basics and How it Works ####

# Basic regression model of weight as described by height
model2.1 <- lm(weight_kg ~ height_m, data = HeightWeight)

# SSE, sum of squared errors, for HeightWeight data
# aka RSS, Residual Sum of Squares for most statisticians
sum(residuals(model2.1)^2)
# equivalently:
deviance(model2.1)

# RSS, regression sum of squares / aka Explained Sum of Squares (ESS)
sum((model2.1$fitted.values - mean(model2.1$model$weight_kg))^2)

# both together
anova(model2.1)

# R^2 from RSS / TSS
anova(model2.1)[1,2] /sum(anova(model2.1)[,2])

# p.35 plot
ggplot(HeightWeight, aes(height_m, weight_kg)) +
    geom_point() +
    geom_smooth(method = lm) +
    labs(x = "Height (m)", y = "Weight (kg)", title = "Scatterplot of Weight by Height")

# Regression summary
summary(model2.1)

# "Publication" style regression table
gtsummary::tbl_regression(model2.1, intercept = T,
               estimate_fun = label_style_number(digits = 1))



#### Chapter 3: Interpreting Main Effects and Significance ####

# Basic regression model of weight as described by height
model3.1 <- lm(weight_kg ~ height_m, data = HeightWeight)

# p.45 plot
ggplot(HeightWeight, aes(height_m, weight_kg)) +
    geom_point() +
    geom_smooth(method = lm) +
    labs(x = "Height (m)", y = "Weight (kg)", title = "Scatterplot of Weight by Height")

# Regression summary
summary(model3.1)

# Confidence intervals
confint(model3.1)

# Scaling
scale(HeightWeight$height_m)[,1]

# Centering
scale(HeightWeight$height_m, center = TRUE, scale = FALSE)[,1]


# Load Categorical Example data
CatEx <- read_csv("~/Downloads/RegressionAnalysisDatasets/Categorical_Example.csv") %>% 
    janitor::clean_names() %>% 
    mutate(major = factor(major, levels = c("Statistics", 
                                            "Political Science",
                                            "Psychology")))

# plot p.55
ggplot(CatEx, aes(major, income)) +
    geom_boxplot()

contrasts(CatEx$major)

# Model with a categorical variable
model3.2 <- lm(income ~ experience + major, data = CatEx)

anova(model3.2)

summary(model3.2)

predict(model3.2, newdata = data.frame(major = c("Statistics",
                                                 "Political Science",
                                                 "Psychology"),
                                       experience = 0))

ggeffects::ggpredict(model3.2, terms = "major", condition = c(experience = 0))

equatiomatic::extract_eq(model3.2, coef_digits = 0, use_coefs = T)


# plot p.68
ggplot(HeightWeight, aes(height_m, weight_kg)) +
    geom_point() +
    geom_smooth(method = lm) +
    labs(x = "Height (m)", y = "Weight (kg)", title = "Scatterplot of Weight by Height") +
    coord_cartesian(xlim = c(0,NA), ylim = c(0,NA))

# 0 as a predictor removes the intercept
model3.3 <- lm(weight_kg ~ 0 + height_m, data = HeightWeight)

summary(model3.3)

# plot p.72
ggplot(HeightWeight, aes(height_m, weight_kg)) +
    geom_point() +
    geom_smooth(method = lm) +
    labs(x = "Height (m)", y = "Weight (kg)", title = "Scatterplot of Weight by Height") +
    coord_cartesian(xlim = c(0,NA), ylim = c(0,NA)) +
    geom_abline(slope = model3.3$coefficients[1], intercept = 0) 

model3.4 <- lm(weight_kg ~ scale(height_m, center = T, scale = F), data = HeightWeight)

summary(model3.4)

# Plot on p.74
ggplot(HeightWeight, aes(scale(height_m, center = T, scale = F), weight_kg)) +
    geom_point() +
    geom_smooth(method = lm) +
    labs(x = "Height (m, centered)", y = "Weight (kg)", title = "Scatterplot of Weight by Height") 



#### Chapter 4: Fitting Curvature ####

# Original Minitab data
EnConOrig = data.frame(x = c(11.15,13.3,14.2,15.7,18.9,19.4,21.4,21.7,23.5,24.3,25.3,26.4,26.7,27.9,29.1),
                       y = c(21.6,12.5,8.91,4.0,1.8,1.0,1.0,0.8,2.35,4.4,3.8,7.4,4.3,19.05,36.2))

# Minitab data modified and trimmed by this book
EnCon = data.frame(x = c(11.15,15.7,18.9,19.4,21.4,21.7,25.3,26.4,26.7,29.1),
                   y = c(21.6,4.0,1.8,1.0,1.0,0.8,3.8,7.4,4.3,36.2)) 

# plot p.79
ggplot(EnCon, aes(x, y)) +
    geom_point() +
    labs(x = "Machine Setting", y = "Energy Consumption", title = "Energy Consumption by Machine Setting") +
    geom_smooth(method = "lm", formula = y ~ poly(x, 2)) +
    scale_y_log10(limits = c(.7, 110)) +
    scale_x_continuous(breaks = seq(10, 30, by = 5), limits = c(10, 30))

model4.1 <- lm(log10(y) ~ x + I(x^2), data = EnCon)

summary(model4.1)


# Load data
Hardness <- read_csv("~/Downloads/RegressionAnalysisDatasets/Hardness.csv") %>% 
    janitor::clean_names()

model4.2 <- lm(hardness ~ temp + pressure + I(pressure^2), data = Hardness)

summary(model4.2)

# plot p.81
p1 <- ggpredict(model4.2, terms = c("pressure")) %>% plot()
p2 <- ggpredict(model4.2, terms = c("temp")) %>% plot()

library(patchwork) # to put plots together using +/, etc

# Main effects plot for hardness
p1 + p2 + plot_annotation(title = "Main Effects Plot for Hardness") & ylim(0, 100)


# Load data
Curved <- read_csv("~/Downloads/RegressionAnalysisDatasets/CurveFittingExample.csv") %>% 
    janitor::clean_names()

# plot p.82
ggplot(Curved, aes(input, output)) +
    geom_point() +
    geom_smooth(method = lm)

model4.3 <- lm(output ~ input, data = Curved)

summary(model4.3)


# Load data
BMI <- read_csv("~/Downloads/RegressionAnalysisDatasets/Predict_BMI.csv") %>% 
    janitor::clean_names()

# plot p.86
ggplot(BMI, aes(bmi, percent_fat)) +
    geom_point() +
    geom_smooth(method = lm, formula = y ~ poly(x, 2))

model4.4 <- lm(percent_fat ~ bmi + I(bmi^2), data = BMI)

summary(model4.4)


# plot p.88 (same plot as p.82)
ggplot(Curved, aes(input, output)) +
    geom_point() +
    geom_smooth(method = lm)

summary(model4.3)


# plot p.90
ggplot(Curved, aes(input, output)) +
    geom_point() +
    geom_smooth(method = lm, formula = y ~ poly(x, 2))

model4.5 <- lm(output ~ input + I(input^2), data = Curved)

summary(model4.5)


# plot p.92 top
ggplot(Curved, aes(inv_input, output)) +
    geom_point() +
    geom_smooth(method = lm)

model4.6 <- lm(output ~ inv_input, data = Curved)

summary(model4.6)


# plot p.92 bottom
ggplot(Curved, aes(inv_input, output)) +
    geom_point() +
    geom_smooth(method = lm, formula = y ~ poly(x, 2))

model4.7 <- lm(output ~ inv_input + I(inv_input^2), data = Curved)

summary(model4.7)

Curved_linear <- broom::augment(model4.6, Curved, interval = "confidence")
Curved_quad <- broom::augment(model4.7, Curved, interval = "confidence")

# plot p.93
ggplot(Curved, aes(input, output)) +
    geom_point(shape = 17) +
    geom_point(data = Curved_linear, aes(y = .fitted), color = "red", shape = 0) +
    geom_point(data = Curved_quad, aes(y = .fitted), color = "blue", shape = 1)

# plot p.94
ggplot(Curved, aes(input, output)) +
    geom_point() +
    geom_smooth(method = lm, formula = y ~ log10(x))

model4.8 <- lm(output ~ I(log10(input)), data = Curved)

summary(model4.8)


# plot p.97 
st <- list(a = 20, b = -20, c = -0.32)
model4.9a <- nls(output ~ a + -b * exp(c*input), Curved, st = st)

summary(model4.9a)
sigma(model4.9a)

nls_prediction <- data.frame(input = seq(min(Curved$input) - 0.05, max(Curved$input) + 0.05, length.out = 50))
nls_prediction$output <- predict(model4.9a, newdata = nls_prediction)

ggplot(Curved, aes(input, output)) +
    geom_point() +
    geom_line(data = nls_prediction, aes(y = output))

# plot p.97 (not the same, but perhaps easier/better, using a GAM)
ggplot(Curved, aes(input, output)) +
    geom_point() +
    geom_smooth(method = "gam", formula = y ~ s(x)) + # show GAM
    geom_line(data = nls_prediction, aes(y = output), color = "red") # show nls model

model4.9b <- mgcv::gam(output ~ s(input), data = Curved)

summary(model4.9b)
sigma(model4.9b)

gratia::conditional_values(model4.9b, condition = "input") %>% 
    gratia::draw() +
    geom_point(data = Curved, aes(input, output))


# Load data
Electron <- read_csv("~/Downloads/RegressionAnalysisDatasets/ElectronMobility.csv") %>% 
    janitor::clean_names()

# plot p.86
ggplot(Electron, aes(density_ln, mobility)) +
    geom_point() +
    geom_smooth(method = lm, formula = y ~ poly(x, 3))

model4.10 <- lm(mobility ~ density_ln + I(density_ln^2) + I(density_ln^3), data = Electron)

summary(model4.10)

plot(model4.10, which = 1)

performance::check_model(model4.10, check = "linearity")

# formula p.103
model4.11 <- (1288.14 + 1491.08*Electron$density_ln + 583.238*(Electron$density_ln^2) + 75.4167*(Electron$density_ln^3)) / (1 + 0.966295*Electron$density_ln + 0.397973*(Electron$density_ln^2) + 0.0497273*(Electron$density_ln^3))

Electron$fitted <- model4.11

# plot p.102
ggplot(Electron, aes(density_ln, mobility)) +
    geom_point() +
    geom_line(aes(y = fitted))

# plot p.103
ggplot(Electron, aes(fitted, mobility - fitted)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_point() 

sqrt(sum((Electron$mobility - Electron$fitted)^2) / (nrow(Electron) - 2)) 
# difference from book is probably rounding error from this code's model, above



#### Chapter 5: Interaction Effects ####

# Load data
IntCat <- read_csv("~/Downloads/RegressionAnalysisDatasets/Interactions_Categorical.csv") %>% 
    janitor::clean_names() %>% 
    mutate_if(is.character, as.factor) 

model5.1 <- lm(enjoyment ~ food * condiment, data = IntCat)

anova(model5.1)

summary(model5.1) # not sure why the book is so different--typo? Minitab vs R?

# plot p.108
interaction.plot(IntCat$food, IntCat$condiment, IntCat$enjoyment, type = "b")
ggpredict(model5.1, c("food", "condiment")) %>% plot(connect_lines = T)

# plot p.109
model5.1b <- lm(enjoyment ~ food + condiment, data = IntCat)
p1 <- ggeffect(model5.1b, terms = c("food")) %>% plot(connect_lines = T)
p2 <- ggeffect(model5.1b, terms = c("condiment")) %>% plot(connect_lines = T)
p1 + p2 + plot_annotation(title = "Main Effects Plots for Enjoyment") & ylim(70, 85)

# Load data
IntCon <- read_csv("~/Downloads/RegressionAnalysisDatasets/Interactions_Continuous.csv") %>% 
    janitor::clean_names() 

model5.2 <- lm(strength ~ temperature + pressure + time + temperature:pressure, data = IntCon)

anova(model5.2) # the book is using a different type of ANOVA than R's default
car::Anova(model5.2, type = 3) # Type III ANOVA

summary(model5.2) 

# plot p.111
ggpredict(model5.2, c("temperature", "pressure [63.68, 81.10]")) %>% plot()



#### Chapter 6: Goodness of Fit ####

# plot p.124
ggplot(Electron, aes(density_ln, mobility)) +
    geom_point() +
    geom_smooth(method = lm, formula = y ~ poly(x, 3))

model4.10 <- lm(mobility ~ density_ln + I(density_ln^2) + I(density_ln^3), data = Electron)

summary(model4.10)

# plot p.125
performance::check_model(model4.10, check = "linearity")

# example of problems with R^2 (text on p.128)
# original code source unknown
set.seed(123)
y <- rnorm(10)
x <- sapply(rep(10,8), rnorm)
noise <- lm(y ~ x)
summary(noise)$r.squared
summary(noise)$adj.r.squared

# predicted R^2
# Load data
Prez <- read_csv("~/Downloads/RegressionAnalysisDatasets/PresidentRanking.csv") %>% 
    janitor::clean_names() 

# plot p.131
ggplot(Prez, aes(approval_high, historians_rank)) +
    geom_point() +
    geom_smooth(method = lm)

model6.1 <- lm(historians_rank ~ approval_high, data = Prez)

summary(model6.1)

# plot p.132
ggplot(Prez, aes(approval_high, historians_rank)) +
    geom_point() +
    geom_smooth(method = lm, formula = y ~ poly(x, 3))

model6.2 <- lm(historians_rank ~ approval_high + I(approval_high^2) + I(approval_high^3), data = Prez)

summary(model6.2)

# guess at predicted R^2 formulas from https://rpubs.com/RatherBit/102428
pr <- residuals(model6.2)/(1-lm.influence(model6.2)$hat)
press <- sum(pr^2)
tss <- sum(anova(model6.2)$'Sum Sq')
pred.r.squared <- 1-(press/tss)
ifelse(pred.r.squared < 0, 0, pred.r.squared)

# Shortcut for predicted R^2; leave-one-out cross-validation
caret::train(historians_rank ~ approval_high + I(approval_high^2) + I(approval_high^3), 
             data = Prez,
             method = "lm", 
             trControl = caret::trainControl(method = "LOOCV"))$results$Rsquared


# book: "Standard error of the regression"
# R summary output: "Residual standard error"
sigma(model4.4)

BMI <- broom::augment(model4.4, BMI, interval = "prediction")
BMI <- BMI %>% 
    mutate(s_lower = .fitted - sigma(model4.4) * 2,
           s_upper = .fitted + sigma(model4.4) * 2)
   
# plot p.136 (originally on p.86)
ggplot(BMI, aes(bmi, percent_fat)) +
    geom_point() +
    # not in book, prediction interval
    geom_line(aes(y = .lower), linetype = "dotted", color = "red") +
    geom_line(aes(y = .upper), linetype = "dotted", color = "red") +
    # in book, s * 2
    geom_line(aes(y = s_lower), linetype = "dashed", color = "blue") +
    geom_line(aes(y = s_upper), linetype = "dashed", color = "blue") +
    geom_smooth(method = lm, formula = y ~ poly(x, 2))



#### Chapter 7: Specify Your Model ####

# no "bear" data so no code here

# WARNING: do not follow book's advice based on stepwise regression; 
# stepwise will always overfit, at best. See:
# Smith, G. Step away from stepwise. J Big Data 5, 32 (2018). 
# https://doi.org/10.1186/s40537-018-0143-6

# stepwise and best subsets regression examples not done because they are bad ideas



#### Chapter 8: Problematic Methods of Specifying Your Model ####

# No stepwise example, even though this example shows why it's a bad idea!



#### Chapter 9: Checking Assumptions and Fixing Problems ####

# plot(model) gives you the residual plots in R
# ?plot.lm for hints on interpreting the residual plots in R
# see https://library.virginia.edu/data/articles/diagnostic-plots for overview
# general tip: use performance::check_model(model) for set of assumption checks

plot(model4.4, which = 1)
performance::check_residuals(model4.4)
performance::check_model(model4.4, check = "linearity")

model4.1_9a <- lm(y ~ x, data = EnConOrig)
plot(model4.1_9a, which = 1)
performance::check_residuals(model4.1_9a)
performance::check_model(model4.1_9a, check = "linearity")

model4.1_9b <- lm(log10(y) ~ x, data = EnCon)
plot(model4.1_9b, which = 1) # fitted values are on log scale

EnCon <- broom::augment(model4.1_9b, EnCon)
ggplot(EnCon, aes(x, .resid)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_point() +
    labs(x = "Machine Setting", y = "Residuals")

# data source (sales residuals by time order) for plot on p.200 is unknown
plot(index(EnCon), EnCon$.resid)

# Load data
Accidents <- read_csv("~/Downloads/RegressionAnalysisDatasets/Heteroscedasticity.csv") %>% 
    janitor::clean_names() 

model9.1 <- lm(accidents ~ population, data = Accidents)

# plot p.202
plot(model9.1, which = 1)
# Other ways to check for heteroscedasticity
plot(model9.1, which = 3)
performance::check_heteroscedasticity(model9.1)
performance::check_model(model9.1, check = "homogeneity")

# plot p.204
qqplot(residuals(model4.4), BMI$percent_fat)

plot(model4.4, which = 2)
performance::check_normality(model4.4)
performance::check_model(model4.4, check = "normality")

model9.2 <- lm(accident_rate ~ population, data = Accidents)

# plot p.211
plot(model9.2, which = 1)

model9.3 <- lm(accidents ~ population, weights = weight, data = Accidents)

# plot p.213
plot(model9.3, which = 3)
performance::check_model(model9.3, check = "homogeneity")

# Box-Cox transformation
MASS::boxcox(accidents ~ population, data = Accidents)
bc = MASS::boxcox(accidents ~ population, data = Accidents, plotit = F)
bc$x[which.max(bc$y)]

model9.4 <- lm(I(accidents^0.8) ~ population, data = Accidents)

# plot p.214; typo in book--those are not standardized residuals
plot(model9.4, which = 1)

# standardized residuals
Accidents <- broom::augment(model9.4, Accidents) %>% 
    mutate(standardized_resid = sqrt(abs(.resid)))
plot(model9.4, which = 3)
performance::check_heteroscedasticity(model9.4)
performance::check_model(model9.4, check = "homogeneity")

# Load data
MultCo <- read_csv("~/Downloads/RegressionAnalysisDatasets/MulticollinearityExample.csv") %>% 
    janitor::clean_names() 

model9.5 <- lm(femoral_neck ~ percent_fat + weight_kg + activity + percent_fat * weight_kg, data = MultCo)

car::Anova(model9.5, type = 3)
summary(model9.5)

performance::check_collinearity(model9.5)
performance::check_model(model9.5, check = "vif")

# could use scale(variable, center = T, scale = F)
model9.6 <- lm(femoral_neck ~ percent_fat_s + weight_s + activity_s + percent_fat_s * weight_s, data = MultCo)

car::Anova(model9.6, type = 3)
summary(model9.6)

performance::check_collinearity(model9.6)
performance::check_model(model9.6, check = "vif")

# Influential points
# data derived using an LLM so will not be exactly the same

InfEx1 = read_csv(
"Input,Output
10.2,10
10.5,14
11.2,16.5
11.5,24
12.0,21.5
12.5,23
13.0,28
13.2,30.5
13.8,36.5
14.0,33
14.2,51
15.0,37
15.5,41.5
16.2,45
16.5,44
18.0,56
18.5,58
18.6,61
19.2,58.5"
)

model9.7 <- lm(Output ~ Input, data = InfEx1)

summary(model9.7)

# plot p.227
ggplot(InfEx1, aes(Input, Output)) +
    geom_point() +
    geom_smooth(method = lm)

# plot p.228
plot(model9.7, which = 1)

InfEx1 <- broom::augment(model9.7, InfEx1)
which.max(InfEx1$.resid)
InfEx1$.resid[11]
which.max(InfEx1$.std.resid)
InfEx1$.std.resid[11]
plot(model9.7, which = 5)
car::outlierTest(model9.7)

# data derived using an LLM so will not be exactly the same
InfEx2 = read_csv(
"Input,Output
10.4,10
10.8,14
11.5,16
12.2,22
12.5,23
13.2,30
13.5,28
14.0,36
15.2,38
15.8,42
16.0,45
16.5,42
18.0,56
18.2,60
18.5,58.5
19.2,58
24.2,82")

model9.8 <- lm(Output ~ Input, data = InfEx2)

summary(model9.8)

# plot p.230
ggplot(InfEx2, aes(Input, Output)) +
    geom_point() +
    geom_smooth(method = lm)

# plot p.231
ggplot(InfEx2, aes(Input)) +
    geom_histogram(binwidth = 2)

plot(model9.8, which = 1)

InfEx2 <- broom::augment(model9.8, InfEx2)
which.max(InfEx2$.resid)
InfEx2$.resid[14]
which.min(InfEx2$.resid)
InfEx2$.resid[17]
which.max(InfEx2$.std.resid)
InfEx2$.std.resid[14]
which.min(InfEx2$.std.resid)
InfEx2$.std.resid[17]

plot(model9.8, which = 4)
plot(model9.8, which = 5)

ls.diag(model9.8)$dfits
which.min(ls.diag(model9.8)$dfits)
ls.diag(model9.8)$dfits[17]
plot(ls.diag(model9.8)$dfits)

car::outlierTest(model9.8)

model9.9 <- lm(Output ~ Input, data = InfEx2[-17,])

summary(model9.9)

# plot p.230
ggplot(InfEx2[-17,], aes(Input, Output)) +
    geom_point() +
    geom_smooth(method = lm)

summary(model9.8)
summary(model9.9)

# Johnson transformation: 
# see the {bestNormalize} package



#### Chapter 10: Using Regression to Make Predictions ####

# Load data
Predict_BMI <- read_csv("~/Downloads/RegressionAnalysisDatasets/Predict_BMI.csv") %>% 
    janitor::clean_names()

# plot p.252
ggplot(Predict_BMI, aes(bmi, percent_fat)) +
    geom_point() +
    geom_smooth(method = lm, formula = y ~ poly(x, 2))

model10.1 <- lm(percent_fat ~ bmi + I(bmi^2), data = Predict_BMI)

summary(model10.1)

# better approach for plot p.253
performance::check_model(model10.1)

anova(model10.1)

broom::glance(model10.1)

# Minitab's predicted R^2
pr <- residuals(model10.1)/(1-lm.influence(model10.1)$hat)
press <- sum(pr^2)
tss <- sum(anova(model10.1)$'Sum Sq')
pred.r.squared <- 1-(press/tss)
ifelse(pred.r.squared < 0, 0, pred.r.squared)

# Shortcut for predicted R^2; leave-one-out cross-validation
caret::train(percent_fat ~ bmi + I(bmi^2), data = Predict_BMI,
             method = "lm", 
             trControl = caret::trainControl(method = "LOOCV"))$results$Rsquared

equatiomatic::extract_eq(model10.1, coef_digits = 2, use_coefs = T)

predict(model10.1, newdata = data.frame(bmi = 18),
        se.fit = T, interval = "confidence")
ggeffects::ggpredict(model10.1, terms = "bmi [18]")
ggeffects::ggpredict(model10.1, terms = "bmi")
ggeffects::ggpredict(model10.1, terms = "bmi") %>% plot()

predict(model10.1, newdata = data.frame(bmi = 18),
        interval = "prediction")
ggeffects::ggpredict(model10.1, terms = "bmi [18]", interval = "prediction")
ggeffects::ggpredict(model10.1, terms = "bmi", interval = "prediction")
ggeffects::ggpredict(model10.1, terms = "bmi", interval = "prediction") %>% plot()

# math puzzle p.260+
# see https://doi.org/10.1016/j.ijforecast.2012.02.002
# y = 0.32 + 1.001 * x
# Var(Y) = 40.78^2
# R^2 = 0.50
# standard deviation of the estimated residuals (SER): 
# in R: sigma(model)
# SER = sqrt((40.78^2) * (1-0.50)) - 0.32)
x <- (qnorm(0.95) * (sqrt((40.78^2) * (1-0.50))) - 0.32) / 1.001

SRP <- read_csv("~/Downloads/RegressionAnalysisDatasets/SimpleRegressionPrecision.csv") %>% 
    janitor::clean_names()

model10.2 <- lm(y ~ x, data = SRP)

summary(model10.2)

SRP <- broom::augment(model10.2, SRP, interval = "prediction")

ggplot(SRP, aes(x, y)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = (qnorm(0.95) * sigma(model10.2) - coef(model10.2)[1]) / coef(model10.2)[2], linetype = "dashed") +
    # one sided "PI"
    geom_line(aes(y = .fitted - sigma(model10.2)*qnorm(0.95))) +
    # two sided PI
    geom_ribbon(aes(ymin = .lower, ymax = .upper), linetype = "dotted", color = "black", fill = NA) +
    geom_point() +
    geom_smooth(method = lm)

model10.3 <- lm(ysample ~ xsample, data = SRP)

summary(model10.3)

SRPsample <- broom::augment(model10.3, SRP[1:50,], interval = "prediction")

ggplot(SRPsample, aes(xsample, ysample)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = (qnorm(0.95) * sigma(model10.3) - coef(model10.3)[1]) / coef(model10.3)[2], linetype = "dashed") +
    # one sided "PI" (90% PI)
    geom_line(aes(y = .fitted - sigma(model10.3)*qnorm(0.95))) +
    # two sided PI (95% PI)
    geom_ribbon(aes(ymin = .lower, ymax = .upper), linetype = "dotted", color = "black", fill = NA) +
    geom_point() +
    geom_smooth(method = lm)


MRP <- read_csv("~/Downloads/RegressionAnalysisDatasets/MultipleRegressionPrecision.csv") %>% 
    janitor::clean_names()

model10.4 <- lm(temperature ~ pressure + fuel_rate, data = MRP)

summary(model10.4)

MRP <- broom::augment(model10.4, MRP, interval = "prediction")

predict(model10.4, newdata = data.frame(pressure = 36, fuel_rate = 17.5),
        se.fit = T, interval = "prediction", level = 0.90)
ggeffects::ggpredict(model10.4, terms = c("pressure [36]", "fuel_rate [17.5]"), ci_level = 0.90, interval = "prediction")
ggeffects::ggpredict(model10.4, terms = c("pressure [36]", "fuel_rate [17.5]"), ci_level = 0.90, interval = "confidence")
ggeffects::ggpredict(model10.4, terms = c("pressure", "fuel_rate"), ci_level = 0.90, interval = "confidence") %>% plot()



#### Chapter 11: Tips, Common Questions, and Concerns ####

ImpVar <- read_csv("~/Downloads/RegressionAnalysisDatasets/ImportantVariables.csv") %>% 
    janitor::clean_names()

model11.1 <- lm(strength ~ time + pressure + temperature, data = ImpVar)

summary(model11.1)

caret::varImp(model11.1)


TestCon <- read_csv("~/Downloads/RegressionAnalysisDatasets/TestConstants.csv") %>% 
    janitor::clean_names()

# plot p.284
ggplot(TestCon, aes(input, output, color = condition, shape = condition)) +
    geom_point() +
    geom_smooth(method = lm)

model11.2 <- lm(output ~ input + condition, data = TestCon)

summary(model11.2)


TestSlp <- read_csv("~/Downloads/RegressionAnalysisDatasets/TestSlopes.csv") %>% 
    janitor::clean_names()

# plot p.286
ggplot(TestSlp, aes(input, output, color = condition, shape = condition)) +
    geom_point() +
    geom_smooth(method = lm)

model11.3 <- lm(output ~ input + condition + input * condition, data = TestSlp)

summary(model11.3)

equatiomatic::extract_eq(model11.3, coef_digits = 2, use_coefs = T)

# plot p.292
ggplot(Predict_BMI, aes(bmi, percent_fat)) +
    geom_point() +
    geom_smooth(method = lm, formula = y ~ poly(x, 2))

summary(model10.1)

predict(model10.1, newdata = data.frame(bmi = 18),
        interval = "prediction")


HighLow <- read_csv("~/Downloads/RegressionAnalysisDatasets/HighLowRsquaredData.csv") %>% 
    janitor::clean_names()

# plot p.300
ggplot(HighLow, aes(input, output1)) +
    geom_point() +
    geom_smooth(method = lm)

model11.4 <- lm(output1 ~ input, data = HighLow)

summary(model11.4)

# plot p.301
ggplot(HighLow, aes(input, output2)) +
    geom_point() +
    geom_smooth(method = lm)

model11.5 <- lm(output2 ~ input, data = HighLow)

summary(model11.5)

predict(model11.4, newdata = data.frame(input = 10),
        se.fit = T, interval = "confidence", level = 0.95)
predict(model11.5, newdata = data.frame(input = 10),
        se.fit = T, interval = "confidence", level = 0.95)

predict(model11.4, newdata = data.frame(input = 10),
        se.fit = T, interval = "prediction", level = 0.95)
predict(model11.5, newdata = data.frame(input = 10),
        se.fit = T, interval = "prediction", level = 0.95)



#### Chapter 12: Choosing the Correct Type of Regression ####

# plot p.309
ggplot(Predict_BMI, aes(bmi, percent_fat)) +
    geom_point() +
    geom_smooth(method = lm, formula = y ~ poly(x, 2)) +
    ggpmisc::stat_poly_eq(ggpmisc::use_label("eq", "adj.R2"), formula = y ~ poly(x, 2, raw = T)) +
    labs(x = "BMI", y = "Percent Fat")


# plot p.311
ggplot(Electron, aes(density_ln, mobility)) +
    geom_point() +
    geom_line(aes(y = fitted)) # from model4.11


# Nonlinear regression
mgcv::gam(y ~ s(x), data = df) # default distribution is Gaussian

# logistic (binomial) regression
glm(y ~ x, data = df, family = binomial)

# ordinal regression
MASS::polr(y ~ x, data = df, family = binomial)

# nominal regression
nnet::multinom(y ~ x, data = df)

# Poisson (count) regression
glm(y ~ x, data = df, family = poisson)
performance::check_overdispersion(model)

# negative binomial (count) regression
MASS::glm.nb(y ~ x, data = df)

# Zero-inflated Poisson regression
pscl::zeroinfl(y ~ x, data = df)

# Zero-inflated negative binomial regression
pscl::zeroinfl(y ~ x, data = df, dist = "negbin", link = "log")



#### Chapter 13: Examples of Other Types of Regression ####

Mammals <- read_csv("~/Downloads/RegressionAnalysisDatasets/Mammals.csv",
                    na = c("", "NA", "*")) %>% 
    janitor::clean_names()

# plot p.321
ggplot(Mammals, aes(adult_body_mass_g, basal_met_rate_m_lo2hr)) +
    geom_point() +
    geom_smooth(method = lm) +
    scale_x_log10() +
    scale_y_log10()

model13.1 <- lm(log10(basal_met_rate_m_lo2hr) ~ log10(adult_body_mass_g), data = Mammals)

summary(model13.1)

Mammals_drop <- Mammals %>% 
    select(met_gram, max_longevity_m, adult_body_mass_g) %>% 
    na.omit() %>% 
    mutate(log10_met_gram = log10(met_gram),
           log10_max_longevity_m = log10(max_longevity_m),
           masspergram = adult_body_mass_g/met_gram)

# plot p.322
ggplot(Mammals, aes(met_gram, max_longevity_m)) +
    geom_point() +
    geom_smooth(method = lm) +
    scale_x_log10() +
    scale_y_log10()

model13.2 <- lm(log10(max_longevity_m) ~ log10(met_gram), data = Mammals_drop)

summary(model13.2)

# plot p.323
ggeffects::ggpredict(model13.2, terms = "met_gram [all]") %>% plot() +
    labs(x = "Metabolic Rate per Gram") +
    scale_y_continuous(breaks = seq(0, 900, 100), limits = c(0, 900)) +
    theme_classic()


Cons <- read_csv("~/Downloads/RegressionAnalysisDatasets/114CongressRepublicans.csv") %>% 
    janitor::clean_names() %>% 
    mutate(freedom_caucus_logistic = ifelse(freedom_caucus == "No", 0, 1))

ggplot(Cons, aes(conservativeness, establishmentarianism, color = freedom_caucus, shape = freedom_caucus)) +
    geom_point() +
    scale_shape_manual(values = c(1, 15))

janitor::tabyl(Cons, freedom_caucus) %>% 
    janitor::adorn_totals()

model13.3 <- glm(freedom_caucus_logistic ~ conservativeness + establishmentarianism,
                 data = Cons,
                 family = binomial)

car::Anova(model13.3, type = 3) # guessing this is a Type III ANOVA

summary(model13.3)
gtsummary::tbl_regression(model13.3, exp = T)

# plot p.328 top
x_grid <- seq(min(Cons$conservativeness) - 0.05, max(Cons$conservativeness) + 0.05, length.out = 50)
y_grid <- seq(min(Cons$establishmentarianism) - 0.05, max(Cons$establishmentarianism) + 0.05, length.out = 50)
prediction_grid <- expand.grid(conservativeness = x_grid, establishmentarianism = y_grid)
prediction_grid$probability <- predict(model13.3, newdata = prediction_grid, type = "response")
ggplot(prediction_grid, aes(x = conservativeness, y = establishmentarianism, z = probability)) +
    geom_contour_filled(aes(fill = after_stat(level)), alpha = 0.7) + 
    scale_fill_viridis_d() + 
    geom_contour(color = "black", linewidth = 0.5) + 
    labs(x = "Conservativeness", y = "Establishmentarianism", fill = "Probability", 
         title = "Logistic Regression Contour Plot") +
    theme_minimal()

# plot p.328 bottom
p1 <- ggpredict(model13.3, terms = c("conservativeness [all]")) %>% plot() + 
    labs(x = "Conservativeness", y = "Probability of being in Freedum Caucus", title = "")
p2 <- ggpredict(model13.3, terms = c("establishmentarianism [all]")) %>% plot() + 
    labs(x = "Establishmentarianism", y = "Probability of being in Freedum Caucus", title = "")

library(patchwork)

p1 + p2 + plot_annotation(title = "Main Effects Plots for Logistic Regression")

Cons %>% 
    filter(name == "MCCARTHY" | name == "RYAN") %>% 
    select(name, z_cons, z_est)



#### End of Book ####
