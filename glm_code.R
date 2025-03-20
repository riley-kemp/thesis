# Load necessary libraries
library(ggplot2)
library(tidyverse)
library(dplyr)
library(corrplot)
library(e1071)
library(pscl)
library(sjPlot)

# Read the CSV file
data <- read.csv("Data/glm_data.csv", stringsAsFactors = TRUE)

# Re-level the role factor to correspond with my manual entry
data$Role <- factor(data$Role, levels = c("Supply", "Transit", "Demand"))

# Re-group data by country, and count occurrances within the supply chain (potentially useful)
all_country_species <- data %>%
  group_by(Country, Region, GDP_Per_Capita, Gov_Effect, Species_Richness, Control_Corruption, PA_Perc, GDP_PPP, Unemployment, Internet_Use, Rural_Pop, Education_GDP, Rule_of_Law, Trade_Infra, Tourism) %>%
  summarize(
    supply_count = sum (Role == "Supply"),
    transit_count = sum (Role == "Transit"),
    demand_count = sum (Role == "Demand"),
    supply_label_count = n_distinct(Label[Role == "Supply"]),
    transit_label_count = n_distinct(Label[Role == "Transit"]),
    demand_label_count = n_distinct(Label[Role == "Demand"]),
    total_paper_count = n_distinct(Label),
    .groups = "drop"
  )

# Create ratios for each IWT trade route component
all_country_species$supply_ratio = if_else(all_country_species$supply_label_count == 0, 0, all_country_species$supply_count / all_country_species$supply_label_count)
all_country_species$transit_ratio = if_else(all_country_species$transit_label_count == 0, 0, all_country_species$transit_count / all_country_species$transit_label_count)
all_country_species$demand_ratio = if_else(all_country_species$demand_label_count == 0, 0, all_country_species$demand_count / all_country_species$demand_label_count)


# ggplot(data, aes(x = Role, y = Species_Richness)) +
#   geom_boxplot() +
#   labs(title = "Species Richness by Supply Chain Role", x = "Role", y = "Species Richness")
# 
# 
# ggplot(data, aes(x = Role, y = GDP_Per_Capita)) +
#   geom_boxplot() +
#   labs(title = "GDP Per Capita by Supply Chain Role", x = "Role", y = "GDP Per Capita")
# 
#   


# Create a copy of the data
model_data <- all_country_species

# Remove variables that are no longer necessary
model_data <- subset(model_data, select = -GDP_Per_Capita)
model_data <- subset(model_data, select = -Control_Corruption)
#model_data <- na.omit(model_data)

# Keep only countries with 2 or more papers worth of observations
model_data <- subset(model_data, total_paper_count >1)
#model_data <- subset(model_data, total_paper_count > 2)

# Create dependent variable
model_data$SD_Ratio <- (model_data$demand_ratio - model_data$supply_ratio) / (model_data$demand_ratio + model_data$supply_ratio)


# Run Shapiro-Wilk test for all numeric columns (Code generated using ChatGPT)

# Loop through each variable and run Shapiro-Wilk test
for (col in colnames(model_data)) {
  # Check if the column is numeric
  if (is.numeric(model_data[[col]])) {
    # Remove NAs from the column
    clean_data <- na.omit(model_data[[col]])
    # Check if the sample size is within the valid range for the test
    if (length(clean_data) > 3 && length(clean_data) <= 5000) {
      test_result <- shapiro.test(clean_data)
      cat("\nVariable:", col, 
          "\nW-statistic:", test_result$statistic, 
          "\nP-value:", test_result$p.value, "\n")
    } else {
      cat("\nVariable:", col, "has an insufficient or too large sample size for Shapiro-Wilk test.\n")
    }
  } else {
    cat("\nVariable:", col, "is not numeric and will be skipped.\n")
  }
}


# Normal:Gov_Effect, Education_GDP
# Non-Normal: Species_Richness, PA_Perc, GDP_PPP, Unemployment, Internet_Use, Rural_Pop

# Additional test for the dependent variable
hist(model_data$SD_Ratio)


#write.csv(model_data,"model_data_pivoted.csv", row.names = FALSE)

# CORR TESTS
corr_matrix <- cor(model_data[, c(3:13,24)], use = "complete.obs")
corrplot.mixed(corr_matrix, order = "AOE")

# Rule_of_Law shows multicollinearity, removed
model_data <- subset(model_data, select = -Rule_of_Law)

# Re-check
corr_matrix <- cor(model_data[, c(3:12,23)], use = "complete.obs")
corrplot.mixed(corr_matrix, order = "AOE")

# Gov_Effect shows multicollinearity, removed
model_data <- subset(model_data, select = -Gov_Effect)

# Re-check
corr_matrix <- cor(model_data[, c(3:11,22)], use = "complete.obs")
corrplot.mixed(corr_matrix, order = "AOE")

# GDP_PPP shows multicollinearity, removed
model_data <- subset(model_data, select = -GDP_PPP)

# Re-check
corr_matrix <- cor(model_data[, c(3:10,21)], use = "complete.obs")
corrplot.mixed(corr_matrix, order = "AOE")

#####################################

# More dependent variable tests/checks

# Histogram
hist(model_data$SD_Ratio, main = "Histogram of SD_Ratio", xlab = "SD_Ratio", col = "lightblue", border = "black")

# Density plot
plot(density(model_data$SD_Ratio), main = "Density Plot of SD_Ratio", xlab = "SD_Ratio")

# Box plot
boxplot(model_data$SD_Ratio, main = "Boxplot of SD_Ratio", ylab = "SD_Ratio")

qqnorm(model_data$SD_Ratio)
qqline(model_data$SD_Ratio, col = "red")

skewness(model_data$SD_Ratio)
kurtosis(model_data$SD_Ratio)
# Non-normal, low skew, kurtosis < 3, using gaussian family glm

#############
# GLM
#############

model_SD <- glm(SD_Ratio ~ Education_GDP + log(Species_Richness + 1) + log(PA_Perc + 1)+ log(Unemployment + 1)  + log(Internet_Use + 1) + log(Rural_Pop + 1) + log(Trade_Infra + 1) + log(Tourism + 1), 
                data = model_data, family = gaussian(), na.action = na.exclude)


summary(model_SD)

# Remove Internet_Use, lower AIC

updated_model_SD <- glm(SD_Ratio ~ Education_GDP + log(Species_Richness + 1) + log(PA_Perc + 1)+ log(Unemployment + 1)  + log(Rural_Pop + 1) + log(Trade_Infra + 1) + log(Tourism + 1), 
                        data = model_data, family = gaussian(), na.action = na.exclude)


summary(updated_model_SD)

# Remove Tourism, higher AIC

updated_model_SD <- glm(SD_Ratio ~ Education_GDP + log(Species_Richness + 1) + log(PA_Perc + 1)+ log(Unemployment + 1)  + log(Rural_Pop + 1) + log(Trade_Infra + 1), 
                        data = model_data, family = gaussian(), na.action = na.exclude)


summary(updated_model_SD)



# Put back Tourism, Remove Trade_Infra, lowest AIC

updated_model_SD <- glm(SD_Ratio ~ Education_GDP + log(Species_Richness + 1) + log(PA_Perc + 1)+ log(Unemployment + 1)  + log(Rural_Pop + 1) +  log(Tourism + 1), 
                        data = model_data, family = gaussian(), na.action = na.exclude)


summary(updated_model_SD)

# Check resuduals
pchisq(updated_model_SD$deviance, updated_model_SD$df.residual)

par(mfrow = c(2,2))
plot(updated_model_SD)

plot(updated_model_SD$residuals)
qqnorm(residuals(updated_model_SD)) 
qqline(residuals(updated_model_SD), col = "red")


# Residual vs Fitted Plot
# Minimal pattern -> better fit
plot(fitted(updated_model_SD), residuals(updated_model_SD))
abline(h = 0, col = "red")  # Adds a horizontal line at 0

# Some deviation from the line
qqnorm(residuals(updated_model_SD)) 
qqline(residuals(updated_model_SD))


hist(residuals(updated_model_SD), breaks = 20, main = "Histogram of Residuals")

# Plot Cook's distance
plot(cooks.distance(updated_model_SD), type = "h", main = "Cook's Distance")
abline(h = 1, col = "red")

summary(updated_model_SD)


# McFadden's pseudo-R-squared (calculate manually)
ll_null <- logLik(glm(SD_Ratio ~ 1, data = model_data))  # null model
ll_full <- logLik(updated_model_SD)  # full model
pseudo_r2 <- 1 - (as.numeric(ll_full) / as.numeric(ll_null))
pseudo_r2

# Continue trying variable combinations

# Remove Rural_Pop, Higher AIC

updated_model_SD <- glm(SD_Ratio ~ Education_GDP + log(Species_Richness + 1) + log(PA_Perc + 1)+ log(Unemployment + 1)  + log(Tourism + 1), 
                        data = model_data, family = gaussian(), na.action = na.exclude)


summary(updated_model_SD)


# Put Back Rural_Pop, Remove PA_Perc, higher AIC

updated_model_SD <- glm(SD_Ratio ~ Education_GDP + log(Species_Richness + 1) +  log(Unemployment + 1)  + log(Rural_Pop + 1) +  log(Tourism + 1), 
                        data = model_data, family = gaussian(), na.action = na.exclude)


summary(updated_model_SD)


# Put Back PA_Perc, Remove Unemployment, higher AIC

updated_model_SD <- glm(SD_Ratio ~ Education_GDP + log(Species_Richness + 1) + log(PA_Perc + 1) + log(Rural_Pop + 1) +  log(Tourism + 1), 
                        data = model_data, family = gaussian(), na.action = na.exclude)


summary(updated_model_SD)


# Put Back Unemployment

updated_model_SD <- glm(SD_Ratio ~ Education_GDP + log(Species_Richness + 1) + log(PA_Perc + 1)+ log(Unemployment + 1)  + log(Rural_Pop + 1) +  log(Tourism + 1), 
                        data = model_data, family = gaussian(), na.action = na.exclude)


summary(updated_model_SD)

# Residual checks again

par(mfrow = c(2,2))
plot(updated_model_SD)

plot(updated_model_SD$residuals)
qqnorm(residuals(updated_model_SD)) 
qqline(residuals(updated_model_SD))


# Residual vs Fitted Plot
# Minimal pattern -> better fit
par(mfrow = c(1,1))
plot(fitted(updated_model_SD), residuals(updated_model_SD))
abline(h = 0, col = "red")  # Add a horizontal line at 0

# Some deviation from the line
qqnorm(residuals(updated_model_SD)) 
qqline(residuals(updated_model_SD))


hist(residuals(updated_model_SD), breaks = 20, main = "Histogram of Residuals")

# Plot Cook's distance, Nothing particularly far away
plot(cooks.distance(updated_model_SD), type = "h", main = "Cook's Distance")
abline(h = 1, col = "red")


summary(updated_model_SD)


# McFadden"s pseudo-R-squared (calculate manually)
ll_null <- logLik(glm(SD_Ratio ~ 1, data = model_data))  # null model
ll_full <- logLik(updated_model_SD)  # full model
pseudo_r2 <- 1 - (as.numeric(ll_full) / as.numeric(ll_null))
pseudo_r2

pseudo_r2 <- pR2(updated_model_SD)
print(pseudo_r2)

# Residuals plot
plot_model(updated_model_SD, show.values = TRUE, value.offset = 0.2, vline.color = "black",
           #axis.labels=c("Education Expenditure by Proportion of GDP (%)","Species Richness","Protected Areas (Terrestrial) (%)", "Unemployment (%)", "Rural Population (%)", "International tourism, expenditures (% of total imports)"),
           title = "GLM Coefficients (95% Confidence)")

# One last model test

# Only significant

updated_model_SD <- glm(SD_Ratio ~ Education_GDP + log(Species_Richness + 1), 
                        data = model_data, family = gaussian(), na.action = na.exclude)


summary(updated_model_SD)
