# Project Name: Independent Research: Fertility Preferences #
# Section code: Statistic and Econometric Analysis

# Author/Maintainer of the code: Angga Bagus Bismoko
# ORCID: https://orcid.org/0000-0001-8716-7492
# Institution: Research Center for Population, National Research & Innovation
#              Agency Republic of Indonesia
# Email1: angg028@brin.go.id

# Start Date: Jan, 27th 2024
# End Date: Jul, 24th 2024

rm(list=ls())

# Step 0: Set Working Directory
setwd("D:/The BRIN A New Chapter/Pusat Riset Kependudukan - BRIN/Research Group - Family Dynamics/Proyek Pribadi/2. Famili Size Preferences/Fertility Preferences_Project")

# Step 1: Load Required Packages
# Install necessary packages with dependencies
install.packages(c("tidyverse", "haven", "openxlsx", "stargazer", "plm", 
                   "fixest", "forcats", "labelled", "gtsummary", "ggplot2", 
                   "reshape2", "summarytools", "car"), dependencies = TRUE)

# Load the packages
packages <- c("tidyverse", "haven", "openxlsx", "stargazer", "plm", 
              "fixest", "forcats", "labelled", "gtsummary", "ggplot2", 
              "reshape2", "summarytools", "car")
lapply(packages, library, character.only = TRUE)

# Step 2: Load and Inspect Data
# Import data
do.data <- openxlsx::read.xlsx("stability.data.update.xlsx")

# Display column names and summary statistics
colnames(do.data)
dfSummary(do.data)
stargazer(do.data, type = "text")

# Step 3: Perform Correlation Analysis
# Convert necessary columns to numeric
df <- do.data %>%
  mutate(across(c(matrilocal, parentsdom, parentsjoin, patrilocal, 
                  inlawsdom, inlawsjoin, wifeauth, employed),
                ~ as.numeric(as.character(.))))

# Select only numeric columns for correlation
numeric_columns <- df %>%
  select(matrilocal, parentsdom, parentsjoin, patrilocal, 
         inlawsdom, inlawsjoin, wifeauth, employed)

# Calculate the correlation matrix
cor_matrix <- cor(numeric_columns, use = "complete.obs")

# Melt the correlation matrix for plotting
melted_cor_matrix <- melt(cor_matrix)

# Create and print the heatmap using ggplot2
corr_plot <- ggplot(melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  coord_fixed()

# Print the correlation plot
print(corr_plot)

# Step 4: Check Multicollinearity
# Create a linear model with dummy variables
model <- lm(inc ~ factor(employed) + factor(education_level) + wifeauth, data = do.data)

# Calculate Variance Inflation Factor (VIF)
vif(model)

## All Panel Regression ##
# Testing for Heteroskedasticity for parents' influence model
fixed.pi <- plm(inc ~ age + agesq + agemar + wifeauth + parentsdom + matrilocal + economic_group + education_level + employed + ruralresident + javabaliresident + hhsize + post_reform, 
               data = do.data, # change from do.data 
               index = c("pidlink", "year"), # c(group index, time index)
               model = "within")

bptest(fixed.pi) 

# Testing for Heteroskedasticity for in-laws' influence model
fixed.ii <- plm(inc ~ age + agesq + agemar + wifeauth + inlawsdom + patrilocal + economic_group + education_level + employed + ruralresident + javabaliresident + hhsize + post_reform, 
               data = do.data, # change from do.data 
               index = c("pidlink", "year"), # c(group index, time index)
               model = "within")

bptest(fixed.ii)

# Testing individual & time-fixed effects for parents' influence model
fixed.p <- plm(inc ~ year + age + agesq + agemar + wifeauth + parentsdom + matrilocal + economic_group + education_level + employed + ruralresident + javabaliresident + hhsize + post_reform, 
               data = do.data, # change from do.data 
               index = c("pidlink", "year"), # c(group index, time index)
               model = "within")
summary(fixed.p)

random.p <- plm(inc ~ year + age + agesq + agemar + wifeauth + parentsdom + matrilocal + economic_group + education_level + employed + ruralresident + javabaliresident + hhsize + post_reform, 
                data = do.data, # change from do.data 
                index = c("pidlink", "year"), # c(group index, time index)
                model = "random")
summary(random.p)

hausman_test <- phtest(fixed.p, random.p)
print(hausman_test) #p-value = 1.57e-12
# The Hausman test is used to decide whether to use fixed effects or random effects.
# H0: FE coefficients are not significantly different from the RE coefficients
# Ha: FE coefficients are significantly different from the RE coefficients
# Check if the difference is statistically significant
if (hausman_test$p.value < 0.05) {
  cat("FE coefficients are significantly different from the RE coefficients (p-value < 0.05).")
} else {
  cat("FE coefficients are not significantly different from the RE coefficients (p-value >= 0.05).")
}

plmtest(fixed.p, c("individual"), type=("bp"))
# The result of the Lagrange Multiplier Test for individual effects indicates that there is STRONG statistical evidence
# to reject the null hypothesis, meaning that INDIVIDUAL EFFECTS are SIGNIFICANT in our panel data model (the p-value < 2.2e-16).

plmtest(fixed.p, c("time"), type=("bp"))
# The result of the Lagrange Multiplier Test for time effects indicates that there is NO STRONG statistical evidence
# to reject the null hypothesis, meaning that TIME EFFECTS are NOT SIGNIFICANT in our panel data model (the p-value < 0.1571).

# Testing individual & time-fixed effects for in-laws' influence model
fixed.i <- plm(inc ~ year + age + agesq + agemar + wifeauth + inlawsdom + patrilocal + economic_group + education_level + employed + ruralresident + javabaliresident + hhsize + post_reform, 
               data = do.data, # change from do.data 
               index = c("pidlink", "year"), # c(group index, time index)
               model = "within")
summary(fixed.i)

random.i <- plm(inc ~ year + age + agesq + agemar + wifeauth + inlawsdom + patrilocal + economic_group + education_level + employed + ruralresident + javabaliresident + hhsize + post_reform, 
                data = do.data, # change from do.data 
                index = c("pidlink", "year"), # c(group index, time index)
                model = "random")
summary(random.i)

hausman_test <- phtest(fixed.i, random.i)
print(hausman_test) #p-value = 2.87e-13
# The Hausman test is used to decide whether to use fixed effects or random effects.
# H0: FE coefficients are not significantly different from the RE coefficients
# Ha: FE coefficients are significantly different from the RE coefficients
# Check if the difference is statistically significant
if (hausman_test$p.value < 0.05) {
  cat("FE coefficients are significantly different from the RE coefficients (p-value < 0.05).")
} else {
  cat("FE coefficients are not significantly different from the RE coefficients (p-value >= 0.05).")
}

plmtest(fixed.i, c("individual"), type=("bp"))
# The result of the Lagrange Multiplier Test for individual effects indicates that there is STRONG statistical evidence
# to reject the null hypothesis, meaning that INDIVIDUAL EFFECTS are SIGNIFICANT in our panel data model (the p-value < 2.2e-16).

plmtest(fixed.i, c("time"), type=("bp"))
# The result of the Lagrange Multiplier Test for time effects indicates that there is NO STRONG statistical evidence
# to reject the null hypothesis, meaning that TIME EFFECTS are NOT SIGNIFICANT in our panel data model (the p-value < 0.1571).

### Fixed-Effect Model using fixest ###
## Full Model ##
# Model 1: Women's Authority - High
model.wah <- feols(inc ~ wifeauth + i(employed, ref = 0) + i(economic_group, ref = 'Low') + hhsize + i(ruralresident, ref = 0) + i(javabaliresident, ref = 0) + i(education_level, ref = 'Elementary') + age + agesq + agemar | pidlink, vcov = 'hetero', data = do.data)
model.wah.split <- feols(inc ~ wifeauth + i(employed, ref = 0) + i(economic_group, ref = 'Low') + hhsize + i(ruralresident, ref = 0) + i(javabaliresident, ref = 0) + i(education_level, ref = 'Elementary') + age + agesq + agemar | pidlink, fsplit = 'inc_stability', vcov = 'hetero', data = do.data)

etable(model.wah.split, fitstat = ~ n + r2 + wr2 + aic + bic)

# Model 2: Parents' Influence
# Model Matrilocal
model.pari1.split <- feols(inc ~ wifeauth + i(matrilocal) + i(employed, ref = 0) + i(economic_group, ref = 'Low') + hhsize + i(ruralresident, ref = 0) + i(javabaliresident, ref = 0) + i(education_level, ref = 'Elementary') + age + agesq + agemar | pidlink, fsplit = 'inc_stability', vcov = 'hetero', data = do.data)

etable(model.pari1.split, fitstat = ~ n + r2 + wr2 + aic + bic)

# Full Model
model.pari <- feols(inc ~ wifeauth + i(matrilocal) + i(parentsjoin) + i(employed, ref = 0) + i(economic_group, ref = 'Low') + hhsize + i(ruralresident, ref = 0) + i(javabaliresident, ref = 0) + i(education_level, ref = 'Elementary') + age + agesq + agemar | pidlink, vcov = 'hetero', data = do.data)
model.paria <- feols(inc ~ wifeauth + i(matrilocal) + i(parentsdom) + i(employed, ref = 0) + i(economic_group, ref = 'Low') + hhsize + i(ruralresident, ref = 0) + i(javabaliresident, ref = 0) + i(education_level, ref = 'Elementary') + age + agesq + agemar | pidlink, vcov = 'hetero', data = do.data)

etable(model.pari, model.paria, fitstat = ~ n + r2 + wr2 + aic + bic)

# Split Model
model.pari.split <- feols(inc ~ wifeauth + i(matrilocal) + i(parentsjoin) + i(employed, ref = 0) + i(economic_group, ref = 'Low') + hhsize + i(ruralresident, ref = 0) + i(javabaliresident, ref = 0) + i(education_level, ref = 'Elementary') + age + agesq + agemar | pidlink, fsplit = 'inc_stability', vcov = 'hetero', data = do.data)
model.paria.split <- feols(inc ~ wifeauth + i(matrilocal) + i(parentsdom) + i(employed, ref = 0) + i(economic_group, ref = 'Low') + hhsize + i(ruralresident, ref = 0) + i(javabaliresident, ref = 0) + i(education_level, ref = 'Elementary') + age + agesq + agemar | pidlink, fsplit = 'inc_stability', vcov = 'hetero', data = do.data)

etable(model.pari.split, model.paria.split, fitstat = ~ n + r2 + wr2 + aic + bic)

# Model 3: In-laws' Influence
# Model Patrilocal
model.ilwi1.split <- feols(inc ~ wifeauth + i(patrilocal) + i(employed, ref = 0) + i(economic_group, ref = 'Low') + hhsize + i(ruralresident, ref = 0) + i(javabaliresident, ref = 0) + i(education_level, ref = 'Elementary') + age + agesq + agemar | pidlink, fsplit = 'inc_stability', vcov = 'hetero', data = do.data)

# Full Model
model.ilwi <- feols(inc ~ wifeauth + i(patrilocal) + i(inlawsjoin) + i(employed, ref = 0) + i(economic_group, ref = 'Low') + hhsize + i(ruralresident, ref = 0) + i(javabaliresident, ref = 0) + i(education_level, ref = 'Elementary') + age + agesq + agemar | pidlink, vcov = 'hetero', data = do.data)
model.ilwia <- feols(inc ~ wifeauth + i(patrilocal) + i(inlawsdom) + i(employed, ref = 0) + i(economic_group, ref = 'Low') + hhsize + i(ruralresident, ref = 0) + i(javabaliresident, ref = 0) + i(education_level, ref = 'Elementary') + age + agesq + agemar | pidlink, vcov = 'hetero', data = do.data)

etable(model.ilwi, model.ilwia, fitstat = ~ n + r2 + wr2 + aic + bic)

# Splil Model
model.ilwi.split <- feols(inc ~ wifeauth + i(patrilocal) + i(inlawsjoin) + i(employed, ref = 0) + i(economic_group, ref = 'Low') + hhsize + i(ruralresident, ref = 0) + i(javabaliresident, ref = 0) + i(education_level, ref = 'Elementary') + age + agesq + agemar | pidlink, fsplit = 'inc_stability', vcov = 'hetero', data = do.data)
model.ilwia.split <- feols(inc ~ wifeauth + i(patrilocal) + i(inlawsdom) + i(employed, ref = 0) + i(economic_group, ref = 'Low') + hhsize + i(ruralresident, ref = 0) + i(javabaliresident, ref = 0) + i(education_level, ref = 'Elementary') + age + agesq + agemar | pidlink, fsplit = 'inc_stability', vcov = 'hetero', data = do.data)

etable(model.ilwi.split, model.ilwia.split, fitstat = ~ n + r2 + wr2 + aic + bic)

#### Interaction Model ####
# Model 2: Parents' Influence
model.pariant.split <- feols(inc ~ wifeauth + i(matrilocal) + i(parentsdom) + i(employed, ref = 0) + i(economic_group, ref = 'Low') + hhsize + i(ruralresident, ref = 0) + i(javabaliresident, ref = 0) + i(education_level, ref = 'Elementary') + age + agemar + agesq + i(wifeauth, matrilocal, ref= '0', ref2 = '0') + i(wifeauth, parentsdom, ref= '0', ref2 = '0') + i(matrilocal, employed, ref = '0', ref2 = '0') + i(parwifeauth, employed, ref = '0', ref2 = '0') + i(parentsdom, economic_group, ref= '0', ref2 = 'Low')| pidlink, fsplit = 'inc_stability', vcov = 'hetero', data = do.data)
model.parianto.split <- feols(inc ~ wifeauth + i(matrilocal) + i(parentsjoin) + i(employed, ref = 0) + i(economic_group, ref = 'Low') + hhsize + i(ruralresident, ref = 0) + i(javabaliresident, ref = 0) + i(education_level, ref = 'Elementary') + age + agemar + agesq + i(wifeauth, matrilocal, ref= '0', ref2 = '0') + i(wifeauth, parentsjoin, ref= '0', ref2 = '0') + i(matrilocal, employed, ref = '0', ref2 = '0') + i(parentsjoin, employed, ref = '0', ref2 = '0') + i(parentsjoin, economic_group, ref= '0', ref2 = 'Low')| pidlink, fsplit = 'inc_stability', vcov = 'hetero', data = do.data)

etable(model.pariant.split, model.parianto.split, fitstat = ~ n + r2 + wr2 + aic + bic)

# Model 3: In-laws' Influence
model.ilwiant.split <- feols(inc ~ wifeauth + i(patrilocal) + i(inlawsdom) + i(employed, ref = 0) + i(economic_group, ref = 'Low') + hhsize + i(ruralresident, ref = 0) + i(javabaliresident, ref = 0) + i(education_level, ref = 'Elementary') + age + agesq + agemar + i(wifeauth, patrilocal, ref= '0', ref2 = '0') + i(wifeauth, inlawsdom, ref= '0', ref2 = '0') + i(patrilocal, employed, ref = '0', ref2 = '0') + i(inlawsdom, employed, ref = '0', ref2 = '0') + i(inlawsdom, economic_group, ref= '0', ref2 = 'Low')| pidlink, fsplit = 'inc_stability', vcov = 'hetero', data = do.data)
model.ilwianto.split <- feols(inc ~ wifeauth + i(patrilocal) + i(inlawsjoin) + i(employed, ref = 0) + i(economic_group, ref = 'Low') + hhsize + i(ruralresident, ref = 0) + i(javabaliresident, ref = 0) + i(education_level, ref = 'Elementary') + age + agesq + agemar + i(wifeauth, patrilocal, ref= '0', ref2 = '0') + i(wifeauth, inlawsjoin, ref= '0', ref2 = '0') + i(patrilocal, employed, ref = '0', ref2 = '0') + i(inlawsjoin, employed, ref = '0', ref2 = '0') + i(inlawsjoin, economic_group, ref= '0', ref2 = 'Low') | pidlink, fsplit = 'inc_stability', vcov = 'hetero', data = do.data)

etable(model.ilwiant.split, model.ilwianto.split, fitstat = ~ n + r2 + wr2 + aic + bic)

##### END #####