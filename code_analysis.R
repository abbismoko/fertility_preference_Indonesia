# Project Name: Independent Research: Fertility Preferences #
# Section code: Statistic and Econometric Analysis

# Author/Maintainer of the code: Angga Bagus Bismoko
# ORCID: https://orcid.org/0000-0001-8716-7492
# Institution: Research Center for Population, National Research & Innovation
#              Agency Republic of Indonesia
# Email1: angg028@brin.go.id

# Start Date: Jan, 27th 2024
# End Date: Mar, 26th 2025

rm(list=ls())

# Step 0: Set Working Directory
setwd("D:/The BRIN A New Chapter/Pusat Riset Kependudukan - BRIN/Research Group - Family Dynamics/Proyek Pribadi/2. Famili Size Preferences/Fertility Preferences_Project")

# Step 1: Load Required Packages
# Install necessary packages with dependencies
install.packages(c("tidyverse", "haven", "openxlsx", "stargazer", "plm", 
                   "fixest", "forcats", "labelled", "gtsummary", "ggplot2", 
                   "reshape2", "summarytools", "car", "lmtest", "tableone", "patchwork", "ggpattern", "ggalluvial"), dependencies = TRUE)

# Load the packages
packages <- c("tidyverse", "haven", "openxlsx", "stargazer", "plm", 
              "fixest", "forcats", "labelled", "gtsummary", "ggplot2", 
              "reshape2", "summarytools", "car", "lmtest", "tableone", "patchwork", "ggpattern", "ggalluvial")
lapply(packages, library, character.only = TRUE)

# Step 2: Load and Inspect Data
# Import data
do.data <- openxlsx::read.xlsx("stability.data.update.xlsx")

# Create the postmarital_living variable
do.data$postmarital_living <- with(do.data, ifelse(neolocal == 1, "neolocal",
                                                   ifelse(matrilocal == 1, "matrilocal",
                                                          ifelse(patrilocal == 1, "patrilocal", NA))))

# Create a new category based on the ideal number of children
do.data$child_pref_group <- cut(do.data$inc, 
                                breaks = c(-Inf, 2, 3, Inf), 
                                labels = c("Low", "Moderate", "High"))

# Remove values of inc above 6
do.data_filtered <- subset(do.data, inc <= 6)

# Ensure the panel is balanced by only including individuals with complete observations
do.data_balanced <- do.data_filtered %>%
  group_by(pidlink) %>%
  filter(n() == length(unique(do.data_filtered$year))) %>%
  ungroup()

do.data_balanced <- pdata.frame(do.data_balanced, index = c('pidlink', 'year'), drop.index = FALSE)

punbalancedness(do.data_balanced)
pdim(do.data_balanced)$balance
pdim(do.data_balanced)

# Categorize wifeauth into levels
do.data_balanced <- do.data_balanced %>%
  mutate(wifeauth_category = case_when(
    wifeauth == 0 ~ "no",
    wifeauth == 1 ~ "yes",
    TRUE ~ NA_character_
  ))

# Save the data frame to an XLS file
write.xlsx(do.data_balanced, "do.data_balanced.xlsx", rowNames = FALSE)

# Display column names and summary statistics
colnames(do.data_balanced)
dfSummary(do.data_balanced)

# Step 3: Perform Correlation Analysis
# Convert necessary columns to numeric
df <- do.data_balanced %>%
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
model <- lm(inc ~ factor(employed) + factor(education_level) + wifeauth, data = do.data_balanced)

# Calculate Variance Inflation Factor (VIF)
vif(model)

### Descriptive Statistics ###
# Convert Variables to Factors:
do.data_balanced$matrilocal <- as.factor(do.data_balanced$matrilocal)
do.data_balanced$patrilocal <- as.factor(do.data_balanced$patrilocal)
do.data_balanced$neolocal <- as.factor(do.data_balanced$neolocal)
do.data_balanced$wifeauth <- as.factor(do.data_balanced$wifeauth)
do.data_balanced$parentsjoin <- as.factor(do.data_balanced$parentsjoin)
do.data_balanced$parentsdom <- as.factor(do.data_balanced$parentsdom)
do.data_balanced$inlawsjoin <- as.factor(do.data_balanced$inlawsjoin)
do.data_balanced$inlawsdom <- as.factor(do.data_balanced$inlawsdom)
do.data_balanced$economic_group <- as.factor(do.data_balanced$economic_group)
do.data_balanced$ruralresident <- as.factor(do.data_balanced$ruralresident)
do.data_balanced$female <- as.factor(do.data_balanced$female)
do.data_balanced$javanese <- as.factor(do.data_balanced$javanese)
do.data_balanced$javabaliresident <- as.factor(do.data_balanced$javabaliresident)
do.data_balanced$inc_stability <- as.factor(do.data_balanced$inc_stability)
do.data_balanced$postmarital_living <- as.factor(do.data_balanced$postmarital_living)
do.data_balanced$child_pref_group <- as.factor(do.data_balanced$child_pref_group)
do.data_balanced$education_level <- as.factor(do.data_balanced$education_level)
do.data_balanced$married <- as.factor(do.data_balanced$married)

# Define the categorical and continuous variables
cat_vars <- c("wifeauth", "parentsjoin", "parentsdom",
              "inlawsjoin", "inlawsdom", "employed", "economic_group", "ruralresident",
              "javabaliresident", "education_level", "javanese", "female", "post_reform",
              "inc_stability", "postmarital_living", "child_pref_group", "wifeauth_category", "married")
cont_vars <- c("inc", "age", "agemar", "hhsize")

# Create the table by year
table_one <- CreateTableOne(vars = c(cat_vars, cont_vars), strata = "year", data = do.data_balanced) # by year
table_one <- CreateTableOne(vars = c(cat_vars, cont_vars), strata = "inc_stability", data = do.data_balanced) # by employed
table_one <- CreateTableOne(vars = c(cat_vars, cont_vars), strata = "child_pref_group", data = do.data_balanced) # by employed
table_one <- CreateTableOne(vars = c(cat_vars, cont_vars), strata = "post_reform", data = do.data_balanced) # by employed

# Print the table
print(table_one)

## Plots
library(ggplot2)
library(dplyr)

# Ensure key variables are factors for plotting
do.data_balanced <- do.data_balanced %>%
  mutate(post_reform = factor(post_reform, levels = c(0, 1), labels = c("Pre-Reform", "Post-Reform")),
         neolocal = as.factor(neolocal),
         matrilocal = as.factor(matrilocal),
         patrilocal = as.factor(patrilocal),
         wifeauth = as.factor(wifeauth),
         employed = as.factor(employed))

# Customize theme
custom_theme <- theme_classic() +
  theme(
    text = element_text(size = 12, family = "serif"), 
    plot.title = element_text(hjust = 0.5, face = "bold"), 
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

# Postmarital Living Arrangements plot
plot_postmarital <- ggplot(do.data_graph, aes(x = post_reform, pattern = postmarital_living)) +
  geom_bar_pattern(position = "fill", pattern_fill = "black") +
  labs(x = "Reform Status", y = "Proportion", pattern = "Postmarital Residence Status") +
  scale_pattern_manual(values = c("stripe", "circle", "crosshatch")) +
  custom_theme

# Wife Authority plot
plot_wifeauth <- ggplot(do.data_graph, aes(x = post_reform, pattern = wifeauth)) +
  geom_bar_pattern(position = "fill", pattern_fill = "black") +
  labs(x = "Reform Status", y = "Proportion", pattern = "Wives' Decision Authority") +
  scale_pattern_manual(values = c("stripe", "circle")) +
  custom_theme

# Ideal Number of Children plot
plot_idealchildren <- ggplot(do.data_graph, aes(x = post_reform, y = inc)) +
  geom_boxplot() +
  labs(x = "Reform Status", y = "Ideal Number of Children") +
  custom_theme

# Arrange the plots in one row with three columns
final_plot <- plot_postmarital + plot_wifeauth + plot_idealchildren + plot_layout(ncol = 3)

# Display the final combined plot
final_plot


# Pivot do.data_balanced to wide format for postmarital_living
do.data_wide <- do.data_balanced %>%
  select(pidlink, year, postmarital_living) %>%  # Select relevant columns
  pivot_wider(names_from = year, 
              values_from = postmarital_living, 
              names_prefix = "postmarital_year_")  # Create wide format with year-specific columns

# Restructure the data into long format
do.data_long <- do.data_wide %>%
  pivot_longer(cols = starts_with("postmarital_year_"), 
               names_to = "year", 
               values_to = "postmarital_living") %>%
  mutate(year = gsub("postmarital_year_", "", year))  # Clean up the year column

# Ensure 'year' and 'postmarital_living' are factors for better plotting
do.data_long$year <- factor(do.data_long$year, levels = c("1997", "2000", "2007", "2014"))
do.data_long$postmarital_living <- factor(do.data_long$postmarital_living, 
                                          levels = c("matrilocal", "patrilocal", "neolocal"))

# We need to define 'x', 'stratum', and 'alluvium' correctly for the plot
do.data_long <- do.data_long %>%
  mutate(stratum = postmarital_living, alluvium = pidlink)

# Create the alluvial plot
ggplot(do.data_long,
       aes(x = year, stratum = stratum, alluvium = alluvium, fill = stratum)) +
  geom_flow(aes(fill = stratum), width = 0.1, alpha = 0.8, color = "black") +  # Flow lines with transparency and outline
  geom_stratum(width = 0.1, color = "black") +  # Strata with black borders
  geom_text(stat = "stratum", aes(label = after_stat(count)), 
            size = 3, color = "black", vjust = -0.5) +  # Add count labels within strata
  scale_x_discrete(limits = c("1997", "2000", "2007", "2014"), 
                   expand = c(0.15, 0.15)) +  # Set x-axis (years) with spacing
  scale_fill_manual(values = c("matrilocal" = "lightblue", 
                               "patrilocal" = "lightgreen", 
                               "neolocal" = "pink")) +  # Define color palette for strata
  labs(x = "Year", 
       y = "Number of Individuals", 
       fill = "Postmarital Residences") +  # Axis labels and legend title
  theme_minimal() +  # Minimal theme for clarity
  theme(
    legend.position = "bottom",  # Position the legend at the bottom
    legend.title = element_text(size = 10),  # Adjust legend title size
    legend.text = element_text(size = 8),  # Adjust legend text size
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  # Center and bold the title
    axis.text.x = element_text(size = 10),  # Adjust x-axis text size
    axis.title = element_text(size = 12)  # Adjust axis title size
  ) +
  ggtitle("The Transition of Postmarital Residences over Waves")  # Add a centered title


# Pivot to wide format for wifeauth_category
do.data_wide_wifeauth <- do.data_balanced %>%
  select(pidlink, year, wifeauth_category) %>%
  pivot_wider(names_from = year, 
              values_from = wifeauth_category, 
              names_prefix = "wifeauth_year_")

# Restructure to long format
wifeauth_data_long <- do.data_wide_wifeauth %>%
  pivot_longer(cols = starts_with("wifeauth_year_"), 
               names_to = "year", 
               values_to = "wifeauth") %>%
  mutate(year = gsub("wifeauth_year_", "", year))  # Clean up the year column

# Ensure 'year' and 'wifeauth' are factors
wifeauth_data_long$year <- factor(wifeauth_data_long$year, levels = c("1997", "2000", "2007", "2014"))
wifeauth_data_long$wifeauth <- factor(wifeauth_data_long$wifeauth, levels = c("no", "yes"))

# Create the alluvial plot
ggplot(wifeauth_data_long,
       aes(x = year, stratum = wifeauth, alluvium = pidlink, fill = wifeauth)) +
  geom_flow(aes(fill = wifeauth), width = 0.1, alpha = 0.8, color = "black") +  # Flow lines
  geom_stratum(width = 0.1, color = "black") +  # Strata
  geom_text(stat = "stratum", aes(label = after_stat(count)), 
            size = 3, color = "black", vjust = -0.5) +  # Add count labels
  scale_x_discrete(limits = c("1997", "2000", "2007", "2014"), 
                   expand = c(0.15, 0.15)) +  # Set x-axis (years) with spacing
  scale_fill_manual(values = c("no" = "lightblue", "yes" = "lightgreen")) +  # Adjust colors
  labs(x = "Year", 
       y = "Number of Individuals", 
       fill = "Wife as Primary Decision-Maker") +  # Axis labels and legend title
  theme_minimal() +  # Minimal theme for clarity
  theme(
    legend.position = "bottom",  # Position the legend at the bottom
    legend.title = element_text(size = 10),  # Adjust legend title size
    legend.text = element_text(size = 8),  # Adjust legend text size
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  # Center and bold the title
    axis.text.x = element_text(size = 10),  # Adjust x-axis text size
    axis.title = element_text(size = 12)  # Adjust axis title size
  ) +
  ggtitle("The Transition of Wife as the Primary Decision-Maker over Waves")  # Add a centered title

# Pivot the data into wide format for child_pref_group
do.data_wide_child_pref <- do.data_balanced %>%
  select(pidlink, year, child_pref_group) %>%
  pivot_wider(names_from = year, 
              values_from = child_pref_group, 
              names_prefix = "child_pref_group_year_")

# Reshape the data into long format
do.data_long_child_pref <- do.data_wide_child_pref %>%
  pivot_longer(cols = starts_with("child_pref_group_year_"), 
               names_to = "year", 
               values_to = "child_pref_group") %>%
  mutate(year = gsub("child_pref_group_year_", "", year))  # Clean up the year column

# Ensure 'year' and 'child_pref_group' are factors
do.data_long_child_pref$year <- factor(do.data_long_child_pref$year, levels = c("1997", "2000", "2007", "2014"))
do.data_long_child_pref$child_pref_group <- factor(do.data_long_child_pref$child_pref_group, 
                                                   levels = c("Low", "Moderate", "High"))

# Prepare stratum and alluvium for plotting
do.data_long_child_pref <- do.data_long_child_pref %>%
  mutate(stratum = child_pref_group, alluvium = pidlink)

# Create the alluvial plot
ggplot(do.data_long_child_pref,
       aes(x = year, stratum = stratum, alluvium = alluvium, fill = stratum)) +
  geom_flow(aes(fill = stratum), width = 0.1, alpha = 0.8, color = "black") +  # Flow lines
  geom_stratum(width = 0.1, color = "black") +  # Strata with borders
  geom_text(stat = "stratum", aes(label = after_stat(count)), 
            size = 3, color = "black", vjust = -0.5) +  # Add count labels
  scale_x_discrete(limits = c("1997", "2000", "2007", "2014"), 
                   expand = c(0.15, 0.15)) +  # Set x-axis (years) with spacing
  scale_fill_manual(values = c("Low" = "lightblue", 
                               "Moderate" = "lightgreen", 
                               "High" = "pink")) +  # Define colors for strata
  labs(x = "Year", 
       y = "Number of Individuals", 
       fill = "Ideal Number Fertility Preference Group") +  # Axis labels and legend title
  theme_minimal() +  # Minimal theme for clarity
  theme(
    legend.position = "bottom",  # Position the legend at the bottom
    legend.title = element_text(size = 10),  # Adjust legend title size
    legend.text = element_text(size = 8),  # Adjust legend text size
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  # Center and bold the title
    axis.text.x = element_text(size = 10),  # Adjust x-axis text size
    axis.title = element_text(size = 12)  # Adjust axis title size
  ) +
  ggtitle("The Transition of Women's Ideal Number Fertility Preference Over Time")  # Add a centered title


## All Panel Regression ##
# Testing for Heteroskedasticity for parents' influence model
fixed.pi <- plm(inc ~ age + agesq + agemar + wifeauth + parentsjoin + matrilocal + economic_group + education_level + employed + ruralresident + javabaliresident + hhsize, 
                data = do.data_balanced, # change from do.data 
                index = c("pidlink", "year"), # c(group index, time index)
                model = "within")

fixed.pii <- plm(inc ~ age + agesq + agemar + wifeauth + parentsdom + matrilocal + economic_group + education_level + employed + ruralresident + javabaliresident + hhsize, 
                data = do.data_balanced, # change from do.data 
                index = c("pidlink", "year"), # c(group index, time index)
                model = "within")

bptest(fixed.pi) #Heteroskedasticity exist, handle with cluster-robust errors
bptest(fixed.pii) #Heteroskedasticity exist, handle with cluster-robust errors

# Testing for Heteroskedasticity for in-laws' influence model
fixed.ii <- plm(inc ~ age + agesq + agemar + wifeauth + inlawsjoin + patrilocal + economic_group + education_level + employed + ruralresident + javabaliresident + hhsize, 
               data = do.data_balanced, # change from do.data 
               index = c("pidlink", "year"), # c(group index, time index)
               model = "within")

fixed.iii <- plm(inc ~ age + agesq + agemar + wifeauth + inlawsdom + patrilocal + economic_group + education_level + employed + ruralresident + javabaliresident + hhsize, 
                data = do.data_balanced, # change from do.data 
                index = c("pidlink", "year"), # c(group index, time index)
                model = "within")

bptest(fixed.ii) #Heteroskedasticity exist, handle with cluster-robust errors
bptest(fixed.iii) #Heteroskedasticity exist, handle with cluster-robust errors

# Testing individual & time-fixed effects for parents' influence model
fixed.p <- plm(inc ~ year + age + agesq + agemar + wifeauth + parentsdom + matrilocal + economic_group + education_level + employed + ruralresident + javabaliresident + hhsize, 
               data = do.data_balanced, # change from do.data 
               index = c("pidlink", "year"), # c(group index, time index)
               model = "within")
summary(fixed.p)

random.p <- plm(inc ~ year + age + agesq + agemar + wifeauth + parentsdom + matrilocal + economic_group + education_level + employed + ruralresident + javabaliresident + hhsize, 
                data = do.data_balanced, # change from do.data 
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
fixed.i <- plm(inc ~ year + age + agesq + agemar + wifeauth + inlawsdom + patrilocal + economic_group + education_level + employed + ruralresident + javabaliresident + hhsize, 
               data = do.data_balanced, # change from do.data 
               index = c("pidlink", "year"), # c(group index, time index)
               model = "within")
summary(fixed.i)

random.i <- plm(inc ~ year + age + agesq + agemar + wifeauth + inlawsdom + patrilocal + economic_group + education_level + employed + ruralresident + javabaliresident + hhsize, 
                data = do.data_balanced, # change from do.data 
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
model.wah <- feols(inc ~ i(wifeauth) + i(neolocal) + i(employed) + i(economic_group, ref = 'Low') + hhsize + i(ruralresident, ref = 0) + i(javabaliresident, ref = 0) + i(education_level, ref = 'Elementary') + age + agesq + agemar | pidlink, fsplit = 'inc_stability', vcov = 'cluster', data = do.data_balanced)
model.wah.split <- feols(inc ~ i(wifeauth) + i(employed) + i(economic_group, ref = 'Low') + hhsize + i(ruralresident, ref = 0) + i(javabaliresident, ref = 0) + i(education_level, ref = 'Elementary') + age + agesq + agemar | pidlink, fsplit = 'inc_stability', vcov = 'cluster', data = do.data_balanced)
model.wah0.split <- feols(inc ~ i(wifeauth) + i(employed) + i(economic_group, ref = 'Low') + hhsize + i(ruralresident, ref = 0) + i(javabaliresident, ref = 0) + i(education_level, ref = 'Elementary') + age + agesq + agemar | pidlink, fsplit = 'inc_stability', vcov = 'hetero', data = do.data_balanced)

#model.waha.split <- feols(inc ~ i(wifeauth) + i(employed) + i(economic_group, ref = 'Low') + hhsize + i(ruralresident, ref = 0) + i(javabaliresident, ref = 0) + i(education_level, ref = 'Elementary') + age + agesq + agemar | pidlink, fsplit = 'child_pref_group', vcov = 'hetero', data = do.data_balanced)

etable(model.wah.split, fitstat = ~ n + r2 + wr2 + aic + bic)
etable(model.wah.split, model.wah0.split, keep = "wifeauth|employed|economic_group", fitstat = ~ n + r2 + wr2 + aic + bic)
#etable(model.waha.split, keep = "wifeauth|employed|economic_group", fitstat = ~ n + r2 + wr2 + aic + bic)

# Model 2: Parents' Influence
# Model Matrilocal
#model.matri.split <- feols(inc ~ i(wifeauth) + i(matrilocal) + i(employed, ref = 0) + i(economic_group, ref = 'Low') + 
#                             hhsize + i(ruralresident, ref = 0) + i(javabaliresident, ref = 0) + 
#                             i(education_level, ref = 'Elementary') + age + agesq + agemar | pidlink, fsplit = 'inc_stability', vcov = 'hetero', data = do.data_balanced)

#model.matri0.split <- feols(inc ~ i(wifeauth) + i(matrilocal) + i(employed, ref = 0) + i(economic_group, ref = 'Low') + 
#                             hhsize + i(ruralresident, ref = 0) + i(javabaliresident, ref = 0) + 
#                             i(education_level, ref = 'Elementary') + age + agesq + agemar | pidlink, fsplit = 'inc_stability', vcov = 'cluster', data = do.data_balanced)

#model.matri1.split <- feols(inc ~ i(wifeauth) + i(postmarital_living, ref = "neolocal") + i(employed, ref = 0) + i(economic_group, ref = 'Low') +
#                           hhsize + i(ruralresident, ref = 0) + i(javabaliresident, ref = 0) + 
#                           i(education_level, ref = 'Elementary') + age + agesq + agemar | pidlink, fsplit = 'inc_stability',
#                         vcov = 'hetero', data = subset(do.data_balanced, postmarital_living %in% c("neolocal", "matrilocal")))

#model.matri2.split <- feols(inc ~ i(wifeauth) + i(postmarital_living, ref = "neolocal") + i(employed, ref = 0) + i(economic_group, ref = 'Low') +
#                              hhsize + i(ruralresident, ref = 0) + i(javabaliresident, ref = 0) + 
#                              i(education_level, ref = 'Elementary') + age + agesq + agemar | pidlink, fsplit = 'inc_stability',
#                            vcov = 'cluster', data = subset(do.data_balanced, postmarital_living %in% c("neolocal", "matrilocal")))

#etable(model.matri.split, model.matri0.split, fitstat = ~ n + r2 + wr2 + aic + bic)
#etable(model.matri1.split, model.matri2.split, fitstat = ~ n + r2 + wr2 + aic + bic)

#etable(model.matri.split, model.matri0.split, keep = "wifeauth|matrilocal|employed|economic_group|postmarital", fitstat = ~ n + r2 + wr2 + aic + bic)
#etable(model.matri1.split, model.matri2.split, keep = "wifeauth|matrilocal|employed|economic_group|postmarital", fitstat = ~ n + r2 + wr2 + aic + bic)

# Full Model
#model.parijoin <- feols(inc ~ i(wifeauth) + i(matrilocal) + i(parentsjoin) + i(employed, ref = 0) + i(economic_group, ref = 'Low') + hhsize + i(ruralresident, ref = 0) + i(javabaliresident, ref = 0) + i(education_level, ref = 'Elementary') + age + agesq + agemar | pidlink, vcov = 'hetero', data = do.data_balanced)
#model.paridom <- feols(inc ~ i(wifeauth) + i(matrilocal) + i(parentsdom) + i(employed, ref = 0) + i(economic_group, ref = 'Low') + hhsize + i(ruralresident, ref = 0) + i(javabaliresident, ref = 0) + i(education_level, ref = 'Elementary') + age + agesq + agemar | pidlink, vcov = 'hetero', data = do.data_balanced)

#etable(model.parijoin, model.paridom, fitstat = ~ n + r2 + wr2 + aic + bic)
#etable(model.parijoin, model.paridom, keep = "wifeauth|matrilocal|employed|economic_group|postmarital|parents|inlaws", fitstat = ~ n + r2 + wr2 + aic + bic)

# Split Model
model.parijoin.split <- feols(inc ~ i(wifeauth) + i(matrilocal) + i(parentsjoin) + i(employed, ref = 0) + i(economic_group, ref = 'Low') + hhsize + i(ruralresident, ref = 0) + i(javabaliresident, ref = 0) + i(education_level, ref = 'Elementary') + age + agesq + agemar | pidlink, fsplit = 'inc_stability', vcov = 'cluster', data = do.data_balanced)
model.paridom.split <- feols(inc ~ i(wifeauth) + i(matrilocal) + i(parentsdom) + i(employed, ref = 0) + i(economic_group, ref = 'Low') + hhsize + i(ruralresident, ref = 0) + i(javabaliresident, ref = 0) + i(education_level, ref = 'Elementary') + age + agesq + agemar | pidlink, fsplit = 'inc_stability', vcov = 'cluster', data = do.data_balanced)

etable(model.parijoin.split, model.paridom.split, keep = "wifeauth|matrilocal|employed|economic_group|postmarital|parents|inlaws", fitstat = ~ n + r2 + wr2 + aic + bic)

model.paritjoin <- feols(inc ~ i(wifeauth) + i(parentsjoin) + i(employed, ref = 0) + i(economic_group, ref = 'Low') +
                            hhsize + i(ruralresident, ref = 0) + i(javabaliresident, ref = 0) + 
                            i(education_level, ref = 'Elementary') + age + agesq + agemar + 
                            i(postmarital_living, ref = "neolocal") | pidlink, fsplit = 'inc_stability',
                          vcov = 'cluster', data = subset(do.data_balanced, postmarital_living %in% c("neolocal", "matrilocal")))

model.paritjoin0 <- feols(inc ~ i(wifeauth) + i(parentsjoin) + i(employed, ref = 0) + i(economic_group, ref = 'Low') +
                           hhsize + i(ruralresident, ref = 0) + i(javabaliresident, ref = 0) + 
                           i(education_level, ref = 'Elementary') + age + agesq + agemar + 
                           i(postmarital_living, ref = "neolocal") | pidlink, fsplit = 'inc_stability',
                         vcov = 'hetero', data = subset(do.data_balanced, postmarital_living %in% c("neolocal", "matrilocal")))


model.paritdom <- feols(inc ~ i(wifeauth) + i(parentsdom) + i(employed, ref = 0) + i(economic_group, ref = 'Low') +
                           hhsize + i(ruralresident, ref = 0) + i(javabaliresident, ref = 0) + 
                           i(education_level, ref = 'Elementary') + age + agesq + agemar + 
                           i(postmarital_living, ref = "neolocal") | pidlink, fsplit = 'inc_stability',
                         vcov = 'cluster', data = subset(do.data_balanced, postmarital_living %in% c("neolocal", "matrilocal")))

model.paritdom0 <- feols(inc ~ i(wifeauth) + i(parentsdom) + i(employed, ref = 0) + i(economic_group, ref = 'Low') +
                          hhsize + i(ruralresident, ref = 0) + i(javabaliresident, ref = 0) + 
                          i(education_level, ref = 'Elementary') + age + agesq + agemar + 
                          i(postmarital_living, ref = "neolocal") | pidlink, fsplit = 'inc_stability',
                        vcov = 'hetero', data = subset(do.data_balanced, postmarital_living %in% c("neolocal", "matrilocal")))

etable(model.paritjoin, model.paritdom, fitstat = ~ n + r2 + wr2 + aic + bic)

# Model 3: In-laws' Influence
# Model Patrilocal
#model.ilwi0.split <- feols(inc ~ i(wifeauth) + i(patrilocal) + i(employed, ref = 0) + i(economic_group, ref = 'Low') + 
 #                            hhsize + i(ruralresident, ref = 0) + i(javabaliresident, ref = 0) + 
  #                           i(education_level, ref = 'Elementary') + age + agesq + agemar | pidlink, fsplit = 'inc_stability', vcov = 'hetero', data = do.data_balanced)

#model.patri0.split <- feols(inc ~ i(wifeauth) + i(postmarital_living, ref = "neolocal") + i(employed, ref = 0) + i(economic_group, ref = 'Low') +
 #                             hhsize + i(ruralresident, ref = 0) + i(javabaliresident, ref = 0) + 
  #                            i(education_level, ref = 'Elementary') + age + agesq + agemar | pidlink, fsplit = 'inc_stability',
   #                         vcov = 'cluster', data = subset(do.data_balanced, postmarital_living %in% c("neolocal", "patrilocal")))


#model.patri1.split <- feols(inc ~ i(wifeauth) + i(postmarital_living, ref = "neolocal") + i(employed, ref = 0) + i(economic_group, ref = 'Low') +
 #                             hhsize + i(ruralresident, ref = 0) + i(javabaliresident, ref = 0) + 
  #                            i(education_level, ref = 'Elementary') + age + agesq + agemar | pidlink, fsplit = 'inc_stability',
   #                         vcov = 'hetero', data = subset(do.data_balanced, postmarital_living %in% c("neolocal", "patrilocal")))


#etable(model.ilwi0.split, fitstat = ~ n + r2 + wr2 + aic + bic)
#etable(model.patri0.split, keep = "wifeauth|patrilocal|employed|economic_group|postmarital", fitstat = ~ n + r2 + wr2 + aic + bic)

# Full Model
#model.ilwijoin <- feols(inc ~ i(wifeauth) + i(patrilocal) + i(inlawsjoin) + i(employed, ref = 0) + i(economic_group, ref = 'Low') + hhsize + i(ruralresident, ref = 0) + i(javabaliresident, ref = 0) + i(education_level, ref = 'Elementary') + age + agesq + agemar | pidlink, vcov = 'hetero', data = do.data_balanced)
#model.ilwidom <- feols(inc ~ i(wifeauth) + i(patrilocal) + i(inlawsdom) + i(employed, ref = 0) + i(economic_group, ref = 'Low') + hhsize + i(ruralresident, ref = 0) + i(javabaliresident, ref = 0) + i(education_level, ref = 'Elementary') + age + agesq + agemar | pidlink, vcov = 'hetero', data = do.data_balanced)

#etable(model.ilwijoin, model.ilwidom, fitstat = ~ n + r2 + wr2 + aic + bic)
#etable(model.ilwijoin, model.ilwidom, keep = "wifeauth|patrilocal|employed|economic_group|postmarital|parents|inlaws", fitstat = ~ n + r2 + wr2 + aic + bic)

# Split Model
model.ilwijoin.split <- feols(inc ~ i(wifeauth) + i(patrilocal) + i(inlawsjoin) + i(employed, ref = 0) + i(economic_group, ref = 'Low') + hhsize + i(ruralresident, ref = 0) + i(javabaliresident, ref = 0) + i(education_level, ref = 'Elementary') + age + agesq + agemar | pidlink, fsplit = 'inc_stability', vcov = 'cluster', data = do.data_balanced)
model.ilwidom.split <- feols(inc ~ i(wifeauth) + i(patrilocal) + i(inlawsdom) + i(employed, ref = 0) + i(economic_group, ref = 'Low') + hhsize + i(ruralresident, ref = 0) + i(javabaliresident, ref = 0) + i(education_level, ref = 'Elementary') + age + agesq + agemar | pidlink, fsplit = 'inc_stability', vcov = 'cluster', data = do.data_balanced)

etable(model.ilwijoin.split, model.ilwidom.split, keep = "wifeauth|patrilocal|employed|economic_group|postmarital|parents|inlaws", fitstat = ~ n + r2 + wr2 + aic + bic)

model.ilwitjoin <- feols(inc ~ i(wifeauth) + i(inlawsjoin) + i(employed, ref = 0) + i(economic_group, ref = 'Low') +
                            hhsize + i(ruralresident, ref = 0) + i(javabaliresident, ref = 0) + 
                            i(education_level, ref = 'Elementary') + age + agesq + agemar + 
                            i(postmarital_living, ref = "neolocal") | pidlink, fsplit = 'inc_stability', 
                          vcov = 'cluster', data = subset(do.data_balanced, postmarital_living %in% c("neolocal", "patrilocal")))

model.ilwitjoin0 <- feols(inc ~ i(wifeauth) + i(inlawsjoin) + i(employed, ref = 0) + i(economic_group, ref = 'Low') +
                           hhsize + i(ruralresident, ref = 0) + i(javabaliresident, ref = 0) + 
                           i(education_level, ref = 'Elementary') + age + agesq + agemar + 
                           i(postmarital_living, ref = "neolocal") | pidlink, fsplit = 'inc_stability', 
                         vcov = 'hetero', data = subset(do.data_balanced, postmarital_living %in% c("neolocal", "patrilocal")))


model.ilwitdom <- feols(inc ~ i(wifeauth) + i(inlawsdom) + i(employed, ref = 0) + i(economic_group, ref = 'Low') +
                           hhsize + i(ruralresident, ref = 0) + i(javabaliresident, ref = 0) + 
                           i(education_level, ref = 'Elementary') + age + agesq + agemar + 
                           i(postmarital_living, ref = "neolocal") | pidlink, fsplit = 'inc_stability', 
                         vcov = 'cluster', data = subset(do.data_balanced, postmarital_living %in% c("neolocal", "patrilocal")))

model.ilwitdom0 <- feols(inc ~ i(wifeauth) + i(inlawsdom) + i(employed, ref = 0) + i(economic_group, ref = 'Low') +
                          hhsize + i(ruralresident, ref = 0) + i(javabaliresident, ref = 0) + 
                          i(education_level, ref = 'Elementary') + age + agesq + agemar + 
                          i(postmarital_living, ref = "neolocal") | pidlink, fsplit = 'inc_stability', 
                        vcov = 'hetero', data = subset(do.data_balanced, postmarital_living %in% c("neolocal", "patrilocal")))

etable(model.ilwitjoin, model.ilwitdom, keep = "wifeauth|patrilocal|employed|economic_group|postmarital|parents|inlaws", fitstat = ~ n + r2 + wr2 + aic + bic)

#### Interaction Model ####
# Model 2: Parents' Influence
model.pariant.split <- feols(inc ~ wifeauth + i(matrilocal) + i(parentsdom) + i(employed, ref = 0) + i(economic_group, ref = 'Low') + hhsize + i(ruralresident, ref = 0) + i(javabaliresident, ref = 0) + i(education_level, ref = 'Elementary') + age + agemar + agesq + i(wifeauth, matrilocal, ref= '0', ref2 = '0') + i(wifeauth, parentsdom, ref= '0', ref2 = '0') + i(matrilocal, employed, ref = '0', ref2 = '0') + i(parentsdom, employed, ref = '0', ref2 = '0') + i(parentsdom, economic_group, ref= '0', ref2 = 'Low')| pidlink, fsplit = 'inc_stability', vcov = 'cluster', data = do.data)
model.parianto.split <- feols(inc ~ wifeauth + i(matrilocal) + i(parentsjoin) + i(employed, ref = 0) + i(economic_group, ref = 'Low') + hhsize + i(ruralresident, ref = 0) + i(javabaliresident, ref = 0) + i(education_level, ref = 'Elementary') + age + agemar + agesq + i(wifeauth, matrilocal, ref= '0', ref2 = '0') + i(wifeauth, parentsjoin, ref= '0', ref2 = '0') + i(matrilocal, employed, ref = '0', ref2 = '0') + i(parentsjoin, employed, ref = '0', ref2 = '0') + i(parentsjoin, economic_group, ref= '0', ref2 = 'Low')| pidlink, fsplit = 'inc_stability', vcov = 'cluster', data = do.data)

etable(model.pariant.split, model.parianto.split, fitstat = ~ n + r2 + wr2 + aic + bic)

model.paritjoin.int <- feols(inc ~ i(wifeauth) + i(parentsjoin) + i(employed, ref = 0) + i(economic_group, ref = 'Low') +
                           hhsize + i(ruralresident, ref = 0) + i(javabaliresident, ref = 0) + 
                           i(education_level, ref = 'Elementary') + age + agesq + agemar + 
                           i(postmarital_living, ref = "neolocal") + 
                             i(wifeauth, postmarital_living, ref = 0, ref2 = "neolocal") + 
                             i(postmarital_living, parentsjoin, ref = "neolocal", ref2 = 0) +
                             i(wifeauth, employed, ref = 0, ref2 = 0) +
                             i(parentsjoin, economic_group, ref = 0, ref2 = "low") | pidlink, fsplit = 'inc_stability',
                         vcov = 'cluster', data = subset(do.data_balanced, postmarital_living %in% c("neolocal", "matrilocal")))

model.paritdom.int <- feols(inc ~ i(wifeauth) + i(parentsdom) + i(employed, ref = 0) + i(economic_group, ref = 'Low') +
                               hhsize + i(ruralresident, ref = 0) + i(javabaliresident, ref = 0) + 
                               i(education_level, ref = 'Elementary') + age + agesq + agemar + 
                               i(postmarital_living, ref = "neolocal") + 
                               i(wifeauth, postmarital_living, ref = 0, ref2 = "neolocal") + 
                               i(postmarital_living, parentsdom, ref = "neolocal", ref2 = 0) +
                               i(wifeauth, employed, ref = 0, ref2 = 0) +
                               i(parentsdom, economic_group, ref = 0, ref2 = "low") | pidlink, fsplit = 'inc_stability',
                             vcov = 'cluster', data = subset(do.data_balanced, postmarital_living %in% c("neolocal", "matrilocal")))

etable(model.paritjoin.int, model.paritdom.int, fitstat = ~ n + r2 + wr2 + aic + bic)

# Model 3: In-laws' Influence
model.ilwiant.split <- feols(inc ~ wifeauth + i(patrilocal) + i(inlawsdom) + i(employed, ref = 0) + i(economic_group, ref = 'Low') + hhsize + i(ruralresident, ref = 0) + i(javabaliresident, ref = 0) + i(education_level, ref = 'Elementary') + age + agesq + agemar + i(wifeauth, patrilocal, ref= '0', ref2 = '0') + i(wifeauth, inlawsdom, ref= '0', ref2 = '0') + i(patrilocal, employed, ref = '0', ref2 = '0') + i(inlawsdom, employed, ref = '0', ref2 = '0') + i(inlawsdom, economic_group, ref= '0', ref2 = 'Low')| pidlink, fsplit = 'inc_stability', vcov = 'hetero', data = do.data)
model.ilwianto.split <- feols(inc ~ wifeauth + i(patrilocal) + i(inlawsjoin) + i(employed, ref = 0) + i(economic_group, ref = 'Low') + hhsize + i(ruralresident, ref = 0) + i(javabaliresident, ref = 0) + i(education_level, ref = 'Elementary') + age + agesq + agemar + i(wifeauth, patrilocal, ref= '0', ref2 = '0') + i(wifeauth, inlawsjoin, ref= '0', ref2 = '0') + i(patrilocal, employed, ref = '0', ref2 = '0') + i(inlawsjoin, employed, ref = '0', ref2 = '0') + i(inlawsjoin, economic_group, ref= '0', ref2 = 'Low') | pidlink, fsplit = 'inc_stability', vcov = 'hetero', data = do.data)

etable(model.ilwiant.split, model.ilwianto.split, fitstat = ~ n + r2 + wr2 + aic + bic)

model.ilwitjoin.int <- feols(inc ~ i(wifeauth) + i(inlawsjoin) + i(employed, ref = 0) + i(economic_group, ref = 'Low') +
                               hhsize + i(ruralresident, ref = 0) + i(javabaliresident, ref = 0) + 
                               i(education_level, ref = 'Elementary') + age + agesq + agemar + 
                               i(postmarital_living, ref = "neolocal") + 
                               i(wifeauth, postmarital_living, ref = 0, ref2 = "neolocal") + 
                               i(postmarital_living, employed, ref = "neolocal", ref2 = 0) +
                               i(postmarital_living, inlawsjoin, ref = "neolocal", ref2 = 0) +
                               i(wifeauth, employed, ref = 0, ref2 = 0) +
                               i(inlawsjoin, economic_group, ref = 0, ref2 = "low") | pidlink, fsplit = 'inc_stability',
                               vcov = 'cluster', data = subset(do.data_balanced, postmarital_living %in% c("neolocal", "patrilocal")))

model.ilwitdom.int <- feols(inc ~ i(wifeauth) + i(inlawsdom) + i(employed, ref = 0) + i(economic_group, ref = 'Low') +
                              hhsize + i(ruralresident, ref = 0) + i(javabaliresident, ref = 0) + 
                              i(education_level, ref = 'Elementary') + age + agesq + agemar + 
                              i(postmarital_living, ref = "neolocal") + 
                              i(wifeauth, postmarital_living, ref = 0, ref2 = "neolocal") + 
                              i(postmarital_living, employed, ref = "neolocal", ref2 = 0) +
                              i(postmarital_living, inlawsdom, ref = "neolocal", ref2 = 0) +
                              i(wifeauth, employed, ref = 0, ref2 = 0) +
                              i(inlawsdom, economic_group, ref = 0, ref2 = "low") | pidlink, fsplit = 'inc_stability',
                              vcov = 'cluster', data = subset(do.data_balanced, postmarital_living %in% c("neolocal", "patrilocal")))

etable(model.ilwitjoin.int, model.ilwitdom.int, keep = "wifeauth|matrilocal|employed|economic_group|postmarital|parents|inlaws", fitstat = ~ n + r2 + wr2 + aic + bic)

# Cross-Sectional Analysis for Stable Preference Group
# Subset only individuals with stable fertility preference
# Create subsets for each year
data_1997 <- subset(do.data_balanced, year == 1997)
data_2000 <- subset(do.data_balanced, year == 2000)
data_2007 <- subset(do.data_balanced, year == 2007)
data_2014 <- subset(do.data_balanced, year == 2014)

# Check the number of observations in each subset
table(do.data_balanced$year)

# Load required packages
library(MASS)      # For Ordered Logit
library(ordinal)   # Alternative Ordered Logit

# Function for OLS regression for each year (Only neolocal & matrilocal)
run_ols_parjoin <- function(data, year) {
  data_filtered <- data %>%
    filter(year == year, postmarital_living %in% c("neolocal", "matrilocal")) %>%
    na.omit() %>%
    mutate(
      postmarital_living = relevel(as.factor(postmarital_living), ref = "neolocal"),
      economic_group = relevel(as.factor(economic_group), ref = "Low")
    ) # Set reference category

  # Check if there are enough observations
  if (nrow(data_filtered) == 0) {
    cat("No valid data for", year, "\n")
    return(NULL)
  }
  
# Estimate OLS model
  model <- lm(inc ~ wifeauth + parentsjoin + employed + economic_group +
                hhsize + ruralresident + javabaliresident + education_level +
                age + agesq + agemar + postmarital_living, data = data_filtered)

  # Output regression results
  cat("\nRegression Results for", year, "\n")
  print(summary(model))

  return(model) # Return the model so it can be saved
}

# Run OLS for each year
model_1997 <- run_ols_parjoin(data_1997, 1997)
model_2000 <- run_ols_parjoin(data_2000, 2000)
model_2007 <- run_ols_parjoin(data_2007, 2007)
model_2014 <- run_ols_parjoin(data_2014, 2014)

# Function for OLS regression for each year (Only neolocal & matrilocal)
run_ols_pardom <- function(data, year) {
  data_filtered <- data %>%
    filter(year == year, postmarital_living %in% c("neolocal", "matrilocal")) %>%
    na.omit() %>%
    mutate(
      postmarital_living = relevel(as.factor(postmarital_living), ref = "neolocal"),
      economic_group = relevel(as.factor(economic_group), ref = "Low")
    ) # Set reference category

  # Check if there are enough observations
  if (nrow(data_filtered) == 0) {
    cat("No valid data for", year, "\n")
    return(NULL)
  }
  
# Estimate OLS model
  model <- lm(inc ~ wifeauth + parentsdom + employed + economic_group +
                hhsize + ruralresident + javabaliresident + education_level +
                age + agesq + agemar + postmarital_living, data = data_filtered)

  # Output regression results
  cat("\nRegression Results for", year, "\n")
  print(summary(model))

  return(model) # Return the model so it can be saved
}

# Run OLS for each year
model_1997 <- run_ols_pardom(data_1997, 1997)
model_2000 <- run_ols_pardom(data_2000, 2000)
model_2007 <- run_ols_pardom(data_2007, 2007)
model_2014 <- run_ols_pardom(data_2014, 2014)

# In-Laws
# Function for OLS regression for each year (Only neolocal & patrilocal)
run_ols_ilwjoin <- function(data, year) {
  data_filtered <- data %>%
    filter(year == year, postmarital_living %in% c("neolocal", "patrilocal")) %>%
    na.omit() %>%
    mutate(
      postmarital_living = relevel(as.factor(postmarital_living), ref = "neolocal"),
      economic_group = relevel(as.factor(economic_group), ref = "Low")
    ) # Set reference category

  # Check if there are enough observations
  if (nrow(data_filtered) == 0) {
    cat("No valid data for", year, "\n")
    return(NULL)
  }

  # Estimate OLS model
  model <- lm(inc ~ wifeauth + inlawsjoin + employed + economic_group +
                hhsize + ruralresident + javabaliresident + education_level +
                age + agesq + agemar + postmarital_living, data = data_filtered)

  # Output regression results
  cat("\nRegression Results for", year, "\n")
  print(summary(model))

  return(model) # Return the model so it can be saved
}

# Run OLS for each year
model_1997 <- run_ols_ilwjoin(data_1997, 1997)
model_2000 <- run_ols_ilwjoin(data_2000, 2000)
model_2007 <- run_ols_ilwjoin(data_2007, 2007)
model_2014 <- run_ols_ilwjoin(data_2014, 2014)

# Function for OLS regression for each year (Only neolocal & patrilocal)
run_ols_ilwdom <- function(data, year) {
  data_filtered <- data %>%
    filter(year == year, postmarital_living %in% c("neolocal", "patrilocal")) %>%
    na.omit() %>%
    mutate(
      postmarital_living = relevel(as.factor(postmarital_living), ref = "neolocal"),
      economic_group = relevel(as.factor(economic_group), ref = "Low")
    ) # Set reference category

  # Check if there are enough observations
  if (nrow(data_filtered) == 0) {
    cat("No valid data for", year, "\n")
    return(NULL)
  }

  # Estimate OLS model
  model <- lm(inc ~ wifeauth + inlawsdom + employed + economic_group +
                hhsize + ruralresident + javabaliresident + education_level +
                age + agesq + agemar + postmarital_living, data = data_filtered)

  # Output regression results
  cat("\nRegression Results for", year, "\n")
  print(summary(model))

  return(model) # Return the model so it can be saved
}

# Run OLS for each year
model_1997 <- run_ols_ilwdom(data_1997, 1997)
model_2000 <- run_ols_ilwdom(data_2000, 2000)
model_2007 <- run_ols_ilwdom(data_2007, 2007)
model_2014 <- run_ols_ilwdom(data_2014, 2014)

# Cross Section for Stable Sample
# Function to run OLS only for the stable preference group
run_stable_parjoin <- function(data, year) {
  data_filtered <- data %>%
    filter(year == year, inc_stability == "stable", postmarital_living %in% c("neolocal", "matrilocal")) %>%
    na.omit() %>%
    mutate(
      postmarital_living = relevel(as.factor(postmarital_living), ref = "neolocal"),
      economic_group = relevel(as.factor(economic_group), ref = "Low")
    ) # Set reference category

  # Check if there are enough observations
  if (nrow(data_filtered) == 0) {
    cat("No valid data for", year, "\n")
    return(NULL)
  }

  # Estimate OLS model
  model <- lm(inc ~ wifeauth + parentsjoin + employed + economic_group +
                hhsize + ruralresident + javabaliresident + education_level +
                age + agesq + agemar + postmarital_living, data = data_filtered)

  # Output regression results
  cat("\nRegression Results for", year, "\n")
  print(summary(model))

  return(model) # Return the model so it can be saved
}

# Run OLS for the stable preference group in 1997 and 2014
model_stable_1997 <- run_stable_parjoin(data_1997, 1997)
coeftest(model_stable_1997, vcov = vcovHC(model_stable_1997, type = "HC1"))

model_stable_2000 <- run_stable_parjoin(data_2000, 2000)
coeftest(model_stable_2000, vcov = vcovHC(model_stable_2000, type = "HC1"))

model_stable_2007 <- run_stable_parjoin(data_2007, 2007) #error
model_stable_2014 <- run_stable_parjoin(data_2014, 2014)

# Function to run OLS only for the stable preference group
run_stable_pardom <- function(data, year) {
  data_filtered <- data %>%
    filter(year == year, inc_stability == "stable", postmarital_living %in% c("neolocal", "matrilocal")) %>%
    na.omit() %>%
    mutate(
      postmarital_living = relevel(as.factor(postmarital_living), ref = "neolocal"),
      economic_group = relevel(as.factor(economic_group), ref = "Low")
    ) # Set reference category

  # Check if there are enough observations
  if (nrow(data_filtered) == 0) {
    cat("No valid data for", year, "\n")
    return(NULL)
  }

  # Estimate OLS model
  model <- lm(inc ~ wifeauth + parentsdom + employed + economic_group +
                hhsize + ruralresident + javabaliresident + education_level +
                age + agesq + agemar + postmarital_living, data = data_filtered)

  # Output regression results
  cat("\nRegression Results for", year, "\n")
  print(summary(model))

  return(model) # Return the model so it can be saved
}

# Run OLS for the stable preference group in 1997 and 2014
model_stable_1997 <- run_stable_pardom(data_1997, 1997)
coeftest(model_stable_1997, vcov = vcovHC(model_stable_1997, type = "HC1"))

model_stable_2000 <- run_stable_pardom(data_2000, 2000)
coeftest(model_stable_2000, vcov = vcovHC(model_stable_2000, type = "HC1"))

model_stable_2007 <- run_stable_pardom(data_2007, 2007) #error
model_stable_2014 <- run_stable_pardom(data_2014, 2014) #error

# Function to run OLS only for the stable preference group
run_stable_ilwjoin <- function(data, year) {
  data_filtered <- data %>%
    filter(year == year, inc_stability == "stable", postmarital_living %in% c("neolocal", "patrilocal")) %>%
    na.omit() %>%
    mutate(
      postmarital_living = relevel(as.factor(postmarital_living), ref = "neolocal"),
      economic_group = relevel(as.factor(economic_group), ref = "Low")
    ) # Set reference category

  # Check if there are enough observations
  if (nrow(data_filtered) == 0) {
    cat("No valid data for", year, "\n")
    return(NULL)
  }

  # Estimate OLS model
  model <- lm(inc ~ wifeauth + inlawsjoin + employed + economic_group +
                hhsize + ruralresident + javabaliresident + education_level +
                age + agesq + agemar + postmarital_living, data = data_filtered)

  # Output regression results
  cat("\nRegression Results for", year, "\n")
  print(summary(model))

  return(model) # Return the model so it can be saved
}

# Run OLS for the stable preference group in 1997 and 2014
model_stable_1997 <- run_stable_ilwjoin(data_1997, 1997)
coeftest(model_stable_1997, vcov = vcovHC(model_stable_1997, type = "HC1"))

model_stable_2000 <- run_stable_ilwjoin(data_2000, 2000)
coeftest(model_stable_2000, vcov = vcovHC(model_stable_2000, type = "HC1"))

model_stable_2007 <- run_stable_ilwjoin(data_2007, 2007) # error
model_stable_2014 <- run_stable_ilwjoin(data_2014, 2014) # error

# Function to run OLS only for the stable preference group
run_stable_ilwdom <- function(data, year) {
  data_filtered <- data %>%
    filter(year == year, inc_stability == "stable", postmarital_living %in% c("neolocal", "patrilocal")) %>%
    na.omit() %>%
    mutate(
      postmarital_living = relevel(as.factor(postmarital_living), ref = "neolocal"),
      economic_group = relevel(as.factor(economic_group), ref = "Low")
    ) # Set reference category

  # Check if there are enough observations
  if (nrow(data_filtered) == 0) {
    cat("No valid data for", year, "\n")
    return(NULL)
  }

  # Estimate OLS model
  model <- lm(inc ~ wifeauth + inlawsdom + employed + economic_group +
                hhsize + ruralresident + javabaliresident + education_level +
                age + agesq + agemar + postmarital_living, data = data_filtered)

  # Output regression results
  cat("\nRegression Results for", year, "\n")
  print(summary(model))

  return(model) # Return the model so it can be saved
}

# Run OLS for the stable preference group in 1997 and 2014
model_stable_1997 <- run_stable_ilwdom(data_1997, 1997)
coeftest(model_stable_1997, vcov = vcovHC(model_stable_1997, type = "HC1"))

model_stable_2000 <- run_stable_ilwdom(data_2000, 2000)
coeftest(model_stable_2000, vcov = vcovHC(model_stable_2000, type = "HC1"))

model_stable_2007 <- run_stable_ilwdom(data_2007, 2007) # error
model_stable_2014 <- run_stable_ilwdom(data_2014, 2014) # error

##### END #####
