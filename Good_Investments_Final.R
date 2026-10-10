# Load and install necessary libraries

install.packages(c("foreign", "tidyverse", "modeest", "dplyr", "plotrix", "moments", "car", "ggplot2", "tidyr", "patchwork", "ggthemes"))

library(foreign)
library(tidyverse)  # includes dplyr
library(modeest)
library(plotrix)
library(moments)
library(car)
library(ggplot2)
library(tidyr)
library(patchwork)
library(ggthemes)

# Function to load and clean data
load_and_clean_data <- function(file_path) {
  df <- read.csv(file_path, stringsAsFactors = FALSE)
  colnames(df) <- gsub("^X", "", colnames(df))
  if (!"Country_Name" %in% colnames(df)) {
    colnames(df)[1] <- "Country_Name"
  }
  return(df)
}

# Load datasets
df_growth <- load_and_clean_data("GDP_Growth_Data.csv")
df_investment <- load_and_clean_data("Investment_Data.csv")

# Ensure correct column ordering
df_growth <- df_growth %>% relocate(Country_Name, Region, IncomeGroup, .before = everything())
df_investment <- df_investment %>% relocate(Country_Name, .before = everything())

# This will filter out all entries that dont have a Region assigned, meaning we will be left with countries only
df_growth_countries <- df_growth %>%
  arrange(desc(Region)) %>%  
  slice_head(n = 214)

# Include "World" data, this will make our job much easier

df_growth_countries <- rbind(df_growth_countries, df_growth[256, ])

# Give an income group to Venezuela

df_growth_countries[120, "IncomeGroup"] <- "Upper middle income"

# No filtering needed for df_investment since it doesn't have Region or IncomeGroup columns

# Function to reshape data from wide to long format
reshape_long <- function(df, value_name) {
  # Define columns to exclude from pivoting
  cols_to_exclude <- c("Country_Name", "Region", "IncomeGroup")
  
  # Use `any_of()` to safely select columns that exist
  df %>%
    pivot_longer(
      cols = -any_of(cols_to_exclude),  # Exclude only the columns that exist
      names_to = "Year",
      values_to = value_name
    ) %>%
    mutate(Year = as.numeric(Year))
}

# Reshape datasets
df_growth_long <- reshape_long(df_growth_countries, "GDP_Growth")
df_investment_long <- reshape_long(df_investment, "Investment_Rate")

# Merge datasets
df_merged <- df_growth_long %>%
  select(Country_Name, Year, Region, IncomeGroup, GDP_Growth) %>%
  inner_join(df_investment_long %>% select(Country_Name, Year, Investment_Rate), 
             by = c("Country_Name", "Year")) %>%
  filter(!is.na(GDP_Growth) & !is.na(Investment_Rate))


# Calculate Effective Investment rates
df_merged <- df_merged %>%
  group_by(Country_Name) %>%
  mutate(
    # Calculate effective rates directly
    Effective_Investment_Rate_Lag1 = GDP_Growth / lag(Investment_Rate, 1),
    Effective_Investment_Rate_Lag2 = GDP_Growth / lag(Investment_Rate, 2),
    Effective_Investment_Rate_Lag3 = GDP_Growth / lag(Investment_Rate, 3)
  ) %>%
  # Replace Inf/-Inf with NA
  mutate(across(
    starts_with("Effective_Investment_Rate"),
    ~ ifelse(is.finite(.), ., NA)
  )) %>%
  # Remove rows where all effective rate columns are NA
  filter(Year >= 1992 & !if_all(starts_with("Effective_Investment_Rate"), is.na)) %>%
  ungroup()  # Remove grouping for further operations

# Add lagged investment rate (3-year lag)
df_merged <- df_merged %>%
  group_by(Country_Name) %>%
  mutate(Investment_Rate_Lag3 = lag(Investment_Rate, 3)) %>%
  ungroup()


### Regions : 

# Calculate country-level means (average over all years) 
calculate_country_means <- function(df) {
  df %>%
    group_by(Region, Country_Name) %>%
    summarize(
      Country_Mean_Rate = mean(Effective_Investment_Rate_Lag3, na.rm = TRUE),
      .groups = "drop"
    )
}

# Define regions
regions <- c(
  "Latin America & Caribbean",
  "East Asia & Pacific", 
  "Europe & Central Asia", 
  "Sub-Saharan Africa", 
  "South Asia", 
  "Middle East & North Africa",
  "North America"
)

# Generate country-level data 
country_data <- calculate_country_means(df_merged)

plot_region_histogram_country <- function(df, region_name, trim_fraction = 0.05) {
  # Calculate trimmed values
  plot_data <- df %>%
    filter(Region == region_name) %>%
    mutate(
      Trimmed_Rate = case_when(
        Country_Mean_Rate < quantile(Country_Mean_Rate, trim_fraction, na.rm = TRUE) ~ 
          quantile(Country_Mean_Rate, trim_fraction, na.rm = TRUE),
        Country_Mean_Rate > quantile(Country_Mean_Rate, 1 - trim_fraction, na.rm = TRUE) ~ 
          quantile(Country_Mean_Rate, 1 - trim_fraction, na.rm = TRUE),
        TRUE ~ Country_Mean_Rate
      ) 
    )
  
  # Create plot (using Trimmed_Rate directly)
  ggplot(plot_data, aes(x = Trimmed_Rate)) +  # <- Changed to Trimmed_Rate
    geom_histogram(aes(y = ..density..), binwidth = 0.028,  # Adjust binwidth as needed
                   fill = "lightblue", alpha = 0.8, color = "white") +
    geom_density(color = "red", linewidth = 1.2) +
    labs(
      title = paste("Country Averages (1992–2023) in", region_name),
      subtitle = paste("Trimmed top/bottom", trim_fraction * 100, "% extreme values"),
      x = "Effective Investment Rate (Trimmed)",  # Updated axis label
      y = "Density"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 11, face = "bold"),
      panel.grid.minor = element_blank()
    )
}

# Generate plots
plot_list <- list()
for (i in seq_along(regions)) {
  region_name <- regions[i]
  plot_list[[i]] <- plot_region_histogram_country(country_data, region_name, trim_fraction = 0.05)
}

# Combine plots (update title to reflect change)
wrap_plots(plot_list, ncol = 2) +
  plot_annotation(
    title = "Country-Level Effective Investment Rates by Region (Raw Values)",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 14))
  )




### INCOME GROUPS:

# Define income groups
income_levels <- c("Low income", "Lower middle income", 
                   "Upper middle income", "High income")

# Generate country-level data with IncomeGroup 
country_data_income <- df_merged %>%
  group_by(IncomeGroup, Country_Name) %>%
  summarize(
    Country_Mean_Rate = mean(Effective_Investment_Rate_Lag3, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(IncomeGroup %in% income_levels)  # Keep only the specified income levels

# We use nearly the same function as the one we used for regions
plot_income_histogram_country <- function(df, income_name, trim_fraction = 0.05) {
  # Calculate trimmed values
  plot_data <- df %>%
    filter(IncomeGroup == income_name) %>%
    mutate(
      Trimmed_Rate = case_when(
        Country_Mean_Rate < quantile(Country_Mean_Rate, trim_fraction, na.rm = TRUE) ~ 
          quantile(Country_Mean_Rate, trim_fraction, na.rm = TRUE),
        Country_Mean_Rate > quantile(Country_Mean_Rate, 1 - trim_fraction, na.rm = TRUE) ~ 
          quantile(Country_Mean_Rate, 1 - trim_fraction, na.rm = TRUE),
        TRUE ~ Country_Mean_Rate
      )
    )
  
  # Create plot
  ggplot(plot_data, aes(x = Trimmed_Rate)) +
    geom_histogram(aes(y = ..density..), binwidth = 0.028,
                   fill = "lightblue", alpha = 0.8, color = "white") +
    geom_density(color = "red", linewidth = 1.2) +
    labs(
      title = paste("Country Averages (1992–2023):", income_name),
      subtitle = paste("Trimmed top/bottom", trim_fraction * 100, "% extreme values"),
      x = "Effective Investment Rate (Trimmed)",
      y = "Density"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 11, face = "bold"),
      panel.grid.minor = element_blank()
    )
}

# Generate income-level plots
income_plot_list <- list()
for (i in seq_along(income_levels)) {
  income_name <- income_levels[i]
  income_plot_list[[i]] <- plot_income_histogram_country(country_data_income, income_name, trim_fraction = 0.05)
}

# Combine income-level plots
wrap_plots(income_plot_list, ncol = 2) +
  plot_annotation(
    title = "Country-Level Effective Investment Rates by Income Group",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 14))
  )




### BOX PLOTS:

# Boxplot function for income groups
plot_income_boxplot <- function(df, trim_fraction = 0.05) {
  df %>%
    mutate(
      Trimmed_Rate = case_when(
        Country_Mean_Rate < quantile(Country_Mean_Rate, trim_fraction, na.rm = TRUE) ~ 
          quantile(Country_Mean_Rate, trim_fraction, na.rm = TRUE),
        Country_Mean_Rate > quantile(Country_Mean_Rate, 1 - trim_fraction, na.rm = TRUE) ~ 
          quantile(Country_Mean_Rate, 1 - trim_fraction, na.rm = TRUE),
        TRUE ~ Country_Mean_Rate
      )
    ) %>%
    ggplot(aes(x = IncomeGroup, y = Trimmed_Rate, fill = IncomeGroup)) +
    geom_boxplot(alpha = 0.8, outlier.shape = NA) +
    stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +  # Add mean markers
    labs(
      title = "Effective Investment Rates by Income Group (Trimmed)",
      x = NULL,
      y = "Effective Investment Rate"
    ) +
    scale_fill_brewer(palette = "Set2") +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 15, hjust = 1))
}

# Boxplot function for regions
plot_region_boxplot <- function(df, trim_fraction = 0.05) {
  df %>%
    mutate(
      Trimmed_Rate = case_when(
        Country_Mean_Rate < quantile(Country_Mean_Rate, trim_fraction, na.rm = TRUE) ~ 
          quantile(Country_Mean_Rate, trim_fraction, na.rm = TRUE),
        Country_Mean_Rate > quantile(Country_Mean_Rate, 1 - trim_fraction, na.rm = TRUE) ~ 
          quantile(Country_Mean_Rate, 1 - trim_fraction, na.rm = TRUE),
        TRUE ~ Country_Mean_Rate
      )
    ) %>%
    ggplot(aes(x = reorder(Region, Trimmed_Rate, FUN = median), y = Trimmed_Rate, fill = Region)) +
    geom_boxplot(alpha = 0.8, outlier.shape = NA) +
    stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +  # Add mean markers
    labs(
      title = "Effective Investment Rates by Region (Trimmed)",
      x = NULL,
      y = "Effective Investment Rate"
    ) +
    scale_fill_viridis_d(option = "plasma") +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 15, hjust = 1))
}


(plot_income_boxplot(country_data_income) | plot_region_boxplot(country_data)) +
  plot_annotation(title = "Comparison Across Income Groups and Regions",
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 14)))




### Statistical Summary

# First separate World data
country_sum <- df_merged %>% filter(Region != "World")

# Statistical summary function
get_summary_stats <- function(df, group_var) {
  df %>%
    group_by({{group_var}}) %>%
    summarise(
      # GDP Growth stats
      GDP_Growth_mean = mean(GDP_Growth, na.rm = TRUE),
      GDP_Growth_var = var(GDP_Growth, na.rm = TRUE),
      GDP_Growth_sd = sd(GDP_Growth, na.rm = TRUE),
      
      # Investment Rate stats
      Investment_Rate_mean = mean(Investment_Rate, na.rm = TRUE),
      Investment_Rate_var = var(Investment_Rate, na.rm = TRUE),
      Investment_Rate_sd = sd(Investment_Rate, na.rm = TRUE),
      
      # Effective Investment Rate (3-year lag)
      Effective_Rate_Lag3_mean = mean(Effective_Investment_Rate_Lag3, na.rm = TRUE),
      Effective_Rate_Lag3_var = var(Effective_Investment_Rate_Lag3, na.rm = TRUE),
      Effective_Rate_Lag3_sd = sd(Effective_Investment_Rate_Lag3, na.rm = TRUE),
      
      # Counts
      n_countries = n_distinct(Country_Name),
      n_observations = n(),
      .groups = 'drop'
    ) %>%
    arrange(desc(Effective_Rate_Lag3_mean))
}

# 3. Generate summaries
region_summary <- get_summary_stats(country_sum, Region)  
income_summary <- get_summary_stats(country_sum, IncomeGroup) %>%
  filter(IncomeGroup %in% c("Low income", "Lower middle income", 
                            "Upper middle income", "High income"))

region_summary[6, "Region"] <- "World" 




### ALL COUNTRIES

country_summary <- df_merged %>%
  select(-Year) %>%  # Remove Year column
  group_by(Country_Name) %>%
  summarize(
    across(where(is.numeric), list(
      mean = ~ mean(.x, na.rm = TRUE),
      sd = ~ sd(.x, na.rm = TRUE),
      var = ~ var(.x, na.rm = TRUE)
    )),
    .groups = "drop"
  )



# Visualization : 

no_world <- country_summary %>%
  filter(Country_Name != "World")

# Histogram
ggplot(no_world, aes(x = Effective_Investment_Rate_Lag3_mean)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.03, fill = "lightblue", alpha = 0.8, color = "white") +
  geom_density(color = "red", linewidth = 1.2) +
  labs(
    title = "Histogram of Effective Investment Rate (Lag 3) Means",
    x = "Effective Investment Rate (Lag 3) Mean",
    y = "Density"
  ) +
  theme_minimal()

# Boxplot
ggplot(no_world, aes(x = "", y = Effective_Investment_Rate_Lag3_mean)) +
  geom_boxplot(fill = "lightblue", alpha = 0.8, outlier.color = "red") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black") +
  labs(
    title = "Boxplot of Effective Investment Rate (Lag 3) Means",
    x = "",
    y = "Effective Investment Rate (Lag 3) Mean"
  ) +
  coord_cartesian(ylim = c(-0.3, 0.35))
  theme_minimal()

  
  
### Hypothesis Testing 

  
# Shapiro-Wilk Test for Normality (excluding extreme values)

# Remove extreme values using Tukey's fences (1.5*IQR)
no_world_clean <- no_world %>%
  mutate(
    lower_bound = quantile(Effective_Investment_Rate_Lag3_mean, 0.25, na.rm = TRUE) - 
      1.5 * IQR(Effective_Investment_Rate_Lag3_mean, na.rm = TRUE),
    upper_bound = quantile(Effective_Investment_Rate_Lag3_mean, 0.75, na.rm = TRUE) + 
      1.5 * IQR(Effective_Investment_Rate_Lag3_mean, na.rm = TRUE)
  ) %>%
  filter(
    Effective_Investment_Rate_Lag3_mean >= lower_bound &
      Effective_Investment_Rate_Lag3_mean <= upper_bound
  ) %>%
  select(-lower_bound, -upper_bound)

# Check sample size after removing extremes
n_remaining <- sum(!is.na(no_world_clean$Effective_Investment_Rate_Lag3_mean))

# Perform Shapiro-Wilk test if sample size is appropriate
shapiro_result <- shapiro.test(no_world_clean$Effective_Investment_Rate_Lag3_mean)

# Interpretation
if (shapiro_result$p.value < 0.05) {
  cat("Interpretation: Data significantly deviates from normality (p < 0.05)\n")
} else {
  cat("Interpretation: Data does not significantly deviate from normality\n")
}
  
#multivariable model
### ENHANCED MULTIVARIATE ANALYSIS

# First, ensure we have all needed variables in our dataset
df_multivariate <- df_merged %>%
  # Create meaningful predictors
  mutate(
    # Economic structure variables
    Investment_Share_GDP = Investment_Rate / 100,  # Convert percentage to proportion
    Human_Capital = log(GDP_Growth + 1 - min(GDP_Growth, na.rm = TRUE)),  # Proxy measure
    # Temporal effects
    Post_2000 = as.numeric(Year >= 2000),
    # Interaction terms
    Investment_Income_Interaction = Investment_Rate * as.numeric(IncomeGroup == "High income")
  ) %>%
  # Group by country to create country-level averages
  group_by(Country_Name) %>%
  mutate(
    Avg_Investment = mean(Investment_Rate, na.rm = TRUE),
    Investment_Stability = sd(Investment_Rate, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  # Filter complete cases
  filter(complete.cases(GDP_Growth, Investment_Rate, Region, IncomeGroup))

# Convert categorical variables to factors
df_multivariate$IncomeGroup <- factor(df_multivariate$IncomeGroup,
                                      levels = c("Low income", "Lower middle income",
                                                 "Upper middle income", "High income"))
df_multivariate$Region <- factor(df_multivariate$Region)

# True multivariate model specification
multivariate_model <- lm(
  GDP_Growth ~ 
    # Core relationship of interest
    Investment_Rate_Lag3 +
    
    # Economic controls
    Avg_Investment +
    Investment_Stability +
    Human_Capital +
    
    # Structural controls
    IncomeGroup +
    Region +
    
    # Temporal controls
    Post_2000 +
    Year +
    
    # Interaction effects
    Investment_Income_Interaction,
  
  data = df_multivariate
)

# Display comprehensive results
summary(multivariate_model)
# Before plotting, adjust the graphics device size
par(mar = c(4, 4, 2, 1))  # Reduce margin sizes
par(mfrow = c(2, 2))      # Set 2x2 plot layout
plot(multivariate_model)   # Now plot
# Check model assumptions
par(mfrow = c(2, 2))
plot(multivariate_model)
par(mfrow = c(1, 1))

# Alternative specification with plm (panel data)
if (!require(plm)) install.packages("plm"); library(plm)

panel_model <- plm(
  GDP_Growth ~ 
    Investment_Rate_Lag3 +
    lag(GDP_Growth, 1) +  # Include lagged dependent variable
    Avg_Investment +
    Investment_Stability +
    IncomeGroup +
    Year,
  data = df_multivariate,
  index = c("Country_Name", "Year"),
  model = "within"  # Fixed effects model
)

summary(panel_model)

# Marginal effects visualization
if (!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
if (!require(ggeffects)) install.packages("ggeffects"); library(ggeffects)

# Plot predicted effects
ggpredict(multivariate_model, terms = c("Investment_Rate_Lag3", "IncomeGroup")) %>%
  plot() +
  labs(title = "Predicted GDP Growth by Investment Rate and Income Group",
       x = "3-Year Lagged Investment Rate",
       y = "Predicted GDP Growth",
       color = "Income Group") +
  theme_minimal()

####for regions
# Load required packages
if (!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)
if (!require(lmtest)) install.packages("lmtest"); library(lmtest)
if (!require(plm)) install.packages("plm"); library(plm)
if (!require(ggeffects)) install.packages("ggeffects"); library(ggeffects)
if (!require(viridis)) install.packages("viridis"); library(viridis)

# 1. Prepare data with regional focus
df_regional <- df_merged %>%
  filter(!is.na(Effective_Investment_Rate_Lag3) & !is.na(Region)) %>%
  group_by(Country_Name) %>%
  mutate(
    Avg_Investment = mean(Investment_Rate, na.rm = TRUE),
    Investment_Stability = sd(Investment_Rate, na.rm = TRUE),
    Human_Capital = log(GDP_Growth + 1 - min(GDP_Growth, na.rm = TRUE))  # Proxy
  ) %>%
  ungroup() %>%
  mutate(
    Post_2000 = as.numeric(Year >= 2000),
    Region = factor(Region),
    IncomeGroup = factor(IncomeGroup)
  ) %>%
  filter(complete.cases(.))

# 2. Run multivariate model with region interactions
region_model <- lm(
  GDP_Growth ~ 
    Investment_Rate_Lag3 * Region +  # Interaction allows different slopes per region
    Avg_Investment +
    Investment_Stability +
    Human_Capital +
    IncomeGroup +                   # Control for income levels
    Post_2000 +
    Year,
  data = df_regional
)

# 3. Display results
summary(region_model)

# 4. Check model assumptions
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
plot(region_model)
par(mfrow = c(1, 1))

# 5. Panel data version (fixed effects)
panel_region_model <- plm(
  GDP_Growth ~ Investment_Rate_Lag3 * Region + lag(GDP_Growth, 1),
  data = df_regional,
  index = c("Country_Name", "Year"),
  model = "within"
)

summary(panel_region_model)

# 6. Visualize regional marginal effects
region_plot <- ggpredict(region_model, 
                         terms = c("Investment_Rate_Lag3 [20,40,60]", "Region")) %>%
  plot() +
  labs(
    title = "GDP Growth Response to Investment by World Region",
    x = "3-Year Lagged Investment Rate (% of GDP)",
    y = "Predicted GDP Growth (%)",
    color = "Region"
  ) +
  theme_minimal(base_size = 12) +
  scale_color_viridis_d() +
  theme(legend.position = "bottom")

print(region_plot)

# 7. Test specific regional comparisons
if (!require(multcomp)) install.packages("multcomp"); library(multcomp)

# Example: Compare East Asia vs. Latin America
comparisons <- glht(
  region_model,
  linfct = c(
    "Investment_Rate_Lag3 + Investment_Rate_Lag3:RegionEast Asia & Pacific = 0",
    "Investment_Rate_Lag3 + Investment_Rate_Lag3:RegionLatin America & Caribbean = 0"
  )
)
summary(comparisons)

# 8. Save key results
results_list <- list(
  model_summary = summary(region_model),
  panel_summary = summary(panel_region_model),
  region_comparisons = summary(comparisons),
  plot = region_plot
)

# 9. Export plot
ggsave("investment_regional_effects.png", plot = region_plot, 
       width = 10, height = 6, dpi = 300)

# Hypothesis Testing Section (Corrected)


# 1. Correlation Tests (Fixed)
# --------------------------

# Function with error handling for correlations (minimum 3 observations)
safe_cor <- function(x, y) {
  complete_cases <- complete.cases(x, y)
  if (sum(complete_cases) < 3) {  # Need at least 3 observations
    return(list(estimate = NA, p.value = NA))
  }
  cor_test <- cor.test(x[complete_cases], y[complete_cases])
  list(estimate = cor_test$estimate, p.value = cor_test$p.value)
}

# --- COUNTRY-LEVEL CORRELATIONS ---
country_correlations <- df_merged %>%
  group_by(Country_Name) %>%
  filter(sum(complete.cases(Investment_Rate_Lag3, GDP_Growth)) >= 10) %>%
  summarise(
    corr_coef = safe_cor(Investment_Rate_Lag3, GDP_Growth)$estimate,
    p_value = safe_cor(Investment_Rate_Lag3, GDP_Growth)$p.value,
    n_obs = sum(complete.cases(Investment_Rate_Lag3, GDP_Growth)),
    .groups = 'drop'
  ) %>%
  mutate(
    correlation_status = case_when(
      is.na(p_value) ~ "Insufficient data",
      abs(corr_coef) >= 0.7 & p_value < 0.05 ~ "Strong correlation (p < 0.05)",
      abs(corr_coef) >= 0.4 & p_value < 0.05 ~ "Moderate correlation (p < 0.05)",
      abs(corr_coef) >= 0.2 & p_value < 0.05 ~ "Weak correlation (p < 0.05)",
      p_value < 0.1 ~ "Very weak correlation (p < 0.1)",
      TRUE ~ "Not significantly correlated"
    ),
    direction = case_when(
      corr_coef > 0 ~ "Positive",
      corr_coef < 0 ~ "Negative",
      TRUE ~ "No correlation"
    )
  )

# Add country-level results to no_world
no_world <- no_world %>%
  left_join(country_correlations, by = "Country_Name")

# --- REGION-LEVEL CORRELATIONS ---
region_correlations <- df_merged %>%
  group_by(Region) %>%
  filter(sum(complete.cases(Investment_Rate_Lag3, GDP_Growth)) >= 3) %>%
  summarise(
    corr_coef = safe_cor(Investment_Rate_Lag3, GDP_Growth)$estimate,
    p_value = safe_cor(Investment_Rate_Lag3, GDP_Growth)$p.value,
    n_obs = sum(complete.cases(Investment_Rate_Lag3, GDP_Growth)),
    .groups = 'drop'
  ) %>%
  mutate(
    correlation_status = case_when(
      is.na(p_value) ~ "Insufficient data",
      abs(corr_coef) >= 0.7 & p_value < 0.05 ~ "Strong correlation (p < 0.05)",
      abs(corr_coef) >= 0.4 & p_value < 0.05 ~ "Moderate correlation (p < 0.05)",
      abs(corr_coef) >= 0.2 & p_value < 0.05 ~ "Weak correlation (p < 0.05)",
      p_value < 0.1 ~ "Very weak correlation (p < 0.1)",
      TRUE ~ "Not significantly correlated"
    ),
    direction = case_when(
      corr_coef > 0 ~ "Positive",
      corr_coef < 0 ~ "Negative",
      TRUE ~ "No correlation"
    )
  )

# Add region-level results to region_summary
region_summary <- region_summary %>%
  left_join(region_correlations, by = "Region")

# --- INCOME GROUP-LEVEL CORRELATIONS ---
income_correlations <- df_merged %>%
  filter(IncomeGroup %in% c("Low income", "Lower middle income", 
                            "Upper middle income", "High income")) %>%
  group_by(IncomeGroup) %>%
  filter(sum(complete.cases(Investment_Rate_Lag3, GDP_Growth)) >= 3) %>%
  summarise(
    corr_coef = safe_cor(Investment_Rate_Lag3, GDP_Growth)$estimate,
    p_value = safe_cor(Investment_Rate_Lag3, GDP_Growth)$p.value,
    n_obs = sum(complete.cases(Investment_Rate_Lag3, GDP_Growth)),
    .groups = 'drop'
  ) %>%
  mutate(
    correlation_status = case_when(
      is.na(p_value) ~ "Insufficient data",
      abs(corr_coef) >= 0.7 & p_value < 0.05 ~ "Strong correlation (p < 0.05)",
      abs(corr_coef) >= 0.4 & p_value < 0.05 ~ "Moderate correlation (p < 0.05)",
      abs(corr_coef) >= 0.2 & p_value < 0.05 ~ "Weak correlation (p < 0.05)",
      p_value < 0.1 ~ "Very weak correlation (p < 0.1)",
      TRUE ~ "Not significantly correlated"
    ),
    direction = case_when(
      corr_coef > 0 ~ "Positive",
      corr_coef < 0 ~ "Negative",
      TRUE ~ "No correlation"
    )
  )

# Add income-group results to income_summary
income_summary <- income_summary %>%
  left_join(income_correlations, by = "IncomeGroup")


# 2. Linear Regression Models (Unchanged)

# ENHANCED REGRESSION ANALYSIS WITH ROBUST INTERPRETATIONS
# Modified helper function with effect size metrics and diagnostics
# Corrected helper function with proper beta calculation
safe_lm_stats <- function(x, y) {
  ok <- complete.cases(x, y)
  if (sum(ok) < 5) {
    return(list(
      estimate = NA, p_value = NA, adj_r2 = NA, 
      ci_lower = NA, ci_upper = NA, f_stat = NA, beta = NA
    ))
  }
  
  m <- lm(y[ok] ~ x[ok])
  s <- summary(m)
  ci <- confint(m)[2, ]
  
  # Calculate standardized beta coefficient
  beta <- coef(m)[2] * (sd(x[ok], na.rm = TRUE) / sd(y[ok], na.rm = TRUE))
  
  list(
    estimate = coef(s)[2, "Estimate"],
    beta = unname(beta),
    p_value = coef(s)[2, "Pr(>|t|)"],
    adj_r2 = s$adj.r.squared,
    ci_lower = ci[1],
    ci_upper = ci[2],
    f_stat = s$fstatistic[1]
  )
}


# Unified regression function with improved interpretation
run_regression <- function(data, group_var) {
  data %>%
    group_by({{group_var}}) %>%
    filter(n() >= 10) %>%  # More conservative N filter
    reframe(
      stats = list(safe_lm_stats(Investment_Rate_Lag3, GDP_Growth)),
      n_obs = sum(complete.cases(Investment_Rate_Lag3, GDP_Growth))
    ) %>%
    mutate(
      estimate = map_dbl(stats, "estimate"),
      beta = map_dbl(stats, "beta"),
      p_value = map_dbl(stats, "p_value"),
      adj_r2 = map_dbl(stats, "adj_r2"),
      ci_lower = map_dbl(stats, "ci_lower"),
      ci_upper = map_dbl(stats, "ci_upper"),
      interpretation = case_when(
        is.na(p_value) ~ 'Insufficient data',
        p_value >= 0.05 ~ 'Not significant',
        adj_r2 < 0.01 ~ paste0('Significant but negligible (R²=', round(adj_r2,3), ')'),
        abs(beta) < 0.1 ~ 'Significant but trivial effect',
        abs(beta) < 0.3 ~ 'Significant with modest effect',
        TRUE ~ 'Significant with notable effect'
      ),
      practical_significance = if_else(
        abs(ci_lower) > 0.1 & abs(ci_upper) > 0.1,
        "Practically meaningful", 
        "Caution: Precision limited"
      )
    ) %>%
    select(-stats)
}

# --- COUNTRY-LEVEL ANALYSIS ---
country_reg <- run_regression(df_merged, Country_Name)

no_world <- no_world %>%
  # Remove old regression columns (if they exist)
  select(-any_of(c("p_value", "adj_r2", "interpretation", "practical_significance"))) %>%
  # Join with new results (matching by country name)
  left_join(
    country_reg %>% 
      select(Country_Name, p_value, adj_r2, interpretation, practical_significance),
    by = c("Country_Name" = "Country_Name")  # Adjust join key if needed
  )

# --- REGION-LEVEL ANALYSIS --- 
region_reg <- run_regression(df_merged, Region)

region_summary <- region_summary %>%
  select(-any_of(c("p_value", "adj_r2", "interpretation", "practical_significance"))) %>%
  left_join(
    region_reg %>% 
      select(Region, p_value, adj_r2, interpretation, practical_significance),
    by = c("Region" = "Region")  # Adjust join key if needed
  )

# --- INCOME GROUP ANALYSIS ---
income_reg <- df_merged %>%
  filter(IncomeGroup %in% income_levels) %>%
  run_regression(IncomeGroup)

income_summary <- income_summary %>%
  select(-any_of(c("p_value", "adj_r2", "interpretation", "practical_significance"))) %>%
  left_join(
    income_reg %>% 
      select(IncomeGroup, p_value, adj_r2, interpretation, practical_significance),
    by = c("IncomeGroup" = "IncomeGroup")  # Adjust join key if needed
  )


write_csv(no_world, "no_world.csv")
write_csv(region_summary, "region_summary.csv")
write_csv(income_summary, "income_summary.csv")

### T-TEST: Is there a significant difference in Effective Investment Rate (Lag 3) between High and Low income countries?

# Filter only High and Low income groups
high_vs_low <- df_merged %>%
  filter(IncomeGroup %in% c("High income", "Low income"))

# T-test: two-sample assuming unequal variance (Welch's t-test)
t_test_result <- t.test(
  Effective_Investment_Rate_Lag3 ~ IncomeGroup,
  data = high_vs_low,
  var.equal = FALSE
)
print(t_test_result)

# H0: There is no difference in mean Effective Investment Rate (Lag 3) between High and Low income countries
# H1: There is a difference

#Chi-square test
# Binarize Investment Rate based on median
df_chi <- df_merged %>%
  mutate(
    Investment_Category = ifelse(Investment_Rate >= median(Investment_Rate, na.rm = TRUE), "High", "Low")
  )

# Create contingency table: Investment vs Income Group
table_chi <- table(df_chi$Investment_Category, df_chi$IncomeGroup)
print(table_chi)

# Run Chi-square test
chi_test_result <- chisq.test(table_chi)
print(chi_test_result)

# H0: Investment rate and income group are independent
# H1: They are associated (dependent)