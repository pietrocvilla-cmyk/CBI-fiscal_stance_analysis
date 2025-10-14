# ============================================================
# Analysis of links between fiscal stance and CBI 
# Pietro Villa 
# Bocconi University 
# Bachelor in Economic and Social Sciences 
# Date: 01-04-2025 
# Description: 
# This script performs the empirical work for a bachelor's thesis on 
# Government bond yields - as fiscal stance proxy - and CBI .
# ============================================================

######## Set the main project directory

# Use simple relative path from project root
file_path <- "data/Original Dataset - bond yield.xlsx"

# Verify we're in the right place
if (!file.exists(file_path)) {
  stop("\n========================================\n",
       "ERROR: Cannot find data file!\n",
       "========================================\n",
       "Expected file: data/Dataset finale - bond yield.xlsx\n",
       "Current working directory: ", getwd(), "\n\n",
       "SOLUTION:\n",
       "1. Close this script\n",
       "2. In your file explorer, find: CBI-fiscal_stance_analysis.Rproj\n",
       "3. Double-click the .Rproj file to open the project\n",
       "4. Then open and run this script\n",
       "========================================\n")
}

cat("✓ Working directory:", getwd(), "\n")
cat("✓ Data file found:", file_path, "\n\n")

dataset <- read_excel(file_path)

# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(openxlsx)
library(plm)
library(car)
library(zoo)
library(ggplot2)
library(lmtest)
library(sandwich)
library(car)


#---------------------- Eliminating countries which are not in all datasets -----------------------------
##There are important differences in the number of countries considered for each 
#variable. Here, we reduce every dataset (sheet) to a common list of countries

# List all sheet names
sheets <- excel_sheets(file_path)
print(sheets)

# Read all sheets into a list of data frames
data_list <- lapply(sheets, function(sheet) {
  read_excel(file_path, sheet = sheet)
})

# Extract unique country lists from each sheet
country_lists <- lapply(data_list, function(df) unique(df$country))

# Find the common countries across all sheets
common_countries <- Reduce(intersect, country_lists)

# Filter each dataset to keep only common countries
filtered_data_list <- lapply(data_list, function(df) {
  df %>% filter(country %in% common_countries)
})

# Load the original Excel file
wb <- loadWorkbook(file_path)

# Overwrite each sheet with the filtered data
for (i in seq_along(sheets)) {
  sheet_name <- sheets[i]
  
  # Remove existing sheet (if exists)
  if (sheet_name %in% names(wb)) {
    removeWorksheet(wb, sheet_name)
  }
  
  # Add new filtered sheet
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet = sheet_name, filtered_data_list[[i]])
}

# Save the updated Excel file
saveWorkbook(wb, file_path, overwrite = TRUE)

# Print completion message
print("All sheets updated. Only countries present in all sheets have been retained.")

#Add a sheet with the complete list of countries considered 
addWorksheet(wb, "Common Countries")
writeData(wb, sheet = "Common Countries", common_countries)

# Save the updated Excel file
saveWorkbook(wb, file_path, overwrite = TRUE)

#------------------------ Adjust dataset in R regression format and merge sheets in a unique dataset -----------
##In this chunk, we transform all the sheets in our dataset in long format to prepare it for 
#regression; then we merge all our sheets in one. 

# Define the relevant sheets
relevant_sheets <- c("Transformed_CBI_Data", "bond yield", "GDP growth", "Political stability")

# Function to reshape wide format to long format
reshape_long <- function(sheet_name) {
  df <- read_excel(file_path, sheet = sheet_name)
  
  # Ensure first column is "country"
  colnames(df)[1] <- "country"
  
  # Convert column names (years) to numeric
  colnames(df)[-1] <- suppressWarnings(as.numeric(colnames(df)[-1]))
  
  # Remove NA column names (if conversion fails)
  df <- df[, !is.na(names(df))]
  
  # Define common missing value representations
  missing_values <- c("no data", "")
  
  # Convert all non-country columns to numeric, replacing known missing values
  df[-1] <- lapply(df[-1], function(x) {
    x <- as.character(x)  # Convert to character (prevents factor issues)
    x[x %in% missing_values] <- NA  # Replace text-based missing values with real NA
    as.numeric(x)  # Convert to numeric
  })
  
  # Convert to long format
  df_long <- df %>%
    pivot_longer(cols = -country, names_to = "year", values_to = "value") %>%
    mutate(year = as.numeric(year)) %>%
    rename(!!sheet_name := value)  # Rename dynamically
  
  return(df_long)
}

# Read and reshape each dataset
cbi_long <- reshape_long("Transformed_CBI_Data")
gdp_long <- reshape_long("GDP growth")
stability_long <- reshape_long("Political stability")
yield_long <- reshape_long("bond yield")

# Generate a full dataset covering 1970-2023
full_years <- expand.grid(country = unique(cbi_long$country), year = 1970:2023)

# Merge datasets on country & year (left join to keep all years)
regression_data <- full_years %>%
  left_join(cbi_long, by = c("country", "year")) %>%
  left_join(gdp_long, by = c("country", "year")) %>%
  left_join(stability_long, by = c("country", "year")) %>%
  left_join(yield_long, by = c("country", "year"))

# Ensure Political Stability is NA for years before 1996
regression_data <- regression_data %>%
  mutate(`Political stability` = ifelse(year < 1996, NA, `Political stability`))

# Sort data by country first, then by year
regression_data <- regression_data %>%
  arrange(country, year)

# Save the cleaned dataset as a new sheet in the original file
wb <- loadWorkbook(file_path)

# Remove existing sheet if it exists
if ("Regression_Data" %in% names(wb)) {
  removeWorksheet(wb, "Regression_Data")
}

# Add new sheet and write data
addWorksheet(wb, "Regression_Data")
writeData(wb, sheet = "Regression_Data", regression_data)
regression_data <- regression_data %>%
  arrange(country, year)


# Save workbook
saveWorkbook(wb, file_path, overwrite = TRUE)

# Print completion message
print("Regression-ready dataset created and saved in 'Regression_Data' sheet, with correct column names and missing values handled.")

#------------------------ Variable creation ---------------------------------------
##In this chunk we create variables for our model 

## Define regression panel dataset
# Define regression dataset 
df_regression <- read_excel(file_path, sheet = "Regression_Data")
print(df_regression)

# Transforming countries and years in numbers to make them readable for the regression
df_regression$country <- as.factor(df_regression$country) # Ensure 'country' is a factor
df_regression$country_id <- as.numeric(df_regression$country) # Create a numeric ID for each country
df_regression$year <- as.numeric(df_regression$year) # Ensure 'year' is numeric
df_regression <- pdata.frame(df_regression, index = c("country_id", "year"), drop.index = FALSE) # Convert to panel data format
pdim(df_regression) # Check if indexing is correct
head(df_regression) # Print first rows to confirm

## Regression variables 
# We create a variable for the CBI index, primary balance, GDP growth and political stability
# Load the dataset once
df_regression <- df_regression %>%
  rename(
    CBI = `Transformed_CBI_Data`,
    g = `GDP.growth`,
    Poly = `Political.stability`,
    yield = `bond.yield`
  )

# Ensure only regression variables are numeric (country_id & year are already numeric)
df_regression <- df_regression %>%
  mutate(across(c(CBI, g, Poly, yield), as.numeric))

# Print first rows to confirm
head(df_regression)

#We create spread variable subtracting yield to US Treasuries data

# 1: We create a lookup table for U.S. yields by year
us_yields <- df_regression %>%
  filter(country == "United States") %>%  # or "USA", "US" — use your actual label
  select(year, us_yield = yield)

# 2: We Join U.S. yield to full dataset by year
df_regression <- df_regression %>%
  left_join(us_yields, by = "year") %>%
  mutate(spread = yield - us_yield)


#----------------------- Divide sample into complete, developed, developing -------------------------
## In this chunk, we divide our sample in developed and developing countries subsamples 

# Define developed countries
developed_countries <- c(
  "Australia", "Austria", "Belgium", "Canada", "Croatia", "Cyprus", "Denmark", 
  "Finland", "France", "Germany", "Greece", "Japan", "Iceland", "Ireland", 
  "Italy", "Luxembourg", "Netherlands", "Norway", "Poland", "Portugal", 
  "Slovenia", "Spain", "Sweden", "Switzerland", "United Kingdom", "United States"
)

# Add Development Status column to the complete sample
df_regression <- df_regression %>%
  mutate(Development_Status = ifelse(country %in% developed_countries, "Developed", "Developing"))


# Create subsamples for developed and developing countries
df_developed <- df_regression %>% filter(Development_Status == "Developed")
df_developing <- df_regression %>% filter(Development_Status == "Developing")

df_developed <- as.data.frame(df_developed)  # Reset structure
df_developed <- pdata.frame(df_developed, index = c("country_id", "year"), drop.index = FALSE)

df_developing <- as.data.frame(df_developing)
df_developing <- pdata.frame(df_developing, index = c("country_id", "year"), drop.index = FALSE)

#-------------------------- Descriptive statistics ----------------------------------------------------------------
## In this chunk we provide descriptive statistics from our variables in the complete 
#sample and in the two sub-samples

# Compute summary statistics for the whole sample 
summary(df_regression)

# Compute summary statistics for developed sample 
summary(df_developed)

# Compute summary statistics for developing sample 
summary(df_developing)

#-------------------------- Data Plotting ----------------------------------------------------------------
## Time changes plotting: we plot each variable over time in the whole sample, illustrating 
# differences between developing and developed countries

#spread 

dev.new()
ggplot(df_regression, aes(x = year, y = spread, group = country, color = Development_Status)) +
  geom_line(alpha = 0.5) +
  labs(title = "Spread on Treasuries Over time by Country Group",
       x = "Year", y = "Spread on US Treasuries",
       color = "Country Group") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


#CBI 

dev.new()
ggplot(df_regression, aes(x = year, y = CBI, group = country, color = Development_Status)) +
  geom_line(alpha = 0.5) +
  labs(title = "CBI Over Time by Country Group",
       x = "Year", y = "Central Bank Independence (CBI)",
       color = "Country Group") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#g 

dev.new()
ggplot(df_regression, aes(x = year, y = g, group = country, color = Development_Status)) +
  geom_line(alpha = 0.5) +
  labs(title = "GDP growth Over Time by Country Group",
       x = "Year", y = "GDP growth",
       color = "Country Group") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#Poly

dev.new()
ggplot(df_regression, aes(x = year, y = Poly, group = country, color = Development_Status)) +
  geom_line(alpha = 0.5) +
  labs(title = "Polytical stability Over Time by Country Group",
       x = "Year", y = "Polytical stability",
       color = "Country Group") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

### Box plotting: we create a box plot for each variable, staring from the whole 
# sample and proceeding with developed and developing countries

#spread on US Treasuries - complete 
df_regression <- as.data.frame(df_regression)

dev.new()
ggplot(df_regression, aes(y = spread)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Distribution of spread on US Treasuries – Whole Sample",
       y = "spread on US Treasuries") +
  theme_minimal()

#spread on US Treasuries - developed 
df_developed <- as.data.frame(df_developed)

dev.new()
ggplot(df_developed, aes(y = spread)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Distribution of spread on US Treasuries – Developed countries",
       y = "spread on US Treasuries") +
  theme_minimal()

#spread on US Treasuries - developing 
df_developing <- as.data.frame(df_developing)

dev.new()
ggplot(df_developing, aes(y = spread)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Distribution of spread on US Treasuries – Developing countries",
       y = "spread on US Treasuries") +
  theme_minimal()

#------------------------------- Complete sample regression -----------------------
##In this chunk we are running our regression model with spread as dependent variable, 
#CBI as independent variable and growth and political instability as controls on the 
#complete sample 

#We now run a panel regression for our model on the whole sample 
re_model_complete <- plm(spread ~ CBI + g + Poly, data = df_regression, model = "random")
fe_model_complete <- plm(spread ~ CBI + g + Poly, data = df_regression, model = "pooling")

#We provide a Hausman test to see which model is best 
phtest(re_model_complete, fe_model_complete)

#We check for multicollinearity 

model_vif <- lm(spread ~ CBI + g + Poly, data = df_regression)
vif(model_vif)

#We summarize our model 
summary(re_model_complete)

#We check for residual distribution
fitted_vals <- as.numeric(fitted(re_model_complete))
residual_vals <- as.numeric(residuals(re_model_complete))
valid_rows <- complete.cases(fitted_vals, residual_vals)
dev.new()
plot(fitted_vals[valid_rows], residual_vals[valid_rows],
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residuals vs Fitted Values (RE Model)",
     pch = 16, col = "blue")
abline(h = 0, col = "red", lty = 2)

#We check for serial correlation (p < 0.05 → serial correlation present)
pbgtest(re_model_complete)

#We check for heteroskedasticity (p < 0.05 → evidence of heteroskedasticity)
bptest(re_model_complete)

# Since we found heteroskedasticity, we use robust standard errors
#We transform the RSE model to make it readable to the table function
spread_complete_se <- sqrt(diag(vcovHC(re_model_complete, type = "HC1")))
coeftest(fe_model_complete, vcov = vcovHC(re_model_complete, type = "HC1"))

#We try to see if transforming with log PB changes something 
df_regression$log_spread <- log(df_regression$spread)

df_regression$spread <- as.numeric(df_regression$spread)
df_regression$log_spread <- sign(df_regression$spread) *
  log(abs(df_regression$spread) + 1)
spread_log_model <- plm(log_spread ~ CBI + g + Poly, data = df_regression, model = "within")
summary(spread_log_model)

#We check for heteroskedasticity (p < 0.05 → evidence of heteroskedasticity)
bptest(PB_developed_log)

#We transform the RSE model to make it readable to the table function
PB_developed_log_se <- sqrt(diag(vcovHC(PB_developed_log, type = "HC1")))


#------------------------------- Developed sample regression -----------------------
##In this chunk we are running our regression model with spread as dependent variable, 
#CBI as independent variable and growth and political instability as controls on the 
#developed countries sub-sample

#We now run a panel regression for our model on the whole sample 
re_model_developed <- plm(spread ~ CBI + g + Poly, data = df_developed, model = "random")
fe_model_developed <- plm(spread ~ CBI + g + Poly, data = df_developed, model = "pooling")

#We provide a Hausman test to see which model is best 
phtest(re_model_developed, fe_model_developed)

#We check for multicollinearity 
model_vif <- lm(spread ~ CBI + g + Poly, data = df_developed)
vif(model_vif)

#We summarize our model 
summary(fe_model_developed)

#We check for residual distribution
fitted_vals <- as.numeric(fitted(fe_model_developed))
residual_vals <- as.numeric(residuals(fe_model_developed))
valid_rows <- complete.cases(fitted_vals, residual_vals)
dev.new()
plot(fitted_vals[valid_rows], residual_vals[valid_rows],
     xlab = "Fitted values developed countries",
     ylab = "Residuals developed countries",
     main = "Residuals vs Fitted Values (FE Model developed countries)",
     pch = 16, col = "blue")
abline(h = 0, col = "red", lty = 2)


#We check for serial correlation (p < 0.05 → serial correlation present)
pbgtest(fe_model_developed)

#We check for heteroskedasticity (p < 0.05 → evidence of heteroskedasticity)
bptest(fe_model_developed)

# Since we found heteroskedasticity, we use robust standard errors
#We transform the RSE model to make it readable to the table function
spread_developed_se <- sqrt(diag(vcovHC(fe_model_developed, type = "HC1")))

coeftest(fe_model_developed, vcov = vcovHC(fe_model_complete, type = "HC1"))


#------------------------------- Developing sample regression -----------------------
##In this chunk we are running our regression model with spread as dependent variable, 
#CBI as independent variable and growth and political instability as controls on the 
#developing countries sub-sample

#We now run a panel regression for our model on the whole sample 
re_model_developing <- plm(spread ~ CBI + g + Poly, data = df_developing, model = "random")
fe_model_developing <- plm(spread ~ CBI + g + Poly, data = df_developing, model = "pooling")

#We provide a Hausman test to see which model is best (p < 0.05 -> use FE)
phtest(re_model_developing, fe_model_developing)

#We check for multicollinearity 

model_vif <- lm(spread ~ CBI + g + Poly, data = df_developing)
vif(model_vif)

#We summarize our model 
summary(fe_model_developing)

#We check for residual distribution
fitted_vals <- as.numeric(fitted(fe_model_developing))
residual_vals <- as.numeric(residuals(fe_model_developing))
valid_rows <- complete.cases(fitted_vals, residual_vals)
dev.new()
plot(fitted_vals[valid_rows], residual_vals[valid_rows],
     xlab = "Fitted values developing countries",
     ylab = "Residuals developing countries",
     main = "Residuals vs Fitted Values (FE Model developing countries)",
     pch = 16, col = "blue")
abline(h = 0, col = "red", lty = 2)


#We check for serial correlation (p < 0.05 → serial correlation present)
pbgtest(fe_model_developing)

#We check for heteroskedasticity (p < 0.05 → evidence of heteroskedasticity)
bptest(fe_model_developing)

# Since we found heteroskedasticity, we use robust standard errors
coeftest(fe_model_developing, vcov = vcovHC(fe_model_developing, type = "HC1"))

#We transform the RSE model to make it readable to the table function
spread_developing_se <- sqrt(diag(vcovHC(fe_model_developing, type = "HC1")))

## We try to see if something changes by smoothing spread taking its log

#We check for heteroskedasticity (p < 0.05 → evidence of heteroskedasticity)
bptest(model_log)

## We put our spread regression in a readable table for the final draft

stargazer(fe_model_complete, fe_model_developed, fe_model_developing,
          se = list(spread_complete_se, spread_developed_se, spread_developing_se),
          type = "html",
          out = "output/Spread_CBI_results.html",
          title = "Central Bank Independence and spread on US Treasuries",
          dep.var.labels = "Spread on US Treasuries",
          column.labels = c("Complete sample", "Developed", "Developing"),
          covariate.labels = c("CBI", "GDP Growth", "Political Stability"),
          omit.stat = c("f", "ser"),
          notes = c("Robust standard error in parentheses.",
                    "*** p<0.01, ** p<0.05, * p<0.1"),
          star.cutoffs = c(0.1, 0.05, 0.01),
          digits = 3)




