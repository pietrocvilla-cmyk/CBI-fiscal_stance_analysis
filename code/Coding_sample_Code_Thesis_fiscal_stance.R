# ============================================================
# Analysis of links between fiscal stance and CBI 
# Pietro Villa 
# Bocconi University 
# Bachelor in Economic and Social Sciences 
# Date: 03-03-2025 
# Description: 
# This script performs the empirical analysis for a bachelor's thesis 
# on CBI and fiscal stance, here proxied by fiscal balance and Debt-to-GDP ratio.
# ============================================================

######## Set the main project directory

# Use simple relative path from project root
file_path <- "data/Original Dataset.xlsx"

# Verify we're in the right place
if (!file.exists(file_path)) {
  stop("\n========================================\n",
       "ERROR: Cannot find data file!\n",
       "========================================\n",
       "Expected file: data/Dataset finale.xlsx\n",
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

# Load necessary packages
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

# ------------------ Adjust CBI index dataset form ------------------------------------------------------
##In this chunk we transform the CBI index file downloaded from Romelli's website 
# since all the other data sources for the other variables are already in wide format 

#Read the original Excel file
sheets <- excel_sheets(file_path)
print(sheets) 
df <- read_excel(file_path, sheet = "CBI data")  
head(df)

# Select relevant columns
df_selected <- df %>%
  select(country, year, cbie_index)

# Transform the dataset from long format to wide format
df_wide <- df_selected %>%
  pivot_wider(names_from = year, values_from = cbie_index)

year_columns <- as.character(sort(as.numeric(names(df_wide)[-1])))  # Extract year columns and sort
df_wide <- df_wide %>%
  select(country, all_of(year_columns))  # Reorder the dataset


# Load the original Excel file
wb <- loadWorkbook(file_path)

# Add the transformed dataset as a new sheet
addWorksheet(wb, "Transformed_CBI_Data")

# Write the transformed data to the new sheet
writeData(wb, sheet = "Transformed_CBI_Data", df_wide)

# Save the updated Excel file (overwrite the original file)
saveWorkbook(wb, file_path, overwrite = TRUE)

# Print completion message
print("Dataset successfully transformed and saved as 'Transformed_CBI_Data' sheet in the original file.")

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
relevant_sheets <- c("Transformed_CBI_Data", "prim_exp", "GDP growth", "Political stability", "Debt")

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
exp_long <- reshape_long("prim_exp")
gdp_long <- reshape_long("GDP growth")
stability_long <- reshape_long("Political stability")
debt_long <- reshape_long("Debt")

# Generate a full dataset covering 1970-2023
full_years <- expand.grid(country = unique(cbi_long$country), year = 1970:2023)

# Merge datasets on country & year (left join to keep all years)
regression_data <- full_years %>%
  left_join(cbi_long, by = c("country", "year")) %>%
  left_join(exp_long, by = c("country", "year")) %>%
  left_join(gdp_long, by = c("country", "year")) %>%
  left_join(stability_long, by = c("country", "year")) %>%
  left_join(debt_long, by = c("country", "year"))

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

## Define regression panel dataset
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
    PB = `prim_exp`,
    g = `GDP.growth`,
    Poly = `Political.stability`,
    debt = `Debt`
  )

# Ensure only regression variables are numeric (country_id & year are already numeric)
df_regression <- df_regression %>%
  mutate(across(c(CBI, PB, g, Poly, debt), as.numeric))

# Print first rows to confirm
head(df_regression)

## Smooth fiscal balance 
# We apply a rolling 5-year average to fiscal balance to be used in the regression

df_regression <- df_regression %>%
  group_by(country_id) %>%
  arrange(year) %>%
  mutate(PB_rolling_avg = rollmean(PB, k = 5, fill = NA, align = "right")) %>%
  ungroup()

#Eliminate several outlier 
lowest_PB_row <- df_regression %>%
  filter(!is.na(PB_rolling_avg)) %>%  # remove NA values
  arrange(PB_rolling_avg) %>%
  slice(1)  # get the first (lowest) row

print(lowest_PB_row)

df_regression <- df_regression %>%
  filter(country != "Equatorial Guinea")


#----------------------- Divide sample into complete, developed, developing -------------------------

# Define developed countries
developed_countries <- c("Australia", "Austria", "Belgium", "Canada", "Croatia", "Cyprus", "Denmark", "Estonia", "Finland", "France", "Germany", 
                        "Greece" ,"Japan", "Iceland", "Ireland", "Italy", "Latvia", "Luxembourg", 
                         "Netherlands", "New Zealand", "Norway", "Portugal", "Slovenia", "Spain", "Sweden", "Switzerland", "United Kingdom")

# Add Development Status column to the complete sample
df_regression <- df_regression %>%
  mutate(Development_Status = ifelse(country %in% developed_countries, "Developed", "Developing"))

# Create subsamples for developed and developing countries
df_developed <- df_regression %>% filter(Development_Status == "Developed")
df_developing <- df_regression %>% filter(Development_Status == "Developing")

df_developed <- pdata.frame(df_developed, index = c("country_id", "year"), drop.index = FALSE)
df_developing <- pdata.frame(df_developing, index = c("country_id", "year"), drop.index = FALSE)



#-------------------------- Descriptive statistics ----------------------------------------------------------------
##In this chunk we display summary statistics for each variable in the main sample
#and in both subsamples

# Compute summary statistics for the whole sample 
summary(df_regression)

# Calculate IQR-based bounds
Q1 <- quantile(df_regression$g, 0.25, na.rm = TRUE)
Q3 <- quantile(df_regression$g, 0.75, na.rm = TRUE)
IQR_val <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR_val
upper_bound <- Q3 + 1.5 * IQR_val

# Filter outliers
outliers_tot_g <- df_regression %>% 
  filter(g < lower_bound | g > upper_bound)

# Compute summary statistics for developed sample 
summary(df_developed)

# Compute summary statistics for developing sample 
summary(df_developing)

#-------------------------- Data Visualization ----------------------------------------------------------------
###In this chunk, we visualize our data plotting it over time at first and on 
#scatter plots then. 

## Time changes plotting: we plot each variable over time in the whole sample, illustrating 
# differences between developing and developed countries

#CBI 

dev.new()
ggplot(df_regression, aes(x = year, y = CBI, group = country, color = Development_Status)) +
  geom_line(alpha = 0.5) +
  labs(title = "CBI Over Time by Country Group",
       x = "Year", y = "Central Bank Independence (CBI)",
       color = "Country Group") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


#PB 

dev.new()
ggplot(df_regression, aes(x = year, y = PB_rolling_avg, group = country, color = Development_Status)) +
  geom_line(alpha = 0.5) +
  labs(title = "PB rolling average Over Time by Country Group",
       x = "Year", y = "PB rolling average",
       color = "Country Group") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#debt 

dev.new()
ggplot(df_regression, aes(x = year, y = debt, group = country, color = Development_Status)) +
  geom_line(alpha = 0.5) +
  labs(title = "Debt-to-GDP ratio Over Time by Country Group",
       x = "Year", y = "Debt-to-GDP ratio",
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

#CBI - whole 

dev.new()
ggplot(df_regression, aes(y = CBI)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Distribution of CBI – Whole Sample",
       y = "Central Bank Independence") +
  theme_minimal()

#CBI - developed 
df_developed <- as.data.frame(df_developed)

dev.new()
ggplot(df_developed, aes(y = CBI)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Distribution of CBI – Developed countries",
       y = "Central Bank Independence") +
  theme_minimal()

#CBI - developing 
df_developing <- as.data.frame(df_developing)

dev.new()
ggplot(df_developing, aes(y = CBI)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Distribution of CBI – Developing countries",
       y = "Central Bank Independence") +
  theme_minimal()

#Debt-to-GDP ratio - whole 

dev.new()
ggplot(df_regression, aes(y = debt)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Debt-to-GDP ratio – Whole Sample",
       y = "Debt-to-GDP ratio") +
  theme_minimal()

#Debt-to-GDP ratio - developed 
df_developed <- as.data.frame(df_developed)

dev.new()
ggplot(df_developed, aes(y = debt)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Distribution of GDP growth ratio – Developed countries",
       y = "Debt-to-GDP ratio") +
  theme_minimal()

#Debt-to-GDP ratio - developing 
df_developing <- as.data.frame(df_developing)

dev.new()
ggplot(df_developing, aes(y = debt)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Debt-to-GDP ratio – Developing countries",
       y = "Debt-to-GDP ratio") +
  theme_minimal()


#GDP growth - whole 

dev.new()
ggplot(df_regression, aes(y = g)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Distribution of GDP growth – Whole Sample",
       y = "GDP growth") +
  theme_minimal()

#GDP growth - developed 
df_developed <- as.data.frame(df_developed)

dev.new()
ggplot(df_developed, aes(y = g)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Distribution of GDP growth – Developed countries",
       y = "GDP growth") +
  theme_minimal()

#GDP growth - developing 
df_developing <- as.data.frame(df_developing)

dev.new()
ggplot(df_developing, aes(y = g)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "GDP growth – Developing countries",
       y = "GDP growth") +
  theme_minimal()

#Political stability - whole 

dev.new()
ggplot(df_regression, aes(y = Poly)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Political stability – Whole Sample",
       y = "Political stability") +
  theme_minimal()

#Political stability - developed 
df_developed <- as.data.frame(df_developed)

dev.new()
ggplot(df_developed, aes(y = Poly)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Political stability – Developed countries",
       y = "Political stability") +
  theme_minimal()

#Political stability - developing 
df_developing <- as.data.frame(df_developing)

dev.new()
ggplot(df_developing, aes(y = Poly)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Political stability – Developing countries",
       y = "Political stability") +
  theme_minimal()

#------------------------------- Complete sample regression - Primary Balance -----------------------
##In this chunk we run our model on the complete sample with primary balance as independent variable; 
#then, we do the same with its logarithmic transformation

#We now run a panel regression for our model on the whole sample 
re_model_complete <- plm(PB_rolling_avg ~ CBI + g + Poly, data = df_regression, model = "random")
fe_model_complete <- plm(PB_rolling_avg ~ CBI + g + Poly, data = df_regression, model = "pooling")

#We provide a Hausman test to see which model is best 
phtest(re_model_complete, fe_model_complete)

#We check for multicollinearity 
model_vif <- lm(PB ~ CBI + g + Poly, data = df_regression)
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
coeftest(re_model_complete, vcov = vcovHC(re_model_complete, type = "HC1"))

#We transform the RSE model to make it readable to the table function
PB_complete_se <- sqrt(diag(vcovHC(re_model_complete, type = "HC1")))

#We try to see if transforming with log PB changes something 
df_regression$log_PB <- log(df_regression$PB)

df_regression$PB_rolling_avg <- as.numeric(df_regression$PB_rolling_avg)
df_regression$log_signed_PB_rolling_avg <- sign(df_regression$PB_rolling_avg) *
  log(abs(df_regression$PB_rolling_avg) + 1)
PB_regression_log <- plm(log_signed_PB_rolling_avg ~ CBI + g + Poly, data = df_regression, model = "within")
summary(PB_regression_log)

coeftest(model_log, vcov = vcovHC(model_log, type = "HC1"))

#We check for heteroskedasticity (p < 0.05 → evidence of heteroskedasticity)
bptest(PB_regression_log)

#We transform the RSE model to make it readable to the table function
PB_regression_log_se <- sqrt(diag(vcovHC(PB_regression_log, type = "HC1")))



#------------------------------- Developed sample regression -----------------------
##In this chunk we run our model on developed countries only with primary balance 
#as independent variable; then we do the same with its logarithmic trnasformation

#We now run a panel regression for our model on the whole sample 
re_model_developed <- plm(PB_rolling_avg ~ CBI + g + Poly, data = df_developed, model = "random")
fe_model_developed <- plm(PB_rolling_avg ~ CBI + g + Poly, data = df_developed, model = "pooling")

#We provide a Hausman test to see which model is best 
phtest(re_model_developed, fe_model_developed)

#We check for multicollinearity 
model_vif <- lm(PB ~ CBI + g + Poly, data = df_developed)
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
coeftest(fe_model_developed, vcov = vcovHC(fe_model_complete, type = "HC1"))

#We transform the RSE model to make it readable to the table function
PB_developed_se <- sqrt(diag(vcovHC(fe_model_developed, type = "HC1")))

#We try to see if transforming with log PB changes something 
df_developed$log_PB <- log(df_developed$PB)

df_developed$PB_rolling_avg <- as.numeric(df_developed$PB_rolling_avg)
df_developed$log_developed_PB_rolling_avg <- sign(df_developed$PB_rolling_avg) *
  log(abs(df_developed$PB_rolling_avg) + 1)
PB_developed_log <- plm(log_developed_PB_rolling_avg ~ CBI + g + Poly, data = df_developed, model = "within")
summary(PB_developed_log)

#We check for heteroskedasticity (p < 0.05 → evidence of heteroskedasticity)
bptest(PB_developed_log)

#We transform the RSE model to make it readable to the table function
PB_developed_log_se <- sqrt(diag(vcovHC(PB_developed_log, type = "HC1")))



#------------------------------- Developing sample regression -----------------------
##In this chunk we run our model on developing countries only with primary balance 
#as independent variable; then we do the same with its logarithmic transformation

#We now run a panel regression for our model on the whole sample 
re_model_developing <- plm(PB_rolling_avg ~ CBI + g + Poly, data = df_developing, model = "random")
fe_model_developing <- plm(PB_rolling_avg ~ CBI + g + Poly, data = df_developing, model = "pooling")

#We provide a Hausman test to see which model is best (p < 0.05 -> use FE)
phtest(re_model_developing, fe_model_developing)

#We check for multicollinearity 
model_vif <- lm(PB ~ CBI + g + Poly, data = df_developing)
vif(model_vif)

#We summarize our model 
summary(re_model_developing)

#We check for residual distribution
fitted_vals <- as.numeric(fitted(re_model_developing))
residual_vals <- as.numeric(residuals(re_model_developing))
valid_rows <- complete.cases(fitted_vals, residual_vals)
dev.new()
plot(fitted_vals[valid_rows], residual_vals[valid_rows],
     xlab = "Fitted values developing countries",
     ylab = "Residuals developing countries",
     main = "Residuals vs Fitted Values (RE Model developing countries)",
     pch = 16, col = "blue")
abline(h = 0, col = "red", lty = 2)


#We check for serial correlation (p < 0.05 → serial correlation present)
pbgtest(re_model_developing)

#We check for heteroskedasticity (p < 0.05 → evidence of heteroskedasticity)
bptest(re_model_developing)

# Since we found heteroskedasticity, we use robust standard errors
coeftest(re_model_developing, vcov = vcovHC(re_model_developing, type = "HC1"))

#We transform the RSE model to make it readable to the table function
PB_developing_se <- sqrt(diag(vcovHC(re_model_developing, type = "HC1")))

## We try to see if something changes by smoothing PB taking its log 
df_developing$log_PB <- log(df_developing$PB)

df_developing$PB_rolling_avg <- as.numeric(df_developing$PB_rolling_avg)
df_developing$log_signed_PB_rolling_avg <- sign(df_developing$PB_rolling_avg) *
  log(abs(df_developing$PB_rolling_avg) + 1)
PB_developing_log <- plm(log_signed_PB_rolling_avg ~ CBI + g + Poly, data = df_developing, model = "within")
summary(PB_developing_log)

#We check for heteroskedasticity (p < 0.05 → evidence of heteroskedasticity)
bptest(PB_developing_log)

#We transform the RSE model to make it readable to the table function
PB_developing_log_se <- sqrt(diag(vcovHC(PB_developing_log, type = "HC1")))

coeftest(fe_model_developing, vcov = vcovHC(fe_model_developing, type = "HC1"))

## We put our Primary Balance regression in a readable table for the final draft

stargazer(re_model_complete, fe_model_developed, re_model_developing,
          se = list(PB_complete_se, PB_developed_se, PB_developing_se),
          type = "html",
          out = "output/PB_CBI_results.html", 
          title = "Central Bank Independence and Fiscal Balance",
          dep.var.labels = "Primary Balance (PB)",
          column.labels = c("Complete sample", "Developed", "Developing"),
          covariate.labels = c("CBI", "GDP Growth", "Political Stability"),
          omit.stat = c("f", "ser"),
          notes = c("Robust standard error coefficients in parentheses.",
                    "*** p<0.01, ** p<0.05, * p<0.1"),
          star.cutoffs = c(0.1, 0.05, 0.01),
          digits = 3)

#Now we generate the same table with its logarithmic transformation 

stargazer(PB_regression_log, PB_developed_log, PB_developing_log,
          se = list(PB_regression_log_se, PB_developed_log_se, PB_developing_log_se),
          type = "html",
          out = "output/PB_CBI_log_results.html", 
          title = "Central Bank Independence and Fiscal Balance - Log",
          dep.var.labels = c("Log PB",
                             "Log PB",
                             "Log PB"),
          column.labels = c("Complete sample", "Developed", "Developing"),
          covariate.labels = c("CBI", "GDP Growth", "Political Stability"),
          omit.stat = c("f", "ser"),
          notes = c("Robust standard error coefficients in parentheses.",
                    "*** p<0.01, ** p<0.05, * p<0.1"),
          star.cutoffs = c(0.1, 0.05, 0.01),
          digits = 3)




#------------------------------- Regression Debt on complete sample -------------------
### We now apply the same regressions using Debt to GDP ratio as endogenous variable
##In this chunk we run our model on complete sample with Debt-to-GDP 
#as independent variable

#We now run a panel regression for our model on the whole sample 
re_model_complete_debt <- plm(debt ~ CBI + g + Poly, data = df_regression, model = "random")
fe_model_complete_debt <- plm(debt ~ CBI + g + Poly, data = df_regression, model = "pooling")

#We provide a Hausman test to see which model is best 
phtest(re_model_complete_debt, fe_model_complete_debt)

#We check for multicollinearity 
model_vif <- lm(debt ~ CBI + g + Poly, data = df_regression)
vif(model_vif)

#We summarize our model 
summary(fe_model_complete_debt)

#We check for residual distribution
fitted_vals <- as.numeric(fitted(fe_model_complete_debt))
residual_vals <- as.numeric(residuals(fe_model_complete_debt))
valid_rows <- complete.cases(fitted_vals, residual_vals)
dev.new()
plot(fitted_vals[valid_rows], residual_vals[valid_rows],
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residuals vs Fitted Values (FE Model)",
     pch = 16, col = "blue")
abline(h = 0, col = "red", lty = 2)


#We check for serial correlation (p < 0.05 → serial correlation present)
pbgtest(fe_model_complete_debt)

#We check for heteroskedasticity (p < 0.05 → evidence of heteroskedasticity)
bptest(fe_model_complete_debt)

# Since we found heteroskedasticity, we use robust standard errors
coeftest(fe_model_complete_debt, vcov = vcovHC(fe_model_complete_debt, type = "HC1"))

#We transform the RSE model to make it readable to the table function
debt_complete_se <- sqrt(diag(vcovHC(fe_model_complete_debt, type = "HC1")))



#------------------------------- Developed sample regression -----------------------
##In this chunk we run our model on developed countries only with Debt-to-GDP 
#as independent variable

#We now run a panel regression for our model on the whole sample 
re_model_developed_debt <- plm(debt ~ CBI + g + Poly, data = df_developed, model = "random")
fe_model_developed_debt <- plm(debt ~ CBI + g + Poly, data = df_developed, model = "pooling")

#We provide a Hausman test to see which model is best 
phtest(re_model_developed_debt, fe_model_developed_debt)

#We check for multicollinearity 
model_vif <- lm(debt ~ CBI + g + Poly, data = df_developed)
vif(model_vif)

#We summarize our model 
summary(fe_model_developed_debt)

#We check for residual distribution
fitted_vals <- as.numeric(fitted(fe_model_developed_debt))
residual_vals <- as.numeric(residuals(fe_model_developed_debt))
valid_rows <- complete.cases(fitted_vals, residual_vals)
dev.new()
plot(fitted_vals[valid_rows], residual_vals[valid_rows],
     xlab = "Fitted values developed countries",
     ylab = "Residuals developed countries",
     main = "Residuals vs Fitted Values (FE Model developed countries)",
     pch = 16, col = "blue")
abline(h = 0, col = "red", lty = 2)


#We check for serial correlation (p < 0.05 → serial correlation present)
pbgtest(fe_model_developed_debt)

#We check for heteroskedasticity (p < 0.05 → evidence of heteroskedasticity)
bptest(fe_model_developed_debt)

# Since we found heteroskedasticity, we use robust standard errors

coeftest(fe_model_developed_debt, vcov = vcovHC(fe_model_complete_debt, type = "HC1"))


debt_developed_se <- sqrt(diag(vcovHC(fe_model_developed_debt, type = "HC1")))


#------------------------------- Developing sample regression -----------------------
##In this chunk we run our model on developing countries only with Debt-to-GDP 
#as independent variable

#We now run a panel regression for our model on the whole sample 
re_model_developing_debt <- plm(debt ~ CBI + g + Poly, data = df_developing, model = "random")
fe_model_developing_debt <- plm(debt ~ CBI + g + Poly, data = df_developing, model = "pooling")

#We provide a Hausman test to see which model is best (p < 0.05 -> use FE)
phtest(re_model_developing_debt, fe_model_developing_debt)

#We check for multicollinearity 

model_vif <- lm(debt ~ CBI + g + Poly, data = df_developing)
vif(model_vif)

#We summarize our model 
summary(fe_model_developing_debt)

#We check for residual distribution
fitted_vals <- as.numeric(fitted(fe_model_developing_debt))
residual_vals <- as.numeric(residuals(fe_model_developing_debt))
valid_rows <- complete.cases(fitted_vals, residual_vals)
dev.new()
plot(fitted_vals[valid_rows], residual_vals[valid_rows],
     xlab = "Fitted values developing countries",
     ylab = "Residuals developing countries",
     main = "Residuals vs Fitted Values (FE Model developing countries)",
     pch = 16, col = "blue")
abline(h = 0, col = "red", lty = 2)


#We check for serial correlation (p < 0.05 → serial correlation present)
pbgtest(fe_model_developing_debt)

#We check for heteroskedasticity (p < 0.05 → evidence of heteroskedasticity)
bptest(fe_model_developing_debt)

# Since we found heteroskedasticity, we use robust standard errors
coeftest(fe_model_developing_debt, vcov = vcovHC(fe_model_developing_debt, type = "HC1"))

#We transform the RSE model to make it readable to the table function
debt_developing_se <- sqrt(diag(vcovHC(fe_model_developing_debt, type = "HC1")))


#We check for heteroskedasticity (p < 0.05 → evidence of heteroskedasticity)
bptest(model_log_debt)

## We put our Debt regression in a readable table for the final draft

#install.packages("stargazer")
stargazer(fe_model_complete_debt, fe_model_developed_debt, fe_model_developing_debt,
          se = list(debt_complete_se, debt_developed_se, debt_developing_se),
          type = "html",
          out = "output/Debt_CBI_results.html",
          title = "Central Bank Independence and Debt-to-GDP ratio",
          dep.var.labels = "Debt-to-GDP ratio",
          column.labels = c("Complete sample", "Developed", "Developing"),
          covariate.labels = c("CBI", "GDP Growth", "Political Stability"),
          omit.stat = c("f", "ser"),
          notes = c("Robust standard error in parentheses.",
                    "*** p<0.01, ** p<0.05, * p<0.1"),
          star.cutoffs = c(0.1, 0.05, 0.01),
          digits = 3)







END











