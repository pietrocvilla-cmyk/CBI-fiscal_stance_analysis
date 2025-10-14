# R Analysis Scripts

This folder contains the R code for analyzing the relationship between Central Bank Independence and fiscal policy outcomes.

## Files

- `Coding_sample_Code_Thesis_fiscal_stance.R` - Analysis with primary balance and debt-to-GDP ratio as dependent variables 
- `Coding_sample_Code_thesis_yield.R` - Analysis with (spread on) government bond yields as dependent variable 

### Setup & Execution

**Step 1: Open the Project**
- Navigate to the main repository folder
- **Double-click `CBI-fiscal_stance_analysis.Rproj`**
- RStudio will open with the correct working directory automatically set ✨

**Step 2: Install Required Packages** (first time only)
```r
install.packages(c("readxl", "dplyr", "tidyr", "openxlsx", "plm", 
                   "lmtest", "sandwich", "car", "ggplot2", "zoo", "stargazer"))
```

**Step 3: Run the Scripts**
- Open `Code_Thesis_fiscal_stance.R` from the Files pane
- Click **"Source"** (top-right) or run line-by-line
- Repeat for `Code_thesis_yield.R`

**Step 4: View Results**
- Output files are saved in the `output/` folder
- Open `.html` files in a web browser to view regression tables

### Important Notes

✅ **Always open the project via the `.Rproj` file** - This ensures all file paths work correctly

⚠️ **Do not open individual R scripts directly** from File Explorer - Open them from within the RStudio Project

🔧 **File paths are automatic** - The RStudio Project handles all path management, so no manual configuration is needed
