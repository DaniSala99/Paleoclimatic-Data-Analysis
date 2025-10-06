# Paleoclimatic Data Analysis

## Project Description

This repository contains a statistical analysis of paleoclimatic time series datasets, developed as a university project for the "Statistical Models and Stochastic Processes" course at Politecnico di Milano.

The project analyzes different methodologies for historical temperature reconstruction to evaluate their comparability, reliability, and ability to identify known historical climate events.

## Dataset

The dataset comprises 11 temperature anomaly time series (500 BC - 2018 AD) obtained through different paleoclimatic methods:

- **Stable isotopes**: Stalagmites (Austria)
- **Dendrochronology**: Switzerland, Slovakia, Alps
- **GHD (Grape Harvest Date)**: Switzerland, France
- **Chironomids**: Swiss lakes
- **Multi-proxy**: Mann series, Central European documentary data
- **Historical indices**: Germany, Czech Republic, Switzerland, Poland

## Applied Methodologies

### 1. Dataset Preparation
- Outlier removal using modified IQR approach
- NA substitution with interpolation + white noise
- Normalization relative to 1900-1945 period

### 2. Statistical Analysis
- **Normality tests**: Lillie test, graphical analysis (QQ-norm, histograms)
- **Correlation analysis**: Spearman correlation coefficient
- **Multiple comparisons**: Kruskal-Wallis test, Conover-Iman test
- **Pairwise comparisons**: Bland-Altman, Wilcoxon test
- **Linear regression**: Models with temporal predictors

### 3. Method Validation Analysis
Verification of ability to identify known historical climate events:
- **Medieval Warm Period** (850-1350)
- **Little Ice Age** (1350-1850)
- **Climate Change** (last 150 years)

### 4. Changing Point Identification
- Changing point technique to identify climate trends
- Application of Mann threshold (1904) for global warming

## Key Results

1. **Significant correlations** between agriculture-based methods (C-E-K)
2. **Geographic affinity** not statistically relevant
3. **Multi-proxy methods** (Mann, Glaser) more reliable for all climate events
4. **Seasonal methods** (GHD) limited in identifying long-term trends
5. **Dating issues** in chironomid-based methods

## Repository Structure

```
├── data/
│   └── Dati.xlsx                 # Original dataset
├── src/
│   └── analisi_paleoclimatica.R  # Main R script
├── docs/
│   └── relazione.pdf             # Complete report (Italian)
├── results/
│   └── plots/                    # Generated plots
└── README.md                     # This file
```

## How to Run the Analysis

### Option 1: GitHub Codespaces (Recommended)

1. **Launch Codespaces**:
   - Click the green "Code" button in the repository
   - Select "Codespaces" → "Create codespace on main"
   - Wait for the environment to load (2-3 minutes)

2. **Install R and dependencies**:
   ```bash
   # In the Codespaces terminal
   sudo apt-get update
   sudo apt-get install -y r-base r-base-dev
   
   # Launch R
   R
   ```

3. **Install required packages**:
   ```r
   # Inside the R session
   install.packages(c(
     "Rcpp", "mice", "tidyverse", "readxl", 
     "segmented", "BlandAltmanLeh", "ggplot2", 
     "nortest", "lawstat"
   ))
   ```

4. **Run the analysis**:
   ```r
   # Load and execute the script
   source("src/analisi_paleoclimatica.R")
   ```

### Option 2: RStudio Cloud

1. Go to [posit.cloud](https://posit.cloud)
2. Create a free account
3. Create a new project from GitHub repository
4. Enter this repository's URL
5. Run the R script in the RStudio environment

### Option 3: Google Colab

1. Open [Google Colab](https://colab.research.google.com)
2. Create a new notebook
3. Change runtime type to "R"
4. Clone the repository:
   ```r
   system("git clone https://github.com/YOUR_USERNAME/REPO_NAME.git")
   setwd("REPO_NAME")
   ```
5. Install packages and run the script

### Option 4: Binder

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/YOUR_USERNAME/REPO_NAME/HEAD?urlpath=rstudio)

Click the Binder badge to open a complete RStudio environment in your browser.

## R Dependencies

```r
# Required packages
install.packages(c(
  "Rcpp",           # C++ compilation
  "mice",           # Missing data handling
  "tidyverse",      # Data manipulation
  "readxl",         # Excel file reading
  "segmented",      # Segmented regression
  "BlandAltmanLeh", # Bland-Altman test
  "ggplot2",        # Visualizations
  "nortest",        # Normality tests
  "lawstat"         # Statistical tests
))
```

## Binder Configuration

To enable Binder, add these files to the repository:

### `install.R`
```r
install.packages(c(
  "Rcpp", "mice", "tidyverse", "readxl", 
  "segmented", "BlandAltmanLeh", "ggplot2", 
  "nortest", "lawstat"
))
```

### `runtime.txt`
```
r-2023-10-01
```

## Technical Notes

- **Execution time**: ~10-15 minutes for complete analysis
- **Memory required**: ~2GB RAM
- **Output**: Plots saved in `results/plots/`
- **Reproducibility**: Fixed seeds for consistent results

## Citation

If you use this code for your research, please cite:

```
Bogani, D., Bonora, S., Bovio, M., Bressi, F., Ciabattoni, E., 
Ferrari, G., Messinetti, M., Sala, D. (2022). 
Paleoclimatic Data Analysis. 
Politecnico di Milano, Statistical Models and Stochastic Processes Course.
```

## License

Project developed for educational purposes - Politecnico di Milano 2021/2022

## Contact

For questions or technical issues, please open an Issue in the repository.
