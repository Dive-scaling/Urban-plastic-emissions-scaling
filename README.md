# Urban-plastic-emissions-scaling
This repository contains the complete set of R** scripts and analytical pipelines used for statistical modeling, quantile regressions, stability testing, and figure generation in our study on urban plastic emission scaling.  
# Urban Plastic Emission Scaling: Statistical Analysis & Visualization Pipeline

This repository contains the complete set of **R（3.1.446）** scripts and analytical pipelines used for statistical modeling, quantile regressions, stability testing, and figure generation in our study on urban plastic emission scaling. 

All scripts are tested, fully executable, and designed to reproduce the exact statistical outputs presented in **Supplementary Tables S1–S13** and the corresponding manuscript figures.

---


All analyses were developed and tested under **R (v4.3.0 or higher)**. 

To run the scripts, ensure you have the required R packages installed:

```R
install.packages(c(
  "quantreg",    # Quantile regression models
  "tidyverse",   # Data manipulation and visualization (ggplot2, dplyr, tidyr)
  "plotly",      # Interactive polar sector plots
  "openxlsx",    # Reading and writing Excel files
  "readxl",      # Reading Excel datasets
  "ggh4x",       # Extended ggplot2 facets and axes
  "cowplot",     # Plot assembly
  "patchwork",   # Multi-panel figure layout
  "ggpointdensity", # Density scatter plots
  "ggrastr",     # Rasterization for high-density plots
  "e1071",       # Skewness and statistical distribution functions
  "doParallel",  # Parallel computing for bootstrap routines
  "foreach"      # Parallel loop execution
))
