# dataADHD

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
<!-- badges: end -->

The goal of dataADHD is to allow me to easily employ the analyses used in the study of the effect of mindfulness training on ADHD and sADHD

## Installation

You can install the development version of dataADHD like so: 

``` r
remotes::install_github("danielgertner00/dataADHD")
```

## Functions

These are the functions inside the package:

``` r
library(dataADHD)

descr_stats() # Descriptive Statistics
descr_table() # APA7 Table for Descriptive Statistics

mcpt_analysis() # Monte Carlo Permutation Test
mcpt_table() # APA7 MCPT Results Table

bf10_analysis() # Bayes Factor Analysis with Sensitivity Check
bf10_table() # APA7 Table for BF10 Results
bf10_plot() # Visualisation of BF10 Data Across Different Priors

estimB_analysis() # Bayes Estimation Analysis with Highest Density Intervals
estimB_table() # APA7 Bayes Estimation Results Table
hdi_plots() # Visualisation of Highest Density Intervals

```

