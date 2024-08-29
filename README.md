
# PoweREST

<!-- badges: start -->
<!-- badges: end -->

PoweREST is R package for the power analysis of detecting differential expressed genes between two conditions using 10X Visium spatial transcriptomics (ST). It enables the user to estimate the power or sample size needed for a 10X Visium ST experiment with and without prior dataset available by depicting how the study power is determined by three key parameters: (i) the number of biological replicates; (ii) the percentage of spots where the gene is detected in both groups; (iii) the log-fold change in average expression between two groups. PoweREST relies fully upon non-parametric modelling techniques but under biologically meaningful constraints which is extremely suitable for complex ST samples. The tool has been evaluated upon data from different tissue samples with promising and robust results.

A graphical interface of a shiny app is at the webpage [PoweREST](https://lanshui.shinyapps.io/PoweREST/), which provides power estimations upon different pilot ST datasets.

For detailed description of all methods and citation, please refer to our paper or GitHub page.

## Installation

You can install the development version of PoweREST like so:

``` r
# install devtools if necessary
install.packages('devtools')

# install the PoweREST package from GitHub for the latest version
devtools::install_github('lanshui98/PoweREST')

# or install it from the cran
install.packages('PoweREST')

# load package
library(PoweREST)
```

## Dependencies
* R version >= 4.4.0.
* R packages: scam, Seurat, dplyr, plotly, resample, fields, patchwork, ggplot2, boot, xgboost, rayshader, rayrender, knitr, rmarkdown, tidyr, knitr
If you have problems installing the package, please try to install necessary packages yourself from CRAN.
``` r
install.packages(c("scam","Seurat","dplyr","plotly","resample","fields","patchwork","ggplot2","boot","xgboost","rayshader", "rayrender","knitr","rmarkdown","tidyr","knitr"))
```

## How to use 'PoweREST'
Detailed steps are introduced in [Tutorial](https://lanshui98.github.io/powerest/documentation/config.html).

The vignette of the package is also available.

# Issues
All feedback, bug reports and suggestions are warmly welcomed! Please make sure to raise issues with a detailed and reproducible example and also please provide the output of your sessionInfo() in R! 

