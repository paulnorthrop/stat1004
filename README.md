
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/paulnorthrop/stat1004?branch=master&svg=true)](https://ci.appveyor.com/project/paulnorthrop/stat1004)

stat1004 <img src="standalone.png" align="right" />
===================================================

Introduction to Probability and Statistics
------------------------------------------

### What does stat1004 do?

The `stat1004` package provides R code, datasets and vignettes (tutorials demonstrating how to use R code) to help students taking STAT0002 (formerly STAT1004) Introduction to Probability and Statistics at University College London to understand the course material and to see how R can be used to perform some of the analyses in the course.

The package is:

-   in the middling stages of development;
-   probably of little use to people not taking STAT0002 at UCL.

### Installation

If you need to install R then you can get it from [CRAN](https://cran.r-project.org/).

If you would like to use RStudio (a user-friendly front-end to R) then you can get it from [here](https://www.rstudio.com/products/rstudio/download/) \[See "Installers for Supported Platforms" near the bottom of the page. Choose the FREE RStudio Desktop Open Source License.\]

To install the `stat1004` package from GitHub type the following at the R Console command prompt `>`. If

``` r
install.packages("remotes")

install.packages(c("plotrix", "rpanel", "rust", "smovie", "tkrplot", "MASS", "knitr", "distributions3"), 
                 dependencies = "Depends")
```

You only need to do this (install the `remotes` package etc) once. Then install `stat1004` (or reinstall it to get the latest version) using

``` r
remotes::install_github("paulnorthrop/stat1004", dependencies = "Depends", build_vignettes = TRUE)
```

### Getting started

Then type

``` r
library(stat1004)
?stat1004
```

to open the main help page, which contains links to vignettes, datasets and other sources of information. \[If `?stat1004` doesn't work then close RStudio, reopen it and try again.\]

You will find that some vignettes contain ideas that we have not yet covered in lectures. If you want to try to understand these ideas before we cover them then please use the links to further information that have been provided.

If you have any questions about this package please ask them via the [STAT0002 Moodle Discussion Forum](https://moodle.ucl.ac.uk/mod/hsuforum/view.php?id=866683).
