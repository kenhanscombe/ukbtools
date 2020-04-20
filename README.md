ukbtools <img src='man/figures/logo.png' align="right" height="138" />
===

<!-- badges: start -->
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/ukbtools)](https://cran.r-project.org/package=ukbtools)
[![Travis build status](https://travis-ci.com/kenhanscombe/ukbtools.svg?branch=master)](https://travis-ci.com/kenhanscombe/ukbtools)
[![Codecov test coverage](https://codecov.io/gh/kenhanscombe/ukbtools/branch/master/graph/badge.svg)](https://codecov.io/gh/kenhanscombe/ukbtools?branch=master)
<!-- badges: end -->

After downloading and decrypting your UK Biobank (UKB) data with the supplied [UKB programs] (http://biobank.ctsu.ox.ac.uk/crystal/docs/UsingUKBData.pdf), you have multiple files that need to be brought together to give you a dataset to explore. The data file has column names that are edited field-codes from the [UKB data showcase](http://www.ukbiobank.ac.uk/data-showcase/). ukbtools makes it easy to collapse the multiple UKB files into a single dataset for analysis, in the process giving meaningful names to the variables. The package also includes functionality to retrieve ICD diagnoses, explore a sample subset in the context of the UKB sample, and collect genetic metadata.


## Installation

```{r, eval = FALSE}

# Install from CRAN
install.packages("ukbtools")

# Install latest development version
devtools::install_github("kenhanscombe/ukbtools", dependencies = TRUE)

```


## Prerequisite: Make a UKB fileset

Download<sup>§</sup> then decrypt your data and create a "UKB fileset" (.tab, .r, .html):

```{bash, eval = FALSE}
ukb_unpack ukbxxxx.enc key
ukb_conv ukbxxxx.enc_ukb r
ukb_conv ukbxxxx.enc_ukb docs

```

`ukb_unpack` decrypts your downloaded `ukbxxxx.enc` file, outputting a `ukbxxxx.enc_ukb` file. `ukb_conv` with the `r` flag converts the decrypted data to a tab-delimited file `ukbxxxx.tab` and an R script `ukbxxxx.r` that reads the tab file. The `docs` flag creates an html file containing a field-code-to-description table (among others).


<sup>§</sup> Full details of the data download and decrypt process are given in the [Using UK Biobank Data](http://biobank.ctsu.ox.ac.uk/crystal/docs/UsingUKBData.pdf) documentation.




## Make a UKB dataset

The function `ukb_df()` takes two arguments, the stem of your fileset and the path, and returns a dataframe with usable column names. This will take a few minutes. The rate-limiting step is reading and parsing the code in the UKB-generated .r file - not `ukb_df` per se.


```{r, eval = FALSE}

library(ukbtools)

my_ukb_data <- ukb_df("ukbxxxx")

```


You can also specify the path to your fileset if it is not in the current directory. For example, if your fileset is in a subdirectory of the working directory called data


```{r, eval = FALSE}

my_ukb_data <- ukb_df("ukbxxxx", path = "/full/path/to/my/data")

```


__Note:__ You can move the three files in your fileset after creating them with `ukb_conv`, but they should be kept together. `ukb_df()` automatically updates the read call in the R source file to point to the correct directory (the current directory by default, or a directory specified by `path`).



## Other tools

All tools are described on the [ukbtools webpage](https://kenhanscombe.github.io/ukbtools/) and in the package vignette "Explore UK Biobank Data"

```{r, eval = FALSE}

vignette("explore-ukb-data", package = "ukbtools")

```

For a list of all functions

```{r, eval = FALSE}

help(package = "ukbtools")

```
