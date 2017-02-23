
# ukbtools: A set of tools to manipulate UK Biobank data

__Co-authors:__ Joni Coleman, Matthew Traylor, Cathryn Lewis

<br>

## Overview

After downloading and decrypting your UK Biobank (UKB) data with the supplied [UKB programs] (http://biobank.ctsu.ox.ac.uk/crystal/docs/UsingUKBData.pdf), you have multiple files that need to be brought together to give you a dataset to explore. The data file has column names that are edited field-codes from the [UKB data showcase](http://www.ukbiobank.ac.uk/data-showcase/). `ukbtools` makes it easy to collapse the multiple UKB files into a single dataset for analysis, in the process giving meaningful names to the variables. The package also includes functionality to retreive ICD diagnoses, explore a sample subset in the context of the UKB sample, and collect genetic metadata.

<br>

## Installation

You can install this development version from github

```{r, eval = FALSE}

library(devtools)
install_github("kenhanscombe/ukbtools", build_vignettes = TRUE, dependencies = TRUE)

```

<br>

__Note:__ This package is pre-alpha - tools are added as soon as the functionality is complete. If anything does not work, first re-install the package `install_github("kenhanscombe/ukbtools", build_vignettes = TRUE, dependencies = TRUE, force = TRUE)` to get the latest development version. If it is still not working, [let me know](https://github.com/kenhanscombe/ukbtools/issues) and I'll fix it.

<br>


## Prerequisite: Make a UKB fileset

Download<sup>§</sup> then decrypt your data and create a "UKB fileset" (.tab, .r, .html):

```{bash, eval = FALSE}
ukb_unpack ukbxxxx.enc key
ukb_conv ukbxxxx.enc_ukb r
ukb_conv ukbxxxx.enc_ukb docs

```

`ukb_unpack` decrypts your downloaded `ukbxxxx.enc` file, outputting a `ukbxxxx.enc_ukb` file. `ukb_conv` with the `r` flag converts the decrypted data to a tab-delimited file `ukbxxxx.tab` and an R script `ukbxxxx.r` that reads the tab file. The `docs` flag creates an html file containing a field-code-to-description table (among others).

<br>

<sup>§</sup> Full details of the data download and decrypt process are given in the [Using UK Biobank Data](http://biobank.ctsu.ox.ac.uk/crystal/docs/UsingUKBData.pdf) documentation.

<br>



## Make a UKB dataset

The function `ukb_df()` takes two arguments, the stem of your fileset and the path, and returns a dataframe with usable column names. This will take a few minutes. The rate-limiting step is reading and parsing the code in the UKB-generated .r file - not `ukb_df` per se.


```{r, eval = FALSE}

my_ukb_data <- ukb_df("ukbxxxx")

```


You can also specify the path to your fileset if it is not in the current directory. For example, if your fileset is in a subdirectory of the working directory called data


```{r, eval = FALSE}

my_ukb_data <- ukb_df("ukbxxxx", path = "/full/path/to/my/data/")

```

<br>

__Note:__ You can move the three files in your fileset after creating them with `ukb_conv`, but they should be kept together. `ukb_df()` automatically updates the read call in the R source file to point to the correct directory (the current directory by default, or a directory specified by `path`).

<br>



## Other tools

Other tools in the package are described in the vignette "Explore UK Biobank Data"

```{r, eval = FALSE}

vignette("explore-ukb-data", package = "ukbtools")

```

For a list of all functions

```{r, eval = FALSE}

help(package = "ukbtools")

```
