
# ukbtools: A set of tools to manipulate UK Biobank data

`ukbtools` is a set of convenience functions for 

- creating a single UKB dataset for analysis
- querying diagnoses and retreiving diagnostic summaries
- extracting genetic metadata for genetic analyses
- exploring properties of a sample subset relative to the full UKB sample, or a reference subset


## Overview

After downloading and decrypting your UK Biobank (UKB) data with the supplied UKB programs (http://biobank.ctsu.ox.ac.uk/crystal/docs/UsingUKBData.pdf), you have multiple files that need to be brought together to give you a tidy dataset to explore. The data file has column names that are the field codes from the UKB data showcase. `ukbtools` provides tools to tidy up the naming of the variables and perform exploratory analysis.

<br>
<br>

## Prerequisites: Making a UKB fileset

Download^§^ then decrypt your data and create a UKB fileset (.tab, .r, .html):

```{bash, eval = FALSE}
ukb_unpack ukbxxxx.enc key
ukb_conv ukbxxxx.enc_ukb r
ukb_conv ukbxxxx.enc_ukb docs

```

`ukb_unpack` decrypts your downloaded `ukbxxxx.enc` file, outputting a `ukbxxxx.enc_ukb` file. `ukb_conv` with the `r` flag converts the decrypted data to a tab-delimited file `ukbxxxx.tab` and an R script `ukbxxxx.r` that reads the tab file. The `docs` flag creates an html file containing a field-code-to-description table (among others).

<br>

^§^ Full details of the data download and decrypt process are given in the _Using UK Biobank Data_ documentation (http://biobank.ctsu.ox.ac.uk/crystal/docs/UsingUKBData.pdf).

<br>
<br>


## Making a UKB dataset

The function `ukb_df()` takes two arguments, the stem of your fileset and the path, and returns a dataframe with usable column names.


```{r, eval = FALSE}

my_ukb_data <- ukb_df("ukbxxxx")

```


You can also specify the path to your fileset if it is not in the current directory. For example, if your fileset is in a subdirectory of the working directory called data


```{r, eval = FALSE}

my_ukb_data <- ukb_df("ukbxxxx", path = "./data/")

```

<br>

__Note:__ You can move the three files in your fileset after creating them with `ukb_conv`, but they should be kept together. `ukb_df()` automatically updates the read call in the R source file to point to the correct directory (the current directly by default, or the directory specified by `path`).


## Other tools

Other tools in the package are described in the vignette "Explore UK Biobank Data"
