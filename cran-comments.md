
## Example data

I've left a couple of examples to run, e.g., some of the ICD functions that query publicly available data that I've incorporated as queryable datasets. Most functions however rely on a "UKB dataset" created with ukb_df. ukb_df itself requires raw data (files) from a approved UK Biobank study so this is not feasible. The UK Biobank is very sensitive data (primary demographic data, genetic data, etc.). It would not be easy to generate example data for all the variables required, that is representative of the UK Biobank, without making the UK Biobank and it's participants very uncomfortable.




## Test environments

* local OS X El Capitan 10.11.6, R 3.4.0
* win-builder (devel and release)




## R CMD check results

#### local OS X El Capitan 10.11.6, R 3.4.0

Status: OK

R CMD check results
0 errors | 0 warnings | 0 notes



#### win-builder (devel and release)

R-release
* using R version 3.4.0 (2017-04-21)
* using platform: x86_64-w64-mingw32 (64-bit)

R-devel
* using R Under development (unstable) (2017-06-28 r72861)
* using platform: x86_64-w64-mingw32 (64-bit)


Status: 1 NOTE

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Ken Hanscombe <ken.hanscombe@gmail.com>'

New submission

Possibly mis-spelled words in DESCRIPTION:
  Biobank (3:34, 6:44)
  ICD (7:67)
  UKB (6:97)
  dataset (6:82)
  fileset (6:101)
  html (6:121)
  metadata (8:22)
  
  


## Downstream dependencies

There are currently no downstream dependencies for this package
