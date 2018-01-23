
# ukbtools 0.10.1

Bug fix:

* `ukb_icd_freq_by`: corrected order by levels of `reference.var` in the optional plot. (order in the default dataframe returned was correct.)

* `ukb_df`: corrected tab file path update in r source file. Specifically, made regular expression more specific (1 case reported of regular expression matching word elsewhere in the source file.). Also, replaced utils::read.delim with readr::read_tsv for faster read, with progress bar.




# ukbtools 0.10.0

New functionality:

* `ukb_icd_freq_by` returns frequency for one or more ICD diagnoses by levels of a reference variable and includes an optional plot

* `ukb_df_full_join` (a thin wrapper around `dplyr::full_join`) recursively called on a list of UKB datasets

* `ukb_df_duplicated_names` to identify duplicated names within a dataset. The variable prefix (constructed from its description), index, and array should make the column name unique. However, typos in UKB documentation that give two variables the do not increment index/array have been observed. You will want to identify these and update them appropriately. We expect the occurrence of such duplicates will be rare.

Updated functionality:

* `ukb_icd_diagnosis` now takes one or more individual ids and returns a dataframe with a potential message noting ids with no diagnoses

* `ukb_icd_keyword` accepts a character vector of one or more "keywords" and returns all ICD descriptions including any of the keywords




# ukbtools 0.9.0

* beta release to CRAN. Feature complete but may contain unknown bugs.
