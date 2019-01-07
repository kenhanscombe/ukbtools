
# ukbtools 0.11.0.9000

_Development version_

Test data:

* Added example raw data ukbXXXX.tab, ukbXXXX.r, ukbXXXX.html to test the 'read'
and 'summarise' functionality `ukb_df`, `ukb_df_field`, and `ukb_context`.



# ukbtools 0.11.0

Updated functionality:

* `ukb_df`: Replaced `readr::read_tsv` with `data.table::fread` for faster read. Also includes an `n_threads` argument passed to `data.table::fread`, which may make read faster. Column names now include field code to ensure names are unique (UK Biobank sometimes use the same description for more than one variable)

Defunct functionality:

* Added defunct message to `ukb_gen_meta`, `ukb_gen_pcs`, `ukb_gen_excl`, `ukb_gen_rel`, `ukb_gen_het`, `ukb_gen_excl_to_na`, and `ukb_gen_write_plink_excl`. `ukb_defunct` explains why these have become defunct and where to get UK Biobank genetic (meta)data.

New functionality:

* Since the UKB changed the way they serve up genetic metadata, the following work with files described in UKB Resource 531: `ukb_gen_sqc_names` supplies column names for the separately downloaded sample QC file; `ukb_gen_rel_count` does the same as before (a count of levels of relatedness or a plot) but with separately downloaded relatedness data; `ukb_gen_related_with_data` returns subset of relatedness data in which both IDs have data on a phenotype of interest; `ukb_gen_samples_to_remove` returns a list of individuals to exclude in order to remove relatedness (one possible solution to a maximal subset problem).




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
