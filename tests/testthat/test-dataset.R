context("munging files")

library(ukbtools)

test_that("ukb_df_field() creates a valid field-lookup dataframe", {
    path_to_example_data <- system.file("extdata", package = "ukbtools")
    f <- ukb_df_field("ukbxxxx", path_to_example_data)

    expect_s3_class(f, "data.frame")
    expect_true(all(complete.cases(f)))
    expect_false(all(grepl("[[:punct:]]", f[["col.name"]])))
})

test_that("ukb_df() creates a dataframe", {
    path_to_example_data <- system.file("extdata", package = "ukbtools")
    df <- ukb_df("ukbxxxx", path_to_example_data)

    expect_s3_class(df, "data.frame")
})