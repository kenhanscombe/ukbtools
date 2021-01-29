context("genetics qc")


test_that("ukb_gen_related_with_data() returns a dataframe for `eid` subset", {
    df <- tibble(
        ID1 = 1:100,
        ID2 = 101:200,
        HetHet = 0,
        IBS0 = 0,
        Kinship = runif(100, min = 0, max = 0.5)
    )

    with_data <- as.character(c(sample(df$ID1, 15), sample(df$ID2, 15)))

    rel_with_data <- ukb_gen_related_with_data(
        df,
        ukb_with_data = with_data
    )
    expect_is(rel_with_data, "data.frame")
})