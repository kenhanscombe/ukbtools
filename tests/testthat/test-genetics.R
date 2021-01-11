context("genetics qc")

make_related <- function(env = parent.frame()) {
    # KING kinship coefficient:
    # >0.354 duplicate/MZ twin
    # [0.177, 0.354] 1st-degree
    # [0.0884, 0.177] 2nd-degree
    # [0.0442, 0.0884] 3rd-degree
    df <- tibble(
        ID1 = 1:100,
        ID2 = 101:200,
        HetHet = 0,
        IBS0 = 0,
        Kinship = runif(100, min = 0, max = 0.5)
    )

    withr::defer(rm(df), envir = env)
}

test_that("ukb_gen_related_with_data() returns a dataframe for `eid` subset", {
    rel <- make_related()
    rel_with_data <- ukb_gen_related_with_data(
        rel, as.character(c(sample(rel$ID1, 15), sample(rel$ID2, 15))))
    expect_is(rel_with_data, "data.frame")
})
