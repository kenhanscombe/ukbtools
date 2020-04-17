context("Defunct genetic metadata functionality")

test_that("defunct error produced", {
  lifecycle::expect_defunct(ukb_gen_meta())
  lifecycle::expect_defunct(ukb_gen_pcs())
  lifecycle::expect_defunct(ukb_gen_excl())
  lifecycle::expect_defunct(ukb_gen_rel())
  lifecycle::expect_defunct(ukb_gen_het())
  lifecycle::expect_defunct(ukb_gen_excl_to_na())
  lifecycle::expect_defunct(ukb_gen_write_plink_excl())
})
