## Testing database functions

scon <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")



glucose_data1 <- cgmr::libreview_csv_df(system.file("extdata", package = "cgmr", "Firstname1Lastname1_glucose.csv"))
glucose_data2 <- cgmr::libreview_csv_df(system.file("extdata", package = "cgmr", "Firstname2Lastname2_glucose.csv"))


test_that("First Write works", {
  expect_equal(db_write_table(scon, table_name = "raw_glucose", glucose_data1$glucose_raw ), "Wrote to table for the first time")
  expect_equal(db_write_table(scon, table_name = "raw_glucose", glucose_data1$glucose_raw ), "Already have that serial number F91A8D8B-15FF-4028-A066-F97CD2ED2660")
  expect_equal(db_write_table(scon, table_name = "raw_glucose", head(glucose_data2)$glucose_raw), "wrote 31737 records to raw_glucose")
})



test_that("get_table",{
  expect_equal(db_get_table(scon, table_name = "non-existent_table"),NULL)
  expect_equal(nrow(db_get_table(scon, table_name = "raw_glucose")),35721)
})



DBI::dbDisconnect(scon)
