## Testing database functions

con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

glucose_data1 <- cgmr::libreview_csv_df(system.file("extdata", package = "cgmr", "Firstname1Lastname1_glucose.csv"))
glucose_data2 <- cgmr::libreview_csv_df(system.file("extdata", package = "cgmr", "Firstname2Lastname2_glucose.csv"))


test_that("First Write works", {
  expect_equal(db_write_table(con, table_name = "sample_table", glucose_data1$glucose_raw ), "Wrote to table for the first time")
  expect_equal(db_write_table(con, table_name = "sample_table", glucose_data1$glucose_raw ), "Already have that serial number F91A8D8B-15FF-4028-A066-F97CD2ED2660")
  expect_equal(db_write_table(con, table_name = "sample_table", head(glucose_data2)$glucose_raw), "wrote 31737 records to sample_table")
})

DBI::dbDisconnect(con)
