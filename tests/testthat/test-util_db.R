## Testing database functions

con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")



glucose_data1 <- cgmr::libreview_csv_df(system.file("extdata", package = "cgmr", "Firstname1Lastname1_glucose.csv"))
glucose_data2 <- cgmr::libreview_csv_df(system.file("extdata", package = "cgmr", "Firstname2Lastname2_glucose.csv"))


test_that("First Write works", {
  expect_equal(db_write_table(con, table_name = "sample_table", glucose_data1$glucose_raw ), "Wrote to table for the first time")
  expect_equal(db_write_table(con, table_name = "sample_table", glucose_data1$glucose_raw ), "Already have that serial number F91A8D8B-15FF-4028-A066-F97CD2ED2660")
  expect_equal(db_write_table(con, table_name = "sample_table", head(glucose_data2)$glucose_raw), "wrote 31737 records to sample_table")
})

test_that("food_list contains correct values", {
  expect_equal(db_food_list(1002)[3], "Hu Simple Chocolate")
})

test_that("user_list",{
  expect_equal(db_user_df() %>% filter(user_id == 1234) %>% pull(last_name), "Sprague")
})

test_that("name for user ID", {
  expect_equal(db_name_for_user_id(con, user_id = 1234), "Richard Sprague")
})


test_that("get_table",{
  expect_equal(db_get_table(con, table_name = "non-existent_table"),NULL)
  expect_equal(nrow(db_get_table(con, table_name = "sample_table")),35721)
})

### The following tests depend on a local Postgres database preloaded with correct tables

test_that("user privileges",{
  expect_equal(db_user_privileges(1234), "admin")
  expect_equal(db_user_privileges(), NULL)

})

test_that("firebase works",{
  expect_equal(db_user_id_from_firebase("q0cqXsbigXg0ZaWy5IsC0AhvPCK2"), 1009)
  expect_equal(db_user_id_from_firebase("somethingwrong"),NA)
})

DBI::dbDisconnect(con)
