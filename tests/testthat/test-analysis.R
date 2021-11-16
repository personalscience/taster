
# Note: to run these tests, you'll need a valid database connection.
# See README for instructions on how to get one.
con <- db_connection()

GLUCOSE_RECORDS<- db_get_table(con, "glucose_records")
NOTES_RECORDS <- db_get_table(con, "notes_records")

test_that("build AUC", {
  expect_equal(build_all_AUC(s_list = "Snapeas",
                glucose_records = GLUCOSE_RECORDS,
                notes_records = NOTES_RECORDS)$ave[10], 91.3)
})

