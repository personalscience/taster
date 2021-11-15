con <- db_connection()

GLUCOSE_RECORDS<- tbl(con,"glucose_records") %>% collect()
NOTES_RECORDS <- tbl(con, "notes_records") %>% collect()

test_that("build AUC", {
  expect_equal(build_all_AUC(s_list = "Snapeas",
                glucose_records = GLUCOSE_RECORDS,
                notes_records = NOTES_RECORDS)$ave[10], 91.3)
})

