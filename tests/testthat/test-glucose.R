# very basic sniff tests to ensure the database is configured enough to run the basic app functions

con <- db_connection()

GLUCOSE_RECORDS<- tbl(con,"glucose_records") %>% collect()
NOTES_RECORDS <- tbl(con, "notes_records") %>% collect()

test_that("glucose range for ID", {
  gr <- glucose_ranges_for_id(user_id = 1234, GLUCOSE_RECORDS)
  expect_equal(as.numeric(gr),c(78.09, 4.99), tolerance = .1 )
})

test_that("food_list_db", {
  expect_equal(food_list_db(1002)[3], "Hu Simple Chocolate")
})
