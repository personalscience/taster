# very basic sniff tests to ensure the database is configured enough to run the basic app functions


# Note: to run these tests, you'll need a valid database connection.
# See README for instructions on how to get one.
con <- db_connection()


GLUCOSE_RECORDS<- db_get_table(con, "glucose_records")
NOTES_RECORDS <- db_get_table(con, "notes_records")

test_that("glucose range for ID", {
  gr <- glucose_ranges_for_id(user_id = 1234, GLUCOSE_RECORDS)
  expect_equal(as.numeric(gr),c(78.09, 4.99), tolerance = .1 )
})

