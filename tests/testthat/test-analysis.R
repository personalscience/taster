
# Note: to run these tests, you'll need a valid database connection.
# See README for instructions on how to get one.
con <- db_connection()

cgm_data <- CgmObject(con)

test_that("build AUC", {
  expect_equal(build_all_AUC(s_list = "Snapeas",cgm_data)$ave[10], 91.3)
})

test_that("food_list contains correct values", {
  expect_equal(db_food_list(con, 1002)[3], "Hu Simple Chocolate")
})


DBI::dbDisconnect(con)
