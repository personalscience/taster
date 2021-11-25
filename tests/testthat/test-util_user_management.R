
con <- db_connection()

user <- UserObject(con)
user2 <- UserObject(con, user_id = 1234)

test_that("null user",{
  expect_equal(user$full_name, "<log in for name>")
  expect_equal(is.na(user$privilege),TRUE)
  expect_equal(user2$full_name, "Richard Sprague")
  expect_equal(user2$privilege , "admin")
})

test_that("user_list",{
  expect_equal(db_user_df() %>% filter(user_id == 1234) %>% pull(last_name), "Sprague")
})

test_that("name for user ID", {
  expect_equal(db_name_for_user_id(con, user_id = 1234), "Richard Sprague")
})

### The following tests depend on a local Postgres database preloaded with correct tables

test_that("user privileges",{
  expect_equal(db_user_privileges(con, 1234), "admin")
  expect_equal(db_user_privileges(con), NULL)

})

test_that("users are visible depending on privileges", {
  expect_equal(db_users_visible(con, -1), c(0, 1234,-1))
})

test_that("firebase works",{
  expect_equal(db_user_id_from_firebase(con, "q0cqXsbigXg0ZaWy5IsC0AhvPCK2"), 1009)
  expect_equal(db_user_id_from_firebase(con, "somethingwrong"),NA)
})


DBI::dbDisconnect(con)
