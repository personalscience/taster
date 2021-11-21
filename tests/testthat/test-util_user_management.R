
con <- db_connection()

user <- UserObject(con)
user2 <- UserObject(con, user_id = 1234)

test_that("null user",{
  expect_equal(user$full_name, "<log in for name>")
  expect_equal(is.na(user$privilege),TRUE)
  expect_equal(user2$full_name, "Richard Sprague")
  expect_equal(user2$privilege , "admin")
})



DBI::dbDisconnect(con)
