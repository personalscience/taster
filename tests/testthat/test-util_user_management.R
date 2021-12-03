
con <- db_connection()
scon  <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

users <- tibble(user_id = c(0,1,2,3),
                first_name = c("a","b","c","d"),
                last_name = c("z","y","x","w"))

fbase <- tibble(user_id = c(0,1,7,8),
                first_name = c("a","b","j",NA_character_),
                last_name = c("z","y","r","s"),
                firebase_id = c("a1","b1", "c1", "d1"))

db_write_table(scon, "user_list", users)
db_write_table(scon, "accounts_firebase", fbase)
db_write_table(scon, "accounts_user", users)

test_that("max user works",{
          expect_equal(user_id_max(scon), 3)
})

test_that("db_replace_user",{
  db_replace_user(scon, list(user_id = 1,
                             first_name = "new_first",
                             last_name = "new_last"))
  expect_equal(tbl(scon, "user_list") %>% filter(user_id == 1) %>% pull(first_name),
               "new_first")
  db_replace_user(scon, list(user_id = 2))
})

test_that("user_find_id works", {
  expect_equal(user_find_id(scon, NULL), NULL)
  expect_equal(user_find_id(scon, list(user_id = 1, firebase_id = NULL)),
               NULL)

  expect_equal(user_find_id(scon, user = list(first_name = "a",  # I know your firebase_id, but not your user_id

                                              firebase_id = "a1")),
               list(first_name = "a", last_name = "z", user_id = 0, firebase_id = "a1"))


  expect_equal(user_find_id(scon, user = list(last_name = "s",  # your first time using this firebase_id; you need a new user_id
                                              firebase_id = "e1")),
               list(first_name = "", last_name = "s", user_id = 4, firebase_id = "e1"))

  expect_equal(user_find_id(scon, user = list(user_id = 0,
                                              firebase_id = NULL)),
               NULL)
  expect_mapequal(user_find_id(scon, user = list(first_name = "Joe",
                                              firebase_id = "new_fb_id" )),
               list(first_name = "Joe", last_name = "", firebase_id = "new_fb_id", user_id = 4))

  expect_equal(user_find_id(con, user = list(first_name = "a",
                                             last_name = "z",
                                             user_id = NULL,
                                             firebase_id = "a1"))$user_id,
               user_id_max(con) + 1)
  expect_equal(user_find_id(con, user = list(user_id = 1234)),
               NULL)

})


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
DBI::dbDisconnect(scon)
