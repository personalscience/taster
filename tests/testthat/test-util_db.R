## Testing database functions


# dittodb::start_db_capturing()
# out <- tbl(con2, "glucose_records") %>% filter(user_id == 1234 & value > 150) %>% collect()
# dittodb::stop_db_capturing()
dittodb::with_mock_db(
  test_that("glucose records", {
    con2 <- db_connection()
    on.exit(dbDisconnect(con2))
    out <- tbl(con2, "glucose_records") %>%
      filter(user_id == 1234 & value > 150) %>% collect()

    expect_equal(dim(out), c(461, 7))
  })
)

scon <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")



glucose_data1 <- cgmr::libreview_csv_df(system.file("extdata", package = "cgmr", "Firstname1Lastname1_glucose.csv"))
glucose_data2 <- cgmr::libreview_csv_df(system.file("extdata", package = "cgmr", "Firstname2Lastname2_glucose.csv"))
sample_table <- tibble(id = c(1,2,3),
                       name = c("a","b","c"),
                       user_id = c(7,8,9),
                       firebase_id = c("x","y","z"))

new_user_records <- tibble(# id = c(1),
                           name = c("m"),
                           user_id = c(7),
                           firebase_id = c("x"))

raw_notes_records <- tibble(Start = c(20,21,22),
                            End = c(30,NA,NA),
                            Comment = c("a","b","c"),
                            Z = c(8,9,10),
                            user_id = c(555,555,555))
new_notes_records <- tibble(Start = c(23,24),
                            End = c(31,32),
                            Comment = c("d","e"),
                            Z = c(6,7),
                            user_id = c(655,655))


test_that("First Write works", {
  expect_equal(db_write_table(scon, table_name = "raw_glucose", glucose_data1$glucose_raw ), "Wrote to table for the first time")
  expect_equal(db_write_table(scon, table_name = "sample_table", sample_table), "Wrote to table for the first time")
  expect_equal(db_write_table(scon, table_name = "sample_table", tibble(name = c("x","y"),
                                                                        user_id = c(15,-1))),
               "Wrote to table with result = 2")
  expect_equal(db_write_table(scon, table_name = "accounts_firebase", sample_table), "Wrote to table for the first time")
  expect_equal(db_write_table(scon, table_name = "accounts_firebase", tibble(name = c("x"),
                                                                             firebase_id = c("x"))),
               "Firebase id already exists")
  expect_equal(db_write_table(scon, table_name = "accounts_firebase", tibble(name = c("x"),
                                                                             firebase_id = c("w"))),
               "Wrote to table with result = 1")
  expect_equal(db_write_table(scon, table_name = "raw_glucose", glucose_data1$glucose_raw ), "Already have that serial number F91A8D8B-15FF-4028-A066-F97CD2ED2660")
  expect_equal(db_write_table(scon, table_name = "raw_glucose", head(glucose_data2)$glucose_raw), "wrote 31737 records to raw_glucose")
})


# delete 2 records that match the `user_id` vector  and replace them with `new_user_records`
test_that("Replace Records works",{
  expect_equal(db_replace_records(scon, user_id = c(1,2,7,9), "sample_table", new_user_records),
               2) # found 2 records to drop
  expect_equal(tbl(scon, "sample_table") %>% filter(user_id == 7) %>% pull(name),
               "m")
})
test_that("Replace Records: user_id = NULL",{
  expect_equal(db_replace_records(scon, user_id = NULL, "sample_table", new_user_records),
               0)
})

test_that("Replace Records: no such user id",{
  new_user_records$user_id <- 21
  new_user_records$name <- "newname"
  expect_equal(db_replace_records(scon, user_id = c(21), "sample_table", new_user_records),
               0) # no such user_id
})

test_that("Replace Records: correct new data in sample table",{
  expect_equal(tbl(scon, "sample_table") %>% pull(name), c("b","x","y","m","newname"))
})

print(tbl(scon, "sample_table"))

test_that("get_table",{
  expect_equal(db_get_table(scon, table_name = "non-existent_table"),NULL)
  expect_equal(nrow(db_get_table(scon, table_name = "raw_glucose")),35721)
  expect_equal(db_get_table(scon, table_name = "accounts_firebase")[["firebase_id"]], c("x","y","z","w"))
})

test_that("raw notes table", {
  expect_equal(db_write_notes_table(scon, "raw_notes", raw_notes_records, user_id = 555), "Wrote to table with result = TRUE")
  expect_equal(db_write_notes_table(scon, "raw_notes", new_notes_records, user_id = 555), "Wrote to table with result = 3")
  expect_equal(tbl(scon, "raw_notes") %>% collect() %>% pull(4), c(6,7))
  expect_equal(names(tbl(scon,"raw_notes") %>% collect()), c("Start","End","Comment","Z", "user_id"))
})
print(tbl(scon,"raw_notes"))



DBI::dbDisconnect(scon)
