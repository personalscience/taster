#' user_management



#' @title Create New User Object
#' @description A User object represents all the useful information to know about a user:
#' `user_id` the unique ID used throughout the app. If 0, then the user is unregistered.
#' `firebase_id` if user is logged in, this is their valid Firebase ID.
#' You cannot have a `firebase_id` without a non-zero `user_id`
#' `privilege` char string representation of the privilege level ("admin", "user")
#' @param con valid database connection
#' @param user_id User ID
#' @param firebase_obj Firebase object
#' @return The return value, if any, from executing the utility.
#' @export
UserObject <- function(con, user_id = NULL,  firebase_obj = NULL) {


  thisEnv <- environment()




  ID = if(is.null(user_id)) 0 else user_id

  f_id <- if(!is.null(firebase_obj)){ # we know the user has login information
    current_user <- NULL # firebase_obj$get_signed_in()
    current_user$response$uid
    } else NULL


  db <- list(
    thisEnv = thisEnv,
    con = con,
    f = firebase_obj,
    user_id = ID,
    f_id = f_id,

    #user_list = dplyr::tbl(con, "user_list"),
    firebase_id = dplyr::tbl(con, "accounts_firebase") %>%
      filter(user_id == ID) %>%
      pull(`firebase_id`) %>% first(),

    privilege = dplyr::tbl(con, "accounts_user") %>%
      filter(user_id == ID) %>%
      pull(`privilege`) %>%
      first(),

    full_name = if (ID == 0) "<log in for name>"
    else dplyr::tbl(con, "accounts_user") %>%
      filter(user_id ==ID) %>%
      collect() %>%
      transmute(full_name = sprintf("%s %s", first_name, last_name)) %>%
      pull(full_name) %>%
      first(),

    list_objects = function() {
      objects <- DBI::dbListObjects(con)
      tables <- DBI::dbListTables(con)
      return(list(objects=objects, tables=tables))
    }

  )



  ## Define the value of the list within the current environment.
  assign('this',db,envir=thisEnv)

  structure(db, class = "list",
            con  = con,
            user_id = user_id,
            firebase_obj = firebase_obj)

}

#' @title Registered Users From Database
#' @slot con valid database connection
#' @slot users_df dataframe of users
#' @export
RegisteredUsers <- setClass("RegisteredUsers", slots = c("con" = "DBIConnection",
                                                         "users_df" = "data.frame",
                                                         #"firebase_obj" = "R6Class",
                                                         "glucose_records" = "data.frame",
                                                         "notes_records" = "data.frame"))

#' @describeIn RegisteredUsers initialize
#' @param con database connection (optional: if missing, call \code{db_connection})
#' @param firebase_obj a valid Firebase object
setMethod("initialize", "RegisteredUsers", function(.Object, con = NULL, firebase_obj = NULL){
  .Object@con <- if(is.null(con)) {db_connection()} else con
  .Object@users_df <- dplyr::tbl(.Object@con, "user_list") %>% collect() %>% dplyr::as_tibble()
  .Object@glucose_records <- dplyr::tbl(.Object@con, "glucose_records") %>% collect()
  .Object@notes_records <- dplyr::tbl(.Object@con, "notes_records") %>% collect()
  .Object@firebase_obj <- NULL
  return(.Object)

})

#' @title Set up a new user
#' @slot id User ID
#' @slot name User name
#' @export
#' @return user object
User <- setClass("User",
                 slots = c("id" = "numeric",
                           "name" = "character")
                 )

#' @describeIn User initialize with `id = ` and a name asking user to log in
setMethod("initialize", "User", function(.Object) {
  .Object@id = 0
  .Object@name = "<Must Log In>"
  return(.Object)

})


#' @describeIn User Show basic characteristics of object
setMethod("show", "User", function(object) {
  cat("An object of class", class(object), "\n")
  cat("name=",object@name,"\n")
  cat("id =", object@id, "\n")
})

#' #' @describeIn User Getter for user ID
#' setMethod("get_id", User, function(User) {
#'   User@.Data[["id"]]
#' })

# @title Id number for object
#' @details return the value for `id`
setGeneric("get_id", function(x, ...) {
  standardGeneric("get_id")
})

#' @describeIn User User ID
setMethod(f = "get_id", signature = "User", function(x, ...) {
  x@id

})



#' @title Set up Firebase support
#' @description This is just a stub for now, in order to consolidate all program-wide calls to Firebase.
#' @param con database connection
#' @import firebase
#' @return firebase object
firebase_setup <- function(con) {
  f <- firebase::FirebaseUI$
    new("local")$ # instantiate
    set_providers( # define providers
      email = TRUE,
      google = TRUE
    )$
    set_tos_url("https://ensembio.com/privacy")$
    set_privacy_policy_url("https://ensembio.com/privacy")$
    launch() # launch

  return(f)
}
