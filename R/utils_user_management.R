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
    },
    set_id = function(new_id){
      this$user_id <- new_id
    },

    print = function() {
      sprintf("Object User: user_id = %s, full_name = %s\n",this$user_id, db$full_name)
    }

  )



  ## Define the value of the list within the current environment.
  assign('this',db,envir=thisEnv)

  structure(db, class = "list",
            con  = con,
            user_id = user_id,
            firebase_obj = firebase_obj,
            print = print)

}

#' @title Print this user
#' @param userObject user object
print_user <- function(userObj) {

  if(is.null(userObj)) return("userObj: <NULL>") else
    return(
  sprintf("User: user_id = %s, full_name = %s, firebase ID = %s\n",
          if(!is.null(userObj[["user_id"]]))
             userObj[["user_id"]] else "<NULL>",
          if(!is.null(userObj[["full_name"]]))
             userObj[["full_name"]] else "<NULL>",
          if(!is.null(userObj[["firebase_id"]]))
            userObj[["firebase_id"]] else "<no firebase ID>"
  )
    )

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
#' See configuration instructions \url{https://firebase.john-coene.com/guide/config/}
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

#' @title Max User ID
#' @description Useful for generating a new `user_id`, this returns the current maximum.
#' @param con valid database connection
user_id_max <- function(con) {

   # Native R code for generalizability.  I use the SQL call for speed reasons.
   # max_id <- tbl(con, "user_list") %>%
   #  filter(user_id == max(user_id, na.rm = TRUE)) %>%
   #  pull(user_id)

  s <- 'SELECT max("user_id") FROM user_list;'

  max_id <- DBI::dbGetQuery(con, s) %>% pull(1)
  return(max_id)
}


#' @title Find Unique `user_id`, Creating One If Necessary
#' @description Intended to be called after someone has firebase credentials, either
#' because they are an existing user who is logging in again, or because they are a brand new
#' user. Either way, this function will return a list that can uniquely identify what
#' we know about that user.  Besides a unique `user_id`, the list will include items
#' for the first name, last name, firebase id, and other information.
#'
#' Ultimately this function should return a user object.
#' @param con valid database connection
#' @param user list containing information needed to set up a new user (either `user_id`, or`firebase_id`)
#' @examples
#' con <- db_connection()
#' me <- list(user_id = 1234, first_name = "Richard", last_name = "Sprague", firebase_id = "769d1YgcNfTy4rQlxTuMqWR0b3t2")
#' u = list(first_name = "a",
#'          last_name = "z",
#'          user_id = NULL,
#'          firebase_id = "a1")
#'
#' user_find_id(con, u)
#' @return list that can uniquely identify the user, including new `user_id` if necessary
user_find_id <- function(con, user) {

  if (!is.list(user)) {
    return(NULL)
  }
  if (is.null(user$firebase_id)){
    return(NULL)
  }

  first_name = if(is.null(user$first_name)) "" else user$first_name
  last_name = if(is.null(user$last_name)) "" else user$last_name


  if (is.null(user$user_id)) { # no user_id for user, so look up firebase_id and return a new user_id if not found.
    uf <- user$firebase_id
    f_id <- tbl(con, "accounts_firebase") %>% filter(firebase_id == uf) %>% count() %>% pull(1)
    user$user_id <- if(f_id == 0) user_id_max(con) + 1 else {
      tbl(con, "accounts_firebase") %>%
        filter(firebase_id == uf) %>%
        pull(user_id)
      }}
  #  at this point, user$user_id exists in accounts_firebase
  # but it's possible user$user_id is a new id and doesn't exist in accounts_users
  ID <- user$user_id
      user_record <- tbl(con, "accounts_user") %>% filter(user_id == ID) %>% collect()
      if(nrow(user_record)>0) { # it's a new user_id so there's no record of it yet
      first_name <- if(is.null(user_record$first_name)) "" else user_record$first_name
      last_name <- if(is.null(user_record$last_name)) "" else user_record$last_name
      }
      message(sprintf("user_find_id found user_id = %s user_record = %s\n",user$user_id, user_record))


  return(list(first_name = first_name, last_name = last_name, user_id = user$user_id, firebase_id = user$firebase_id))
}


#' @title Current User
#' @description Intended to be called as a reactive, this function requires a currently-running firebase instance
#' and will not respond unless the user is logged in.  If logged in, then return the current user_id
#' @param user_object User object in list form (e.g. list(con = db_connection(), firebase_id = firebase_setup())
#' @return user_id user ID
util_current_user<- function(user_object) {
message("util_current_user")
  con <- user_object[["con"]]
  f <- user_object[["firebase_id"]]

  user <- f$get_signed_in()
  if(is.null(user)) {
    message("user_id is null")
    user_id <- 0
    username <- "<must sign in to see name>"
  }
  else {
    message("User is signed in")
    f_id <- db_user_id_from_firebase(con, user$response$uid)
    user_id <- if(is.na(f_id)) 0 else f_id  # if user isn't registered return user_id = 0

    cat(file=stderr(),sprintf("\nUser %s is signed in\n",user_id))

    username <- db_name_for_user_id(con, f, user_id)
  }


  current_id <- list(id=if(is.null(user_id)) 0 else as.numeric(user_id), name = username)
  message("current ID=",current_id)
  return(current_id)
}

