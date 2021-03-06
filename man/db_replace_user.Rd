% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_db.R
\name{db_replace_user}
\alias{db_replace_user}
\title{Replace User Information in Database}
\usage{
db_replace_user(con, user)
}
\arguments{
\item{con}{valid database connection}

\item{user}{list representing user object}
}
\value{
logical does it succeed or fail?
}
\description{
Find the user matching this `user_id` and replace
with the other values in `user`
}
