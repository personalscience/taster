% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_db.R
\name{db_replace_records}
\alias{db_replace_records}
\title{Replace All Records in `table_name` that match `user_id`}
\usage{
db_replace_records(con, user_id, table_name, table_df)
}
\arguments{
\item{con}{valid database connection}

\item{user_id}{user ID}

\item{table_name}{character string name of a database table}

\item{table_df}{dataframe of records to substitute}
}
\value{
integer number of rows affected
}
\description{
Replace All Records in `table_name` that match `user_id`
}
