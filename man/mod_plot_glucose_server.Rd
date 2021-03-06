% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_plot_glucose.R
\name{mod_plot_glucose_server}
\alias{mod_plot_glucose_server}
\title{Plot a glucose chart - server}
\usage{
mod_plot_glucose_server(id, glucose_df, con)
}
\arguments{
\item{id}{shiny module id}

\item{glucose_df}{reactive for a valid glucose dataframe}

\item{con}{database connection}
}
\value{
ggplot object representing a glucose chart
}
\description{
Given a (reactive) libreview dataframe, this Shiny module will
generate a valid ggplot object and display it in an accompanying UI
}
