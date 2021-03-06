% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/balance_tbl.R
\name{balance_tbl}
\alias{balance_tbl}
\title{Create a balance table usially needed for the course Statistics II at the Hertie School}
\usage{
balance_tbl(data, treatment)
}
\arguments{
\item{data}{a dataframe containing only a binary treatment variable and a series of covariates to be included in the table}

\item{treatment}{the treatment variable in the provided dataframe}
}
\value{
table
}
\description{
Creates a well-formatted table containing the mean values of a set of covariates for treatment and control groups.
}
