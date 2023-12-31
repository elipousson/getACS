% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_acs_ts.R
\name{get_acs_ts}
\alias{get_acs_ts}
\title{Get multiple years of ACS data for time series analysis}
\usage{
get_acs_ts(
  geography,
  variables = NULL,
  table = NULL,
  cache_table = TRUE,
  year = 2022,
  state = NULL,
  county = NULL,
  survey = "acs5",
  ...,
  quiet = FALSE
)
}
\arguments{
\item{geography}{Required character vector of one or more geographies. See
\url{https://walker-data.com/tidycensus/articles/basic-usage.html#geography-in-tidycensus}
for supported options. Defaults to \code{c("county", "state")} for
\code{\link[=get_acs_geographies]{get_acs_geographies()}}. If a supplied geography does not support county
and state parameters, these options are dropped before calling
\code{\link[tidycensus:get_acs]{tidycensus::get_acs()}}. Any required parameters are also bound to the
returned data frame as new columns.}

\item{variables}{Character string or vector of character strings of variable
IDs. tidycensus automatically returns the estimate and the margin of error
associated with the variable.}

\item{table}{A character vector of tables.}

\item{cache_table}{Whether or not to cache table names for faster future
access. Defaults to FALSE; if TRUE, only needs to be called once per
dataset.  If variables dataset is already cached via the
\code{load_variables} function, this can be bypassed.}

\item{year}{A numeric vector of years. If length 1, the function uses
\code{\link[=acs_survey_ts]{acs_survey_ts()}} to get data for all comparable survey years back to the
start of the ACS. This is the recommended approach for using
\code{\link[=get_acs_ts]{get_acs_ts()}}. If length is greater than 1, return the selected years even
if those years may not be valid to compare.}

\item{state}{An optional vector of states for which you are requesting data.
State names, postal codes, and FIPS codes are accepted. Defaults to NULL.}

\item{county}{The county for which you are requesting data. County names and
FIPS codes are accepted. Must be combined with a value supplied to `state`.
Defaults to NULL.}

\item{survey}{The ACS contains one-year, three-year, and five-year surveys
expressed as "acs1", "acs3", and "acs5". The default selection is "acs5."}

\item{...}{Other keyword arguments}

\item{quiet}{If \code{FALSE} (default), leave \code{cli.default_handler} option
unchanged. If \code{TRUE}, set \code{cli.default_handler} to \link{suppressMessages}
temporarily with \code{\link[rlang:local_options]{rlang::local_options()}}}
}
\value{
A data frame or sf object.
}
\description{
\code{\link[=get_acs_ts]{get_acs_ts()}} is a variant on \code{\link[=get_acs_geographies]{get_acs_geographies()}} that supports
downloading data for multiple years in addition to multiple tables or
multiple geographies. The year is appended as an additional column in the
returned data frame. The intended use is to provide the latest year needed
and the function will download data for all non-overlapping survey periods.
For example, 2021 ACS data using the 5-year sample can be compared to 5-year
data from 2016 and 2011. Not all variables can be compared across different
years and caution is recommended when using ACS data for time series
analysis.
}
