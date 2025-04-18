% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_acs_ext.R
\name{get_acs_tables}
\alias{get_acs_tables}
\alias{get_acs_geographies}
\alias{get_acs_geography}
\title{Get multiple tables or multiple geographies of ACS data}
\usage{
get_acs_tables(
  geography,
  table = NULL,
  cache_table = TRUE,
  year = 2022,
  survey = "acs5",
  variables = NULL,
  moe_level = 90,
  ...,
  crs = NULL,
  label = TRUE,
  perc = TRUE,
  reliability = FALSE,
  keep_geography = TRUE,
  geoid_col = "GEOID",
  quiet = FALSE,
  call = caller_env()
)

get_acs_geographies(
  geography = c("county", "state"),
  variables = NULL,
  table = NULL,
  cache_table = TRUE,
  year = 2022,
  state = NULL,
  county = NULL,
  msa = NULL,
  survey = "acs5",
  ...,
  label = TRUE,
  perc = TRUE,
  geoid_col = "GEOID",
  quiet = FALSE
)

get_acs_geography(
  geography,
  variables = NULL,
  table = NULL,
  cache_table = TRUE,
  year = 2022,
  state = NULL,
  county = NULL,
  msa = NULL,
  survey = "acs5",
  ...,
  label = TRUE,
  perc = TRUE,
  geoid_col = "GEOID",
  call = caller_env()
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

\item{table}{A character vector of tables.}

\item{cache_table}{Whether or not to cache table names for faster future
access. Defaults to FALSE; if TRUE, only needs to be called once per
dataset.  If variables dataset is already cached via the
\code{load_variables} function, this can be bypassed.}

\item{year}{The year, or endyear, of the ACS sample. 5-year ACS data is
available from 2009 through 2023; 1-year ACS data is available from 2005
through 2023, with the exception of 2020.  Defaults to 2023.}

\item{survey}{The ACS contains one-year, three-year, and five-year surveys
expressed as "acs1", "acs3", and "acs5". The default selection is "acs5."}

\item{variables}{Character string or vector of character strings of variable
IDs. tidycensus automatically returns the estimate and the margin of error
associated with the variable.}

\item{moe_level}{The confidence level of the returned margin of error.  One
of 90 (the default), 95, or 99.}

\item{...}{
  Arguments passed on to \code{\link[tidycensus:get_acs]{tidycensus::get_acs}}
  \describe{
    \item{\code{output}}{One of "tidy" (the default) in which each row represents an
enumeration unit-variable combination, or "wide" in which each row
represents an enumeration unit and the variables are in the columns.}
    \item{\code{zcta}}{The zip code tabulation area(s) for which you are requesting
data. Specify a single value or a vector of values to get data for more
than one ZCTA. Numeric or character ZCTA GEOIDs are accepted. When
specifying ZCTAs, geography must be set to `"zcta"` and `state` must be specified with
`county` left as `NULL`. Defaults to NULL.}
    \item{\code{geometry}}{if FALSE (the default), return a regular tibble of ACS data.
if TRUE, uses the tigris package to return an sf tibble with simple feature
geometry in the `geometry` column.}
    \item{\code{keep_geo_vars}}{if TRUE, keeps all the variables from the Census
shapefile obtained by tigris.  Defaults to FALSE.}
    \item{\code{shift_geo}}{(deprecated) if TRUE, returns geometry with Alaska and Hawaii shifted for
thematic mapping of the entire US. Geometry was originally obtained from
the albersusa R package. As of May 2021, we recommend using \code{tigris::shift_geometry()}
instead.}
    \item{\code{summary_var}}{Character string of a "summary variable" from the ACS to
be included in your output. Usually a variable (e.g. total population) that
you'll want to use as a denominator or comparison.}
    \item{\code{key}}{Your Census API key. Obtain one at
\url{https://api.census.gov/data/key_signup.html}}
    \item{\code{show_call}}{if TRUE, display call made to Census API. This can be very
useful in debugging and determining if error messages returned are due to
tidycensus or the Census API. Copy to the API call into a browser and see
what is returned by the API directly. Defaults to FALSE.}
  }}

\item{crs}{Coordinate reference system to use for returned sf tibble when
\code{geometry = TRUE} is passed to \code{\link[tidycensus:get_acs]{tidycensus::get_acs()}}. Defaults to \code{NULL}.}

\item{label}{If \code{TRUE} (default), label the returned ACS data with
\code{\link[=label_acs_metadata]{label_acs_metadata()}} before returning the data frame.}

\item{perc}{If \code{TRUE} (default), use the denominator column ID to calculate
each estimate as a percent share of the denominator value and use
\code{\link[tidycensus:moe_prop]{tidycensus::moe_prop()}} to calculate a new margin of error for the percent
estimate.}

\item{reliability}{If \code{TRUE}, use \code{\link[=assign_acs_reliability]{assign_acs_reliability()}} to assign a
reliability value to estimate values based on the specified \code{moe_level}.}

\item{keep_geography}{If \code{TRUE} (default), bind geography and any supplied
county or state columns to the returned data frame.}

\item{geoid_col}{A GeoID column name to use if perc is \code{TRUE}, Defaults to
'GEOID'.}

\item{quiet}{If \code{FALSE} (default), leave \code{cli.default_handler} option
unchanged. If \code{TRUE}, set \code{cli.default_handler} to \link{suppressMessages}
temporarily with \code{\link[rlang:local_options]{rlang::local_options()}}}

\item{call}{The execution environment of a currently
running function, e.g. \code{caller_env()}. The function will be
mentioned in error messages as the source of the error. See the
\code{call} argument of \code{\link[rlang:abort]{abort()}} for more information.}

\item{state}{An optional vector of states for which you are requesting data.
State names, postal codes, and FIPS codes are accepted. Defaults to NULL.}

\item{county}{The county for which you are requesting data. County names and
FIPS codes are accepted. Must be combined with a value supplied to `state`.
Defaults to NULL.}

\item{msa}{Name or GeoID of a metro area that should be filtered from the
overall list of metro areas returned when geography or geographies  is
"metropolitan/micropolitan statistical area", "cbsa", or "metropolitan
statistical area/micropolitan statistical area".}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}
These functions wrap \code{\link[tidycensus:get_acs]{tidycensus::get_acs()}} and \code{\link[=label_acs_metadata]{label_acs_metadata()}} to
support downloading multiple tables and combining tables into a single data
frame or downloading data for multiple geographies. Note that while the
Census API does not have a specific rate or request limit when using a Census
API key, using these functions with a large number of tables or geographies
may result in errors or failed requests.

CRAN policies require that tidycensus avoid caching by default, however, this
package sets \code{cache_table = TRUE} by default to avoid unecessary load on the
Census API.
}
\examples{
\dontrun{
if (interactive()) {
  get_acs_tables(
    geography = "county",
    county = "Baltimore city",
    state = "MD",
    table = c("B01003", "B19013")
  )

  get_acs_geographies(
    geography = c("county", "state"),
    state = "MD",
    table = c("B01003", "B19013")
  )
}
}
}
