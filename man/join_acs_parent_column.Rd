% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/join_acs.R
\name{join_acs_parent_column}
\alias{join_acs_parent_column}
\title{Join parent column titles to ACS data based on parent column ID values}
\usage{
join_acs_parent_column(
  data,
  column_id_col = "column_id",
  column_title_col = "column_title",
  parent_id_col = "parent_column_id",
  suffix = c("", "_parent"),
  na_matches = "never",
  relationship = "many-to-one"
)
}
\arguments{
\item{data}{A data frame with the specified column names. Expected to be
labelled using \code{\link[=label_acs_metadata]{label_acs_metadata()}}.}

\item{column_id_col, column_title_col, parent_id_col}{Column ID, column title,
and parent column ID.}

\item{suffix}{Suffix passed to \code{\link[dplyr:mutate-joins]{dplyr::left_join()}}, Default: \code{c("", "_parent")}}

\item{na_matches}{Should two \code{NA} or two \code{NaN} values match?
\itemize{
\item \code{"na"}, the default, treats two \code{NA} or two \code{NaN} values as equal, like
\code{\%in\%}, \code{\link[=match]{match()}}, and \code{\link[=merge]{merge()}}.
\item \code{"never"} treats two \code{NA} or two \code{NaN} values as different, and will
never match them together or to any other values. This is similar to joins
for database sources and to \code{base::merge(incomparables = NA)}.
}}

\item{relationship}{Handling of the expected relationship between the keys of
\code{x} and \code{y}. If the expectations chosen from the list below are
invalidated, an error is thrown.
\itemize{
\item \code{NULL}, the default, doesn't expect there to be any relationship between
\code{x} and \code{y}. However, for equality joins it will check for a many-to-many
relationship (which is typically unexpected) and will warn if one occurs,
encouraging you to either take a closer look at your inputs or make this
relationship explicit by specifying \code{"many-to-many"}.

See the \emph{Many-to-many relationships} section for more details.
\item \code{"one-to-one"} expects:
\itemize{
\item Each row in \code{x} matches at most 1 row in \code{y}.
\item Each row in \code{y} matches at most 1 row in \code{x}.
}
\item \code{"one-to-many"} expects:
\itemize{
\item Each row in \code{y} matches at most 1 row in \code{x}.
}
\item \code{"many-to-one"} expects:
\itemize{
\item Each row in \code{x} matches at most 1 row in \code{y}.
}
\item \code{"many-to-many"} doesn't perform any relationship checks, but is provided
to allow you to be explicit about this relationship if you know it
exists.
}

\code{relationship} doesn't handle cases where there are zero matches. For that,
see \code{unmatched}.}
}
\value{
A data frame with added parent column title.
}
\description{
\code{\link[=join_acs_parent_column]{join_acs_parent_column()}} uses data labelled with parent_column_id values to
join parent column titles to a data frame of ACS data.
}
