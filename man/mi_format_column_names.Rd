% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/michigan_specific_functions.R
\name{mi_format_column_names}
\alias{mi_format_column_names}
\title{Michigan-specific Function to Rename Columns}
\usage{
mi_format_column_names(df)
}
\arguments{
\item{df}{a dataframe}
}
\value{
returns a dataframe with renamed columns and completely empty columns removed
}
\description{
This function takes a dataframe containing precinct data with the original Michigan candidate/party headers and
converts the headers to proper formatting to use with package's processing functions. It is tailored
to work with the way Michigan counties' format of "Candidate (PARTY)" and also incorporates step to remove empty columns.
}
\examples{
\dontrun{
mi_format_column_names(mydataframe)
}
}
