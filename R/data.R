#' Precinct results from 2020 presidential election in Saratoga County, NY.
#'
#' A dataset containing precinct-by-precinct election results. Note that NY rules allow
#' candidates to be listed under more than one party on the ballot.
#'
#' @format A data frame with 196 rows and 11 variables:
#' \describe{
#'   \item{precinct}{election precinct name}
#'   \item{'Joseph R. Biden - DEM'}{number of votes}
#'   \item{'Donald J. Trump - REP'}{number of votes}
#'   \item{'Donald J. Trump - CON'}{number of votes}
#'   \item{'Howie Hawkins - GRE'}{number of votes}
#'   \item{'Jo Jorgensen - LIB'}{number of votes}
#'   \item{'Brock Pierce - IND'}{number of votes}
#'   \item{WriteIn}{number of write in votes}
#'   \item{Blanks}{number of undervotes, where voter skipped race}
#'   \item{Voids}{number of overvotes, where voter marked multiple candidates}
#'   ...
#' }
#' @source \url{https://www.saratogacountyny.gov/2020-official-general-election-results/}
"precinctsampledata_ny"
