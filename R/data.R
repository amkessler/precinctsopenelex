#' Sample precinct data from 2020 presidential election
#'
#' A dataset containing precinct-by-precinct election results in Saratoga County, NY, for the 2020
#' presidential election. Note that New York's unusual rules allow candidates to be listed under more than one
#' party on the ballot, which is why you'll see Trump in there twice under both the Republican and Conservative parties.
#'
#' @format A data frame with 196 rows and 11 variables:
#' \describe{
#'   \item{precinct}{election precinct name}
#'   \item{'Joseph R. Biden - DEM'}{votes for Biden as Democrat}
#'   \item{'Donald J. Trump - REP'}{votes for Trump as Republican}
#'   \item{'Donald J. Trump - CON'}{votes for Trump as Conservative Party}
#'   \item{'Howie Hawkins - GRE'}{votes for Hawkins as Green Party}
#'   \item{'Jo Jorgensen - LIB'}{votes for Jogensen as Libertartian}
#'   \item{'Brock Pierce - IND'}{votes for Pierce as Independence Party}
#'   \item{WriteIn}{votes for write-in candidates}
#'   \item{Blanks}{undervotes, where voter skipped the race and left it blank}
#'   \item{Voids}{overvotes, where voter improperly marked multiple candidates}
#'   ...
#' }
#' @source \url{https://www.saratogacountyny.gov/2020-official-general-election-results/}
"precinctsampledata_ny"

