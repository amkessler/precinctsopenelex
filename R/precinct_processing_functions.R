#' Create File Name String for Importing Data
#'
#' Can be used to build a file string specific to a county, based on the standardized folder/file format
#' "NY_Saratoga/NY_Saratoga_GE20_cleaned.xlsx"
#'
#' @param stateabbr two-letter state abbreviation (e.g. "NY")
#' @param countyname county name where the precincts located (e.g. "Saratoga")
#'
#' @return a text string
#' @export
#'
#' @examples
#' create_infile_string("NY", "Saratoga")
create_infile_string <- function(stateabbr, countyname) {
  in_name <- paste0(
    stateabbr,
    "_",
    countyname,
    "/",
    stateabbr,
    "_",
    countyname,
    "_GE20_cleaned.xlsx"
  )
  return(in_name)
}

#' Reshaping County Precinct Data to OpenElex Format
#'
#' This function is designed to take a dataset ready to be reshaped once the initial cleanup steps are taken.
#' The file will have a precinct column, followed by columns with vote results parsed from the "candidate - party" field
#'
#' @param df formatted dataframe of county precinct-level results
#' @param office which office the results refer to (e.g. presidential, U.S. House, State Senate, etc.)
#' @param district number for what district is associated with the office (e.g. "45" for the 45th congressional district. note presidential should be left blank as "")
#'
#'
#' @return a reshaped dataframe in openelex long/tidy format
#' @export
#' @import dplyr
#' @import tidyr
#' @import stringr
#'
#' @examples
#' \dontrun{#' reshape_precinct_data(dataframe)}
reshape_precinct_data <- function(df, office, district){
  #determine how many columns, since races can have diff number of candidates
  colnum <- length(colnames(df))
  #begin processing dataset
  df <- df %>%
    #transform to long/tidy format
    pivot_longer(cols = 2:all_of(colnum), names_to = "name", values_to = "votes") %>%
    #clean and add necessary columns
    mutate(
      temp = str_split(name, " - ", simplify = TRUE),
      candidate = temp[, 1],
      party = temp[, 2],
      office = office,
      district = district,
      candidate = str_replace_all(candidate, "WriteIn", "Write-Ins") #standarize with openelex name
    ) %>%
    #reorder columns to match openelex format
    select(
      precinct, office, district, candidate, party, votes
    )
  #return results
  return(df)
}


#' Create File Name String for Saving Final OpenElex File
#'
#' This function can be used to build a string for the final csv file. It uses the correct naming
#' convention for the standardized OpenElex names for precinct files.
#'
#' @param target_state two-letter state abbreviation
#' @param target_county county name
#'
#' @return a file path/name
#' @export
#'
#' @examples
#' create_outfile_string("NY", "Saratoga")
create_outfile_string <- function(target_state, target_county) {
  out_string <- paste0(
    target_state,
    "_",
    target_county,
    "/20201103__",
    target_state,
    "__general__",
    str_to_lower(target_county),
    "__precinct.csv"
  )
  return(out_string)
}


