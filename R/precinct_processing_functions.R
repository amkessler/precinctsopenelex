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

#' Reshape County Precinct Data for Candidates to OpenElex Format
#'
#' This function is designed to take a wide dataset with one row per precinct and candidate vote
#' columns and reshape once the initial pre-processing is done.
#' The file to import should have have a precinct column, followed by columns with vote results formatted as "candidate - party".
#' The resulting dataframe will be long/tidy data with these columns: precinct, office, district, party, votes.
#'
#' @param df formatted dataframe of county precinct-level results
#' @param office which office the results refer to (e.g. "presidential", "U.S. House", "State Senate", etc.)
#' @param district number for what district is associated with the office (e.g. "45" for the 45th congressional district. note presidential should be left blank as "")
#'
#'
#' @return a reshaped dataframe in openelex long/tidy format
#' @export
#' @import dplyr
#' @import tidyr
#' @import stringr
#'
#' @examples \dontrun{
#' reshape_precinct_data(mydataframe)
#' }
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
      candidate = str_replace_all(candidate, "WriteIn", "Write-Ins"), #standarize with openelex name
      candidate = str_replace_all(candidate, "Unresolved Write-In", "Write-Ins")
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


#' Convert a Single Column of Top-level Totals by Precinct
#'
#' This function can be used in situations present in some counties where precinct
#' totals exist that you want to capture separately, like total ballots cast, total registered voters, etc.
#' The function assumes the existence of a "precinct" column with the precinct names, and then asks you to
#' provide the column name (quoted) with the total votes you want to capture. Additionally to match
#' OpenElex formatting the category is listed as the "office": the third argument allows you to provide
#' that value.
#'
#'
#'
#' @param df dataframe with the precinct totals
#' @param column_with_totals quoted name of target column containing the totals to capture (e.g. "total_reg_voters")
#' @param office_text text to fill in the OpenElex office column as per the standard (e.g. "Registered Voters")
#'
#' @return formatted dataframe ready to match up with OpenElex column structure
#' @export
#'
convert_toplevel_totals <- function(df, column_with_totals, office_text) {
  df_converted <- df %>%
    #isolate just the precinct name and target column containing vote totals
    select(precinct, votes = column_with_totals) %>%
    #add in the standard OpenElex columns
    mutate(
      office = office_text,
      district = "",
      candidate = "",
      party = ""
    ) %>%
    #reorder columns to match OpenElex standard format
    select(precinct, office, district, candidate, party, votes)
  return(df_converted)
}
