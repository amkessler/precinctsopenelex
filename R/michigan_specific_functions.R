#' Michigan-Specific Function to Rename Columns in Correct Format
#'
#' This function takes a dataframe containing precinct data with the original Michigan candidate/party headers and
#' converts the headers to proper formatting to use with package's processing functions. It is tailored
#' to work with the way Michigan counties' format of "Candidate (PARTY)" and also incorporates step to remove empty columns.
#'
#' @param df a dataframe
#'
#' @return returns a dataframe with renamed columns and completely empty columns removed
#' @export
#'
#' @examples \dontrun{
#' mi_format_column_names(mydataframe)
#' }
mi_format_column_names <- function(df) {

  df <- df %>%
    janitor::remove_empty("cols")

  newcolnames <- df %>%
    names() %>%
    str_squish() %>%
    str_replace_all("\\(", "- ") %>%
    str_remove_all("\\)") %>%
    str_replace_all("\\/", "&") #for multi-candidate listings likes prez & VP

  colnames(df) <- newcolnames

  return(df)
}





#' Michigan-Specific Function To Clean Columns With Embedded Precinct Names
#'
#' A number of Michigan counties report their results in a format that both mixes
#' together the precinct names and several vote categories (e.g. early vote, total vote)
#' in a single column. This function both extracts the precinct name from that mess,
#' and also removes rows featuring  multiple vote subtypes to leave just the "Total" count
#' for each candidate.
#'
#' @param data a dataframe with the above mentioned issues
#'
#' @return returns a cleaned dataframe ready for further processing
#' @export
#'
#' @examples \dontrun{
#' mi_clean_embedded_precinct_names(mydataframe)
#' }
mi_clean_embedded_precinct_names <- function(data) {
  #first we need to determine if the second column is NA, since the pattern is the precinct names are NA for vote columns
  #don't want to hardcode in a column name because they'll be different for every election race type. but 2nd should always be NA.
  second_col_name <- data %>%
    select(2) %>%
    names()
  #then use that second column name variable to do a conditional replace of the precinct name only in new column
  data <- data %>%
    mutate(
      testcol := if_else(is.na(!!sym(second_col_name)), precinct, "replaceme"), #need to use tidyeval !! here with sym to use variable name
      testcol = na_if(testcol, "replaceme")
    )
  #now, we'll use tidyr's fill() function to fill down each name through the NAs until it hits a new one
  data <- data %>%
    tidyr::fill(testcol, .direction = "down")
  #now that we have the precinct name with every row, we can filter for just the "Total" counts we want
  data <- data %>%
    filter(precinct == "Total")
  #finally rename our test column as "precinct" and remove the unneeded vote type total column, order remaining columns
  data <- data %>%
    select(-precinct) %>%
    rename(precinct = testcol) %>%
    select(precinct, everything())

  return(data)
}

