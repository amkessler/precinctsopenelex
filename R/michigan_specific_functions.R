#' Michigan-Specific Function to Rename Columns in Correct Format
#'
#' This function takes a dataframe containing precinct data with the original Michigan candidate/party headers and
#' converts the headers to proper formatting to use with package's processing functions. It is tailored
#' to work with the way Michigan counties' format of "Candidate (PARTY)" and also incorporates step to remove empty columns.
#'
#' @param df
#'
#' @return df with renamed columns and completely empty columns removed
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
