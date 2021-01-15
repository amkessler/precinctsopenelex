#' Michigan Specific Function to Rename Columns for Straight-Party Dataset
#'
#' This function takes a dataframe containing precinct data with the original Michigan headers for straight-party
#' vote totals and converts the headers to proper formatting to use with package's processing functions
#'
#' @param df
#'
#' @return df with renamed columns
#' @export
#'
#' @examples \dontrun{
#' mi_replace_colnames_straightparty(mydataframe)
#' }
mi_replace_colnames_straightparty <- function(df) {
  newcolnames <- c('Democratic Party - DEM' = 'democratic_party_dem',
                   'Republican Party - REP' = 'republican_party_rep',
                   'Libertarian Party - LIB' = 'libertarian_party_lib',
                   'U.S. Taxpayers Party - UST' = 'u_s_taxpayers_party_ust',
                   'Working Class Party - WCP' = 'working_class_party_wc',
                   'Green Party - GRN' = 'green_party_grn',
                   'Natural Law Party - NLP' = 'natural_law_party_nat',
                   'WriteIn' = 'unresolved_write_in')

  df_renamed <- df %>%
    clean_names() %>%
    rename(!!!newcolnames)

  return(df_renamed)

}



#' Michigan Specific Function to Rename Columns for Presidential Dataset
#'
#' This function takes a dataframe containing precinct data with the original Michigan headers for presidential
#' election results and converts the headers to proper formatting to use with package's processing functions
#'
#' @param df
#'
#' @return df with renamed columns
#' @export
#'
#' @examples \dontrun{
#' mi_replace_colnames_presidential(mydataframe)
#' }
mi_replace_colnames_presidential <- function(df) {
  newcolnames <- c('Joseph R. Biden - DEM' = 'joseph_r_biden_kamala_d_harris_dem',
                   'Donald J. Trump - REP' = 'donald_j_trump_michael_r_pence_rep',
                   'Jo Jorgensen - LIB' = 'jo_jorgensen_jeremy_cohen_lib',
                   'Don Blankenship - UST' = 'don_blankenship_willia_m_mohr_ust',
                   'Howie Hawkins - GRN' = 'howie_hawkins_angela_walker_grn',
                   'Rocky De La Fuente - NAT' = 'rocky_de_la_fuente_darcy_richardson_nat',
                   'WriteIn' = 'unresolved_write_in')

  df_renamed <- df %>%
    clean_names() %>%
    rename(!!!newcolnames)

  return(df_renamed)

}


#' Michigan Specific Function to Rename Columns for US Senate Dataset
#'
#' This function takes a dataframe containing precinct data with the original Michigan headers for US Senate
#' election results and converts the headers to proper formatting to use with package's processing functions
#'
#' @param df
#'
#' @return df with renamed columns
#' @export
#'
#' @examples \dontrun{
#' mi_replace_colnames_ussenate(mydataframe)
#' }
mi_replace_colnames_ussenate <- function(df) {
  newcolnames <- c('Gary Peters - DEM' = 'gary_peters_dem',
                   'John James - REP' = 'john_james_rep',
                   'Valerie L. Willis - UST' = 'valerie_l_willis_ust',
                   'Marcia Squier - GRN' = 'marcia_squier_grn',
                   'Doug Dern - NAT' = 'doug_dern_nat',
                   'WriteIn' = 'unresolved_write_in')

  df_renamed <- df %>%
    clean_names() %>%
    rename(!!!newcolnames)

  return(df_renamed)

}


