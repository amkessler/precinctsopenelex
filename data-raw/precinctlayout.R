## code to prepare `precinctlayout` dataset goes here

usethis::use_data(precinctlayout, overwrite = TRUE)


#read in one of the races, in this case presidential from Saratoga County, NY
#this will show the format the data should be in before beginning this process
precinctsampledata_ny <- readxl::read_excel("data-raw/Saratoga_NY_GE20_cleaned.xlsx",
                                    sheet = "presidential")


#use this dataset for the package to have a working sample to let people use and understand
usethis::use_data(precinctsampledata_ny)
