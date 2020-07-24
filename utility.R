######################################################
#######################################################
# This script contains functions used for fetching
# ODRS data and preprocessing them, and
# plot EWMA and other charts, which would be
# used in app.R

# Maintainer: Enhao Liu<liu.5045@osu.edu>
# Version: 1.1.4
# Last Updated Date : 2020-06-23
#######################################################
#######################################################

library(ggplot2)
library(ggthemes)
library(readxl)
library(qcc)
library(purrr)
library(lubridate)
library(tidyverse)
library(sf)
library(plotly)
library(leaflet)
library(htmltools)
library(EpiEstim)
library(heatmaply)
library(mgcv)

# library(crosstalk)



#######################################################
######### Data Collection Functions ###################
#######################################################

#' Correct the year as 2020
correct_date <- function(x) {
    tryCatch(
        {
            lubridate::year(x) <- 2020

            return(x)
        },
        error = function(e) {
            # if NAs
            return(x)
        }
    )
}

#' Check and Return Row Index with Incorrect Dates
#' in which the year value < 2019
#' @return a vector of row indexes
get_wrong_dates_idx <- function(data, date_col, last_update_date) {
    last_update_date <- as.Date(last_update_date)

    all_date_col <- c(
        "onsetDate", "Onset Date",
        "eventDate",
        "deathDate", "Date Of Death",
        "admitDate", "Admission Date",
        "dischargeDate"
    )

    if (!date_col %in% all_date_col) {
        stop(paste0("'", date_col, "' cannot be found in the dataframe."))
    }

    date_col_name <- date_col
    date_col <- dplyr::sym(date_col)

    if (date_col_name == "onsetDate" | date_col_name == "Onset Date") {
        txt_1 <- "Onset Dates"
    } else if (date_col_name == "eventDate") {
        txt_1 <- "Event Dates"
    } else if (date_col_name == "deathDate" | date_col_name == "Date Of Death") {
        txt_1 <- "Death Dates"
    } else if (date_col_name == "admitDate" | date_col_name == "Admission Date") {
        txt_1 <- "Admission Dates"
    } else if (date_col_name == "dischargeDate") {
        txt_1 <- "Discharge Dates"
    }


    # Find the wrong dates if the year value < 2019
    incorrect_date_idx <- data %>%
        dplyr::select(!!date_col) %>%
        tibble::rownames_to_column() %>%
        dplyr::filter(lubridate::year(!!date_col) < 2019) %>%
        dplyr::pull(rowname)


    incorrect_date_idx <- as.numeric(incorrect_date_idx)

    if (length(incorrect_date_idx) > 0) {
        incorrect_date_value <- data[incorrect_date_idx, ] %>%
            dplyr::pull(!!date_col)

        # Exclude some idx which cannot be auto-corrected
        # because the month value > the month value of last update date
        incorrect_month <- lubridate::month(incorrect_date_value)
        incorrect_month_greater_last_update_idx <- which(incorrect_month > lubridate::month(last_update_date))
        cannot_fix_idx <- incorrect_date_idx[incorrect_month_greater_last_update_idx]

        if (length(cannot_fix_idx) > 0) {
            cannot_fix_value <- incorrect_date_value[incorrect_month_greater_last_update_idx]
            cannot_fix_value <- paste(cannot_fix_value, collapse = ", ")

            incorrect_date_idx <- setdiff(incorrect_date_idx, cannot_fix_idx)

            incorrect_date_value <- incorrect_date_value[-incorrect_month_greater_last_update_idx]


            txt <- paste0(
                "Warning: Wrong ", txt_1, " '", cannot_fix_value,
                "' cannot be auto-corrected. Need further investigation."
            )
            message(txt)
        }

        if (length(incorrect_date_idx) > 0) {
            correct_date_value <- correct_date(incorrect_date_value)

            incorrect_date_value <- paste(incorrect_date_value, collapse = ", ")
            correct_date_value <- paste(correct_date_value, collapse = ", ")

            txt <- paste(
                "Autocorrect the Wrong", txt_1, "from",
                incorrect_date_value, "to", correct_date_value
            )
            message(txt)
        }
    }

    return(incorrect_date_idx)
}

#' Get Ohio County with Hospital Preparedness Region
get_hospital_region <- function() {
    regions_list <- list(
        "1" = "Williams, Fulton, Lucas, Ottawa, Sandusky, Erie, Huron, Seneca, Hancock, Wood, Henry, Defiance, Paulding, Putnam, Allen, Auglaize, Mercer, Van Wert",
        "2" = "Lorain, Cuyahoga, Geauga, Lake, Ashtabula",
        "3" = "Darke, Shelby, Miami, Champaign, Clark, Greene, Montgomery, Preble",
        "4" = "Wyandot, Crawford, Marion, Morrow, Knox, Licking, Delaware, Franklin, Fairfield, Pickaway, Fayette, Madison, Union, Logan, Hardin",
        "5" = "Richland, Ashland, Wayne, Holmes, Tuscarawas, Carroll, Stark, Columbiana, Mahoning, Trumbull, Portage, Summit, Medina",
        "6" = "Butler, Warren, Clinton, Hamilton, Clermont, Brown, Adams, Highland",
        "7" = "Ross, Pike, Scioto, Lawrence, Jackson, Gallia, Meigs, Vinton, Athens, Hocking",
        "8" = "Coshocton, Muskingum, Perry, Guernsey, Noble, Morgan, Washington, Monroe, Belmont, Harrison, Jefferson"
    )

    regions_names_seq <- character(0)
    regions_number_seq <- numeric(0)
    for (i in seq_along(regions_list)) {
        region_names_vec <- stringr::str_split(regions_list[[i]], ", ")[[1]]
        region_num <- as.integer(names(regions_list)[i])
        region_num_vec <- rep(region_num, length(region_names_vec))

        regions_names_seq <- c(regions_names_seq, region_names_vec)
        regions_number_seq <- c(regions_number_seq, region_num_vec)
    }

    regions_df <- data.frame(
        "County" = regions_names_seq,
        "Hospital Preparedness Region" = regions_number_seq,
        stringsAsFactors = FALSE,
        check.names = FALSE
    )

    return(regions_df)
}

#' Get Region, Zone, Counties mapping
get_region_zone_county_df <- function() {
    df <- data.frame(
        "Hospital Preparedness Region" = c(1:8),
        "Region" = c("NW", "NE", "WC", "CEN", "NECO", "SW", "SEC", "SE"),
        "Zone" = c(1, 1, 3, 2, 1, 3, 2, 2),
        "County" = c(
            "Allen, Auglaize, Defiance, Erie, Fulton, Hancock, Henry, Huron, Lucas, Mercer, Ottawa, Paulding, Putnam, Sandusky, Seneca, Van Wert, Williams, Wood",
            "Ashtabula, Cuyahoga, Geauga, Lake, Lorain",
            "Champaign, Clark, Darke, Greene, Miami, Montgomery, Preble, Shelby",
            "Crawford, Delaware, Fairfield, Fayette, Franklin, Hardin, Knox, Licking, Logan, Madison, Marion, Morrow, Pickaway, Union, Wyandot",
            "Ashland, Carroll, Columbiana, Holmes, Mahoning, Medina, Portage, Richland, Stark, Summit, Trumbull, Tuscarawas, Wayne",
            "Adams, Brown, Butler, Clermont, Clinton, Hamilton, Highland, Warren",
            "Athens, Gallia, Hocking, Jackson, Lawrence, Meigs, Pike, Ross, Scioto, Vinton",
            "Belmont, Coshocton, Guernsey, Harrison, Jefferson, Monroe, Morgan, Muskingum, Noble, Perry, Washington"
        ),
        stringsAsFactors = FALSE,
        check.names = FALSE
    )

    df <- df %>%
        dplyr::mutate_if(is.integer, as.numeric)

    return(df)
}


#' Get ODRS data from the URL
#' @param `data_url`: default = https://coronavirus.ohio.gov/static/COVIDSummaryData.csv
#' @param `backup_data_file`: default = NULL
#' The backup data file is used to load the ODRS data if fail to fetch the
#' ODRS data from https://coronavirus.ohio.gov/static/COVIDSummaryData.csv
#' NOTE: backup data file could be specified as a new URL reference to the
#' ODRS data
#' @param last_update_date : string, default = "auto"
#' The last updated date specified. By default, the last updated
#' date is the max(`Onset Date`).
#' It also allow to specify the `last_update_date` with format "%Y-%m-%d"
get_ODRS <- function(data_url = "https://coronavirus.ohio.gov/static/COVIDSummaryData.csv",
                     backup_data_file = NULL,
                     last_update_date = "auto") {
    covid_df <- tryCatch(
        {
            readr::read_csv(
                file = data_url,
                col_types = readr::cols(
                    County = readr::col_character(),
                    Sex = readr::col_character(),
                    `Age Range` = readr::col_character(),
                    `Onset Date` = readr::col_character(),
                    `Date Of Death` = readr::col_character(),
                    `Admission Date` = readr::col_character(),
                    `Case Count` = readr::col_character(),
                    `Death Count` = readr::col_character(),
                    `Hospitalized Count` = readr::col_character()
                )
            )
        },
        error = function(cond) {
            # message(cond)
            message(paste("Access Denied:", data_url))
            message(paste("Switch to load the local ODRS data:", backup_data_file))

            if (is.null(backup_data_file)) {
                stop("Fail to load the local ODRS data. Please specify 'backup_data_file' to indicate the file path.")
            }
            df <- readr::read_csv(
                file = backup_data_file,
                col_types = readr::cols(
                    County = readr::col_character(),
                    Sex = readr::col_character(),
                    `Age Range` = readr::col_character(),
                    `Onset Date` = readr::col_character(),
                    `Date Of Death` = readr::col_character(),
                    `Admission Date` = readr::col_character(),
                    `Case Count` = readr::col_character(),
                    `Death Count` = readr::col_character(),
                    `Hospitalized Count` = readr::col_character()
                )
            )

            return(df)
        }
    )

    all_valid_county_ohio <- get_hospital_region() %>%
        dplyr::pull(County)

    non_valid_county_df <- covid_df %>%
        dplyr::filter(!County %in% all_valid_county_ohio) %>%
        dplyr::mutate(
            `Case Count` = readr::parse_number(
                `Case Count`,
                na = c("", NA)
            )
        ) %>%
        dplyr::group_by(County) %>%
        dplyr::summarise_at("Case Count", list(sum))

    # tidy the data
    covid_df <- covid_df %>%
        dplyr::filter(County %in% all_valid_county_ohio) %>%
        mutate(
            `Onset Date` = readr::parse_date(
                `Onset Date`,
                format = "%m/%d/%Y",
                na = c("", NA, "Unknown")
            ),
            `Date Of Death` = readr::parse_date(
                `Date Of Death`,
                format = "%m/%d/%Y",
                na = c("", NA, "Unknown")
            ),
            `Admission Date` = readr::parse_date(
                `Admission Date`,
                format = "%m/%d/%Y",
                na = c("", NA, "Unknown")
            ),
            `Case Count` = readr::parse_number(
                `Case Count`,
                na = c("", NA)
            ),
            `Death Count` = readr::parse_number(
                `Death Count`,
                na = c("", NA)
            ),
            `Hospitalized Count` = readr::parse_number(
                `Hospitalized Count`,
                na = c("", NA)
            )
        )

    covid_df <- as.data.frame(covid_df)

    if (last_update_date == "auto") {
        last_update_date <- as.character(max(covid_df$`Onset Date`))
    } else {
        check_valid_date <- as.Date(last_update_date, "%Y-%m-%d")
        if (is.na(check_valid_date)) {
            stop("'last_update_date' is either 'auto' or date format with '%Y-%m-%d'")
        }
    }

    message(paste("Fetching ODRS Data: Last Updated:", last_update_date))

    # Show non-valid County dataframe
    if (nrow(non_valid_county_df) > 0) {
        message("Remove invalid counties from ODRS data: ")
        print(non_valid_county_df)
    }

    # Show the total number of cases
    total_case_counts <- sum(covid_df$`Case Count`)
    txt <- paste("Cumulative COVID-19 Case Counts:", total_case_counts)
    message(txt)



    # Find the wrong dates if the year value < 2019
    incorrect_onset_date_idx <- get_wrong_dates_idx(covid_df, "Onset Date", last_update_date)

    incorrect_death_date_idx <- get_wrong_dates_idx(covid_df, "Date Of Death", last_update_date)

    incorrect_admission_date_idx <- get_wrong_dates_idx(covid_df, "Admission Date", last_update_date)

    incorrect_date_all_idx <- unique(c(
        incorrect_onset_date_idx,
        incorrect_death_date_idx,
        incorrect_admission_date_idx
    ))

    # Correct those wrong dates
    if (length(incorrect_date_all_idx) > 0) {
        covid_df_incorrect <- covid_df[incorrect_date_all_idx, ]
        covid_df_correct <- covid_df[-incorrect_date_all_idx, ]
        covid_df_incorrect <- covid_df_incorrect %>%
            dplyr::mutate_at(c("Onset Date", "Date Of Death", "Admission Date"), list(correct_date))

        covid_df <- rbind(covid_df_correct, covid_df_incorrect)
        rownames(covid_df) <- 1:nrow(covid_df)
    }


    # Impute missing hospitalized counts when has Admission date
    missing_hosp_counts_idx <- covid_df %>%
        tibble::rownames_to_column() %>%
        dplyr::filter(!is.na(`Admission Date`) & `Hospitalized Count` == 0) %>%
        dplyr::pull(rowname)

    missing_hosp_counts_idx <- as.numeric(missing_hosp_counts_idx)

    if (length(missing_hosp_counts_idx) > 0) {
        covid_df_missing <- covid_df[missing_hosp_counts_idx, ]
        covid_df_nomissing <- covid_df[-missing_hosp_counts_idx, ]

        # impute
        covid_df_missing <- covid_df_missing %>%
            dplyr::mutate(`Hospitalized Count` = `Case Count`)

        impute_missing_counts <- sum(covid_df_missing$`Hospitalized Count`)

        txt <- paste("Impute", impute_missing_counts, "Missing Hospitalized Counts for Those with Admission Dates")
        message(txt)
        # recover
        covid_df <- rbind(covid_df_nomissing, covid_df_missing)
        rownames(covid_df) <- 1:nrow(covid_df)
    }

    return(covid_df)
}


#' Extract county vector by specifying zone number/ region number
#' @param zone_number integer, default = NULL
#' Range from 1 to 3
#' @param region_number integer, default = NULL
#' Range from 1 to 8
extract_county_vector <- function(zone_number = NULL, region_number = NULL) {
    if (is.null(zone_number) & is.null(region_number)) {
        stop("Please specify either 'zone_number' or 'region_number'")
    }

    if (!is.null(zone_number) & !is.null(region_number)) {
        stop("Only one argument is allowed to be specified. Either 'zone_number' or 'region_number'")
    }

    county_zone_region_df <- get_region_zone_county_df()

    if (!is.null(zone_number)) {
        if (zone_number > 3 | zone_number < 1) {
            stop("'zone_number' is integer value and range from 1 to 3")
        }

        county_vec <- county_zone_region_df %>%
            dplyr::filter(Zone %in% zone_number) %>%
            dplyr::pull(County)

        county_vec <- lapply(county_vec, function(x) {
            stringr::str_split(x, pattern = ", ")
        })

        county_vec <- unlist(county_vec)
    }

    if (!is.null(region_number)) {
        if (region_number > 8 | region_number < 1) {
            stop("'region_number' is integer value and range from 1 to 8")
        }

        county_vec <- county_zone_region_df %>%
            dplyr::filter(`Hospital Preparedness Region` %in% region_number) %>%
            dplyr::pull(County)

        county_vec <- lapply(county_vec, function(x) {
            stringr::str_split(x, pattern = ", ")
        })

        county_vec <- unlist(county_vec)
    }

    return(county_vec)
}


#' Get character representation for region number
get_char_region <- function(region_numbers) {
    df <- data.frame(
        "Hospital Preparedness Region" = c(1:8),
        "Region" = c("NW", "NE", "WC", "CEN", "NECO", "SW", "SEC", "SE"),
        "Zone" = c(1, 1, 3, 2, 1, 3, 2, 2),
        "County" = c(
            "Allen, Auglaize, Defiance, Erie, Fulton, Hancock, Henry, Huron, Lucas, Mercer, Ottawa, Paulding, Putnam, Sandusky, Seneca, Van Wert, Williams, Wood",
            "Ashtabula, Cuyahoga, Geauga, Lake, Lorain",
            "Champaign, Clark, Darke, Greene, Miami, Montgomery, Preble, Shelby",
            "Crawford, Delaware, Fairfield, Fayette, Franklin, Hardin, Knox, Licking, Logan, Madison, Marion, Morrow, Pickaway, Union, Wyandot",
            "Ashland, Carroll, Columbiana, Holmes, Mahoning, Medina, Portage, Richland, Stark, Summit, Trumbull, Tuscarawas, Wayne",
            "Adams, Brown, Butler, Clermont, Clinton, Hamilton, Highland, Warren",
            "Athens, Gallia, Hocking, Jackson, Lawrence, Meigs, Pike, Ross, Scioto, Vinton",
            "Belmont, Coshocton, Guernsey, Harrison, Jefferson, Monroe, Morgan, Muskingum, Noble, Perry, Washington"
        ),
        stringsAsFactors = FALSE,
        check.names = FALSE
    )

    region_value <- df %>%
        dplyr::filter(`Hospital Preparedness Region` %in% region_numbers) %>%
        dplyr::pull(Region)

    region_value <- paste(region_value, collapse = " & ")
    return(region_value)
}


#' Get merged ODRS and Region data
#' @param `data_url`: default = https://coronavirus.ohio.gov/static/COVIDSummaryData.csv
#' @param `backup_data_file`: default = NULL
#' The backup data file is used to load the ODRS data if fail to fetch the
#' ODRS data from https://coronavirus.ohio.gov/static/COVIDSummaryData.csv
#' NOTE: backup data file could be specified as a new URL or
#' any file path reference to the ODRS data
#' @param last_update_date : string, default = "auto"
#' The last updated date specified. By default, the last updated
#' date is the max(`Onset Date`).
#' It also allow to specify the `last_update_date` with format "%Y-%m-%d"
get_merged_ODRS_region <- function(data_url = "https://coronavirus.ohio.gov/static/COVIDSummaryData.csv",
                                   backup_data_file = NULL,
                                   last_update_date = "auto") {
    covid_df <- get_ODRS(
        data_url = data_url,
        backup_data_file = backup_data_file,
        last_update_date = last_update_date
    )

    region_df <- get_hospital_region()

    covid_df <- covid_df %>%
        dplyr::left_join(region_df, by = "County")

    if (last_update_date == "auto") {
        attr(covid_df, "last_update_date") <- as.character(max(covid_df$`Onset Date`))
    } else {
        check_valid_date <- as.Date(last_update_date, "%Y-%m-%d")
        if (is.na(check_valid_date)) {
            stop("'last_update_date' is either 'auto' or date format with '%Y-%m-%d'")
        }
        attr(covid_df, "last_update_date") <- last_update_date
    }

    return(covid_df)
}




#' Convert the IOP ODRS data to the formatting
#' for the Shiny app and Report
#' @param data the dataframe of the tidy ODRS data
convert2special_ODRS <- function(data, last_update_date = "auto") {
    covid_df <- data %>%
        dplyr::select(
            diagaddress_county,
            diagaddress_zip,
            ageyears,
            sex,
            case,
            hospitalized,
            # covid_icu,
            # censustract,
            death,
            onsetDate,
            admitDate,
            # dischargeDate,
            deathDate
        ) %>%
        dplyr::mutate(sex = readr::parse_character(
            sex,
            na = c("", NA, "NA", "Unknown", "NULL", "No")
        )) %>%
        dplyr::mutate(
            case = as.numeric(case),
            hospitalized = as.numeric(hospitalized),
            death = as.numeric(death)
        ) %>%
        dplyr::mutate_if(is.numeric, coalesce, 0) %>%
        dplyr::rename(
            County = diagaddress_county,
            ZIP = diagaddress_zip,
            Age = ageyears,
            Sex = sex,
            `Case Count` = case,
            `Death Count` = death,
            `Hospitalized Count` = hospitalized,
            `Onset Date` = onsetDate,
            `Date Of Death` = deathDate,
            `Admission Date` = admitDate
        ) %>%
        dplyr::mutate(ZIP_5d = stringr::str_sub(ZIP, end = 5))

    # County & Region numbers
    region_df <- get_hospital_region()

    # Merge Region numbers
    covid_df <- covid_df %>%
        dplyr::left_join(region_df, by = "County")

    if (last_update_date == "auto") {
        attr(covid_df, "last_update_date") <- as.character(max(covid_df$`Onset Date`))
    } else {
        check_valid_date <- as.Date(last_update_date, "%Y-%m-%d")
        if (is.na(check_valid_date)) {
            stop("'last_update_date' is either 'auto' or date format with '%Y-%m-%d'")
        }
        attr(covid_df, "last_update_date") <- last_update_date
    }

    message(paste("Fetching ODRS Data: Last Updated:", attr(covid_df, "last_update_date")))

    # Show the total number of cases
    total_case_counts <- sum(covid_df$`Case Count`)
    txt <- paste("Cumulative COVID-19 Case Counts:", total_case_counts)
    message(txt)


    return(covid_df)
}



###################################################
######### Population related Functions ############
###################################################

#' Get Ohio Population data
#' @param `data_file`: default = "./data/population/Ohio_population.xlsx"
#' File path reference to the Ohio Popultaion data
# get_ohio_pop <- function(data_file = "./data/population/Ohio_population.xlsx") {
#     pop_df <- readxl::read_excel(data_file)
#     pop_df <- pop_df %>%
#         mutate(Count = readr::parse_number(Count))
#     return(pop_df)
# }


#' Get Ohio Population data
#' @param `data_file`: default = "./data/population/Ohio_population.csv"
#' File path reference to the Ohio Popultaion data
get_ohio_pop <- function(data_file = "./data/population/Ohio_population.csv") {
    pop_df <- readr::read_csv(data_file)
    return(pop_df)
}

#' Get the Population for specific hospital regions
#' @param `pop_df`: dataframe
#' Population dataframe obtained by calling get_ohio_pop()
#' Columns: County, Count
#' @param `region_df`: dataframe
#' The dataframe obtained by calling get_hospital_region()
#' @param `region`: str or vector, default = 'all'
#' By default, aggregate data for the entire Ohio.
#' 1. Allow to specify a vector of region numbers, e.g., c(1,2,3) or c(1)
#' aggragate data for specific hospital regions. The valid number is from 1 to 8
#' 2. Allow to specify a string or a vector of region names, i.e., county names
#' @param `return_proportion`: boolean, default = TRUE
#' Use to control return the counts/proportion of population
#' By default, return the proportion of regions' population
#' to that of the entire Ohio
get_region_pop <- function(pop_df,
                           region_df,
                           region = c(1),
                           return_proportion = TRUE) {
    county_names <- region_df %>%
        dplyr::pull(County)

    # merge the population with the region numbers
    pop_df <- pop_df %>%
        dplyr::left_join(region_df, by = "County")

    # all population counts
    all_pop_count <- sum(pop_df$Count)

    # If region is character: "all" or county names
    if (typeof(region) == "character") {
        if (length(region) == 1) {
            if (region == "all") {
                region_pop_count <- pop_df %>%
                    dplyr::pull(Count)
            } else {
                if (region %in% county_names) {
                    region_pop_count <- pop_df %>%
                        dplyr::filter(County %in% region) %>%
                        dplyr::pull(Count)
                } else {
                    txt <- paste0(
                        "Incorrect county names: ", region,
                        ". Please specify the correct county names in the State of Ohio."
                    )
                    stop(txt)
                }
            }
        } else {
            if (all(region %in% county_names)) {
                region_pop_count <- pop_df %>%
                    dplyr::filter(County %in% region) %>%
                    dplyr::pull(Count)
            } else {
                incorrect_names <- region[!region %in% county_names]
                incorrect_names <- paste(incorrect_names, collapse = ", ")
                txt <- paste0("Incorrect county names: ", incorrect_names, ". Please specify the correct county names in the State of Ohio.")
                stop(txt)
            }
        }
    } else {
        # If region is vector of numbers
        region_pop_count <- pop_df %>%
            dplyr::filter(`Hospital Preparedness Region` %in% region) %>%
            dplyr::pull(Count)
    }

    # calculate counts
    region_pop_count <- sum(region_pop_count)

    if (return_proportion) {
        region_pop_count <- region_pop_count / all_pop_count
    }

    return(region_pop_count)
}


###################################################
######### Data Aggragation Functions #############
##################################################

#' Stack mutiple values as a single long string
stack_strings <- function(x) {
    paste(unique(sort(x)), collapse = ", ")
}

#' Transform to evenly-spaced time series (day-based)
#' @param `data`: dataframe with columns `Onset Date` and `Case Count`
#' @return a dataframe
get_evenly_spaced_data <- function(data, last_update_date) {
    last_update_date <- as.Date(last_update_date, "%Y-%m-%d")
    data <- data %>%
        mutate(`Onset Date` = as.Date(`Onset Date`)) %>%
        complete(
            `Onset Date` = seq.Date(min(`Onset Date`), last_update_date, by = "day"),
            fill = list(`Case Count` = 0)
        )

    return(data)
}

#' Aggragation function for data
#' @param `data`: dataframe
#' The merged COVID Counts dataframe with Hospital Region
#' @param `last_update_date`: default = auto
#' The last updated date for the data
#' By default, the `last_update_date` = attr(data, "last_update_date")
#' It also allow to specify the `last_update_date` with format "%Y-%m-%d"
#' @param `region`: str or vector, default = 'all'
#' By default, aggregate data for the entire Ohio.
#' 1. Allow to specify a vector of region numbers, e.g., c(1,2,3) or c(1)
#' aggragate data for specific hospital regions. The valid number is from 1 to 8
#' 2. Allow to specify a string or a vector of region names, i.e., county names
#' @param `evenly_space` : boolean, default = TRUE
#' Whether or not make evenly spaced data by day
#' @return the aggragated dataframe
super_agg <- function(data, last_update_date = "auto", region = "all", evenly_space = TRUE) {
    if (last_update_date == "auto") {
        last_update_date <- attr(data, "last_update_date")
    } else {
        check_valid_date <- as.Date(last_update_date, "%Y-%m-%d")
        if (is.na(check_valid_date)) {
            stop("'last_update_date' is either 'auto' or date format with '%Y-%m-%d'")
        }
    }

    # Drop missing counts

    # For case counts with missing onset dates
    missing_df <- data %>%
        # rownames_to_column() %>%
        dplyr::filter(is.na(`Onset Date`) & `Case Count` > 0)

    # missing_idx_case <- as.numeric(missing_df$rowname)

    missing_count <- sum(missing_df$`Case Count`)

    if (missing_count > 0) {
        txt <- paste("Exclude", missing_count, "Case Counts with Missing Onset Dates or Unknown Dates.")
        message(txt)
    }

    # For death counts with missing death dates
    missing_df <- data %>%
        # rownames_to_column() %>%
        dplyr::filter(is.na(`Date Of Death`) & `Death Count` > 0)

    # missing_idx_death <- as.numeric(missing_df$rowname)

    missing_count <- sum(missing_df$`Death Count`)

    if (missing_count > 0) {
        txt <- paste("Exclude", missing_count, "Death Counts with Missing/Unknown Death of Dates.")
        message(txt)
    }

    # For hospitalized counts with missing admission dates
    missing_df <- data %>%
        # rownames_to_column() %>%
        dplyr::filter(is.na(`Admission Date`) & `Hospitalized Count` > 0)

    # missing_idx_hosp <- as.numeric(missing_df$rowname)

    missing_count <- sum(missing_df$`Hospitalized Count`)

    if (missing_count > 0) {
        txt <- paste("Exclude", missing_count, "Hospitalized Counts with Missing/Unknown Admission Dates.")
        message(txt)
    }

    # missing_idx_all <- unique(c(missing_idx_case, missing_idx_death, missing_idx_hosp))


    # Valid county names
    county_names <- get_hospital_region() %>%
        dplyr::pull(County)

    # If region is character: "all" or county names
    if (typeof(region) == "character") {
        if (length(region) == 1) {
            if (region == "all") {
                data <- data
            } else {
                if (region %in% county_names) {
                    data <- data %>%
                        dplyr::filter(County == region)
                } else {
                    txt <- paste0("Incorrect county names: ", region, ". Please specify the correct county names in the State of Ohio.")
                    stop(txt)
                }
            }
        } else {
            # if region names > 1
            if (all(region %in% county_names)) {
                data <- data %>%
                    dplyr::filter(County %in% region)
            } else {
                incorrect_names <- region[!region %in% county_names]
                incorrect_names <- paste(incorrect_names, collapse = ", ")
                txt <- paste0("Incorrect county names: ", incorrect_names, ". Please specify the correct county names in the State of Ohio.")
                stop(txt)
            }
        }
    } else {
        # If specify the region number
        region_numbers <- c(1:8)
        if (all(region %in% region_numbers)) {
            data <- data %>%
                dplyr::filter(`Hospital Preparedness Region` %in% region)
        } else {
            incorrect_names <- region[!region %in% region_numbers]
            incorrect_names <- paste(incorrect_names, collapse = ", ")
            txt <- paste0("Incorrect region numbers: ", incorrect_names, ". Please specify the region numbers from 1 to 8.")
            stop(txt)
        }
    }

    df_agg_count <- data %>%
        dplyr::filter(!is.na(`Onset Date`)) %>%
        group_by(`Onset Date`) %>%
        summarise_at("Case Count", list(sum))

    if (evenly_space) {
        df_agg_count <- get_evenly_spaced_data(df_agg_count, last_update_date)
    }

    # Aggregate for the Death Count
    df_agg_death <- data %>%
        dplyr::filter(!is.na(`Date Of Death`)) %>%
        group_by(`Date Of Death`) %>%
        summarise_at("Death Count", list(sum))

    # Aggregate for Hospitalized Count

    df_agg_hosp <- data %>%
        dplyr::filter(!is.na(`Admission Date`)) %>%
        group_by(`Admission Date`) %>%
        summarise_at("Hospitalized Count", list(sum))

    # Merge aggregated Case Count, Death Count
    df_agg_all <- df_agg_count %>%
        left_join(df_agg_death, by = c("Onset Date" = "Date Of Death"))

    # Merge aggregated Hospitalized Count
    df_agg_all <- df_agg_all %>%
        left_join(df_agg_hosp, by = c("Onset Date" = "Admission Date"))

    # For NAs in those Counts columns, convert to 0
    df_agg_all <- df_agg_all %>%
        mutate_if(is.numeric, coalesce, 0)


    attr(df_agg_all, "last_update_date") <- last_update_date
    attr(df_agg_all, "aggregated") <- TRUE

    return(df_agg_all)
}


# Aggregate Counts by Date column and Group column
# **NOTE**: county with NAs would be removed
#' Aggregate case counts by specifying
#' date column and group column (e.g., County)
#' @param data dataframe
#' The ODRS data
agg_daily_counts_by_cols <- function(
                                     data,
                                     group_col = "County",
                                     date_col = "Onset Date",
                                     count_col = "Case Count",
                                     last_update_date = "auto",
                                     evenly_space = TRUE) {
    if (last_update_date == "auto") {
        last_update_date <- attr(data, "last_update_date")
    } else {
        check_valid_date <- as.Date(last_update_date, "%Y-%m-%d")
        if (is.na(check_valid_date)) {
            stop("'last_update_date' is either 'auto' or date format with '%Y-%m-%d'")
        }
    }

    # Check arguments
    # check if valid columns
    col_names <- colnames(data)
    cols <- c(group_col, count_col, date_col)
    if (!all(cols %in% col_names)) {
        incorrect_col_names <- cols[!cols %in% col_names]
        incorrect_col_names <- paste(incorrect_col_names, collapse = ", ")
        txt <- paste0("Incorrect column names: ", incorrect_col_names)
        stop(txt)
    }

    # symolize the string col names
    count_col_name <- count_col
    date_col_name <- date_col
    group_col_name <- group_col
    count_col <- dplyr::sym(count_col)
    date_col <- dplyr::sym(date_col)
    group_col <- dplyr::sym(group_col)

    txt <- paste(
        "Aggregate ODRS", count_col_name,
        "by", group_col_name, date_col_name, ": Last Updated Date:", last_update_date
    )
    message(txt)

    org_total_counts <- sum(data[count_col_name])

    # Aggregate by group column
    dummy <- data %>%
        dplyr::select(!!group_col) %>%
        dplyr::filter(is.na(!!group_col))

    n_missing <- nrow(dummy)

    txt <- paste(
        "Exclude", n_missing,
        "cases with missing", group_col_name, "values"
    )
    message(txt)

    dummy <- data %>%
        dplyr::select(!!date_col, !!count_col) %>%
        dplyr::filter(is.na(!!date_col) & !!count_col > 0)

    n_missing <- nrow(dummy)

    txt <- paste(
        "Exclude", n_missing,
        "cases with missing", date_col_name, "values"
    )
    message(txt)


    df_agg <- data %>%
        dplyr::filter(!is.na(!!group_col)) %>%
        dplyr::filter(!is.na(!!date_col)) %>%
        dplyr::group_by(!!group_col, !!date_col) %>%
        dplyr::summarise_at(count_col_name, list(sum)) %>%
        dplyr::ungroup()


    agg_total_counts <- sum(df_agg[count_col_name])
    txt_1 <- paste("Cumulative", count_col_name, "(original):", org_total_counts)
    txt_2 <- paste("Cumulative", count_col_name, "(aggregated):", agg_total_counts)
    message(paste(txt_1, txt_2, sep = "\n"))

    if (evenly_space) {
        last_update_date <- as.Date(last_update_date, "%Y-%m-%d")
        df_agg <- df_agg %>%
            tidyr::complete(
                !!date_col := seq.Date(min(!!date_col), last_update_date, by = "day"),
                !!group_col
            ) %>%
            mutate_if(is.numeric, coalesce, 0)
    }

    return(df_agg)
}


# Aggregate Cumulative Counts by Group column
# **NOTE**: county with NAs would be removed
#' Aggregate case counts by specifying
#' date column and group column (e.g., County)
#' @param data dataframe
#' The ODRS data
agg_cumu_counts_by_cols <- function(
                                    data,
                                    group_col = "County",
                                    count_col = "Case Count",
                                    last_update_date = "auto") {
    if (last_update_date == "auto") {
        last_update_date <- attr(data, "last_update_date")
    } else {
        check_valid_date <- as.Date(last_update_date, "%Y-%m-%d")
        if (is.na(check_valid_date)) {
            stop("'last_update_date' is either 'auto' or date format with '%Y-%m-%d'")
        }
    }

    # Check arguments
    # check if valid columns
    col_names <- colnames(data)
    cols <- c(group_col, count_col)
    if (!all(cols %in% col_names)) {
        incorrect_col_names <- cols[!cols %in% col_names]
        incorrect_col_names <- paste(incorrect_col_names, collapse = ", ")
        txt <- paste0("Incorrect column names: ", incorrect_col_names)
        stop(txt)
    }

    # symolize the string col names
    count_col_name <- count_col
    group_col_name <- group_col
    count_col <- dplyr::sym(count_col)
    group_col <- dplyr::sym(group_col)

    txt <- paste(
        "Aggregate ODRS", count_col_name,
        "by", group_col_name, ": Last Updated Date:", last_update_date
    )
    message(txt)

    org_total_counts <- sum(data[count_col_name])

    # Aggregate by group column
    dummy <- data %>%
        dplyr::select(!!group_col) %>%
        dplyr::filter(is.na(!!group_col))

    n_missing <- nrow(dummy)

    txt <- paste(
        "Exclude", n_missing,
        "cases with missing", group_col_name, "values"
    )
    message(txt)

    df_agg <- data %>%
        dplyr::filter(!is.na(!!group_col)) %>%
        dplyr::group_by(!!group_col) %>%
        dplyr::summarise_at(count_col_name, list(sum)) %>%
        dplyr::ungroup()


    agg_total_counts <- sum(df_agg[count_col_name])
    txt_1 <- paste("Cumulative", count_col_name, "(original):", org_total_counts)
    txt_2 <- paste("Cumulative", count_col_name, "(aggregated):", agg_total_counts)
    message(paste(txt_1, txt_2, sep = "\n"))

    return(df_agg)
}




###############################################
######### Chart Functions #####################
###############################################

#' Define Colors used
colors_custom <- list(
    blue_gray_1 = "#d5e4eb",
    blue_gray_2 = "#c3d6df",
    blue_gray_3 = "#6aa5be",
    light_blue = "#4a9bc0",
    black = "#000000",
    white = "#ffffff",
    red = "#f50909",
    dark_red_1 = "#ca0b11",
    dark_red_2 = "#9e080d",
    light_red = "#f82929",
    pink = "#f77171",
    pink_gray = "#dd9595",
    orange = "#ff9900",
    dark_orange = "#f87529",
    yellow_gray = "#e2d066"
)


#' Define Plot Theme
theme_customized <- function(base_size = 11, base_family = "sans",
                             legend_position = "bottom", legend_just = "center") {
    half_line <- base_size / 2
    ret <- ggthemes::theme_foundation(base_size = base_size, base_family = base_family) +
        theme(
            ## Base setting for lines, rect, text
            line = element_line(colour = "black"),
            # remove all rectangular elements' fill value and no lines
            rect = element_rect(fill = NA, colour = NA, linetype = 0),
            text = element_text(colour = "black"),
            ## Plot setting: background, margin, titles
            plot.background = element_rect(fill = colors_custom$blue_gray_1, color = NA),
            plot.margin = unit(c(6, 5, 6, 5) * 2, "points"),
            plot.title = element_text(size = rel(1.5), hjust = 0, vjust = 1, face = "bold", margin = margin(b = half_line)),
            plot.title.position = "panel",
            plot.subtitle = element_text(size = rel(1), hjust = 0, vjust = 1, margin = margin(b = half_line)),
            plot.caption = element_text(size = rel(0.6), hjust = 0, vjust = 1, margin = margin(t = half_line)),
            plot.caption.position = "panel",
            plot.tag = element_text(size = rel(1), hjust = 0.5, vjust = 0.5),
            plot.tag.position = "topleft",

            ## Panel setting: background, grid, border...
            panel.background = element_rect(fill = colors_custom$blue_gray_2, linetype = 0),
            panel.border = element_blank(),
            panel.grid.major = element_line(colour = "white"),
            panel.grid.minor = element_line(colour = "white", size = 0.25),
            # panel.grid.major.x = element_blank(),
            # panel.grid.major.y = element_blank(),
            # panel.spacing = unit(0.25, "lines"),

            ## Axis setting: text, lines, ticks
            axis.line = element_line(size = rel(0.8)),
            # axis.line.y = element_blank(),
            axis.text = element_text(size = rel(0.8)),
            axis.text.x = element_text(vjust = 0, margin = margin(b = half_line, unit = "pt")),
            # axis.text.y = element_text(hjust = 0, margin = margin(r = half_line, unit = "pt")),
            axis.ticks = element_line(),
            axis.title = element_text(size = rel(1)),
            axis.title.x = element_blank(), # remove x title
            # axis.title.y = element_text(angle = 90),
            # axis.ticks.length = unit(-base_size * 0.5, "points"),

            # Legend setting: background, margin, position
            legend.background = element_rect(linetype = 0),
            legend.margin = margin(0, 0, 0, 0, "cm"),
            # legend.box.margin = margin(-0.5, -0.5, -0.5, -0.5, "cm"),
            legend.box.spacing = unit(half_line, "points"),
            # legend.box.background = element_rect(fill = "#e5e5e5", size = .1),
            legend.position = legend_position,
            legend.justification = legend_just,
            legend.direction = NULL,
            legend.title = element_text(size = rel(0.8), hjust = 0),
            legend.title.align = NULL,
            legend.text = element_text(size = rel(0.6)),
            legend.text.align = NULL,
            legend.spacing = unit(base_size, "points"),
            legend.key = element_rect(linetype = 0),
            legend.key.size = unit(1.2, "lines"),
            legend.key.height = NULL,
            legend.key.width = NULL,

            # strip.background = element_rect(fill = bgcolors["ebg"], colour = NA, linetype = 0),
            # strip.text = element_text(size = rel(1.25)),
            # strip.text.x = element_text(),
            # strip.text.y = element_text(angle = -90),
            complete = TRUE
        )

    return(ret)
}

#' Plot Daily Counts (ggplot version)
#' @param `data`: dataframe
#' The aggregated COVID dataframe
plot_daily_counts <- function(data, last_update_date,
                              count_col = "Case Count",
                              date_col = "Onset Date",
                              plot_type = "bar",
                              show_cumulative = FALSE,
                              color_fill = colors_custom$light_blue,
                              title = NULL) {
    # Check arguments
    # check if valid columns
    col_names <- colnames(data)
    cols <- c(count_col, date_col)
    if (!all(cols %in% col_names)) {
        incorrect_col_names <- cols[!cols %in% col_names]
        incorrect_col_names <- paste(incorrect_col_names, collapse = ", ")
        txt <- paste0("Incorrect column names: ", incorrect_col_names)
        stop(txt)
    }

    # check if valid plot type
    all_types <- c("bar", "line")
    if (!plot_type %in% all_types) {
        all_types_str <- paste(all_types, collapse = ", ")
        txt <- paste0("Please specify valid plot type: ", all_types_str)
        stop(txt)
    }

    # set plot title
    base_title <- paste0("Daily COVID-19 ", count_col, "s")

    if (show_cumulative) {
        base_title <- paste0("Cumulative COVID-19 ", count_col, "s")
    }

    if (!is.null(title)) {
        title <- paste(base_title, title, sep = ": ")
    } else {
        title <- base_title
    }

    # set caption
    caption_line_1 <- "Data Source: Ohio Disease Reporting System (ODRS) https://coronavirus.ohio.gov/wps/portal/gov/covid-19/dashboards"
    caption_line_2 <- paste("Last Updated", format(as.Date(last_update_date), "%B %d, %Y"), sep = ": ")
    caption <- paste(caption_line_1, caption_line_2, sep = "\n")

    # symolize the string col names
    count_col_name <- count_col
    date_col_name <- date_col
    count_col <- dplyr::sym(count_col)
    date_col <- dplyr::sym(date_col)

    if (show_cumulative) {
        p <- ggplot(
            data,
            aes(
                x = !!date_col,
                y = cumsum(!!count_col),
                text = paste0(
                    "Date: ", !!date_col,
                    "<br>Cumulative ", count_col_name, ": ", cumsum(!!count_col)
                )
            )
        )
    } else {
        p <- ggplot(
            data,
            aes(
                x = !!date_col,
                y = !!count_col,
                text = paste0(
                    "Date: ", !!date_col,
                    "<br>", count_col_name, ": ", !!count_col
                )
            )
        )
    }


    p <- p +
        labs(
            title = title,
            caption = caption,
            x = date_col_name,
            y = count_col_name
        ) +
        theme_customized()

    if (plot_type == "bar") {
        p <- p +
            geom_col(fill = color_fill)
    } else if (plot_type == "line") {
        p <- p +
            geom_line(color = color_fill) +
            geom_point(color = color_fill)
    }

    return(p)
}



#' Plotly Daily Counts (plotly version)
#' @param `data`: dataframe
#' The aggregated COVID dataframe
plotly_daily_counts <- function(data, last_update_date,
                                count_col = "Case Count",
                                date_col = "Onset Date",
                                plot_type = "bar",
                                show_cumulative = FALSE,
                                color_fill = colors_custom$light_blue,
                                title = "Ohio") {
    # Check arguments
    # check if valid columns
    col_names <- colnames(data)
    cols <- c(count_col, date_col)
    if (!all(cols %in% col_names)) {
        incorrect_col_names <- cols[!cols %in% col_names]
        incorrect_col_names <- paste(incorrect_col_names, collapse = ", ")
        txt <- paste0("Incorrect column names: ", incorrect_col_names)
        stop(txt)
    }

    # check if valid plot type
    all_types <- c("bar")
    if (!plot_type %in% all_types) {
        all_types_str <- paste(all_types, collapse = ", ")
        txt <- paste0("Please specify valid plot type: ", all_types_str)
        stop(txt)
    }

    # set plot title
    base_title <- paste0("Daily COVID-19 ", count_col, "s")

    if (show_cumulative) {
        base_title <- paste0("Cumulative COVID-19 ", count_col, "s")
    }

    if (!is.null(title)) {
        title <- paste(base_title, title, sep = ": ")
    } else {
        title <- base_title
    }

    # set caption
    caption_line <- paste("Last Updated", format(as.Date(last_update_date), "%B %d, %Y"), sep = ": ")


    # Plot
    if (show_cumulative) {
        p <- plot_ly(
            data = data
        ) %>%
            add_bars(
                x = ~ get(date_col),
                y = ~ cumsum(get(count_col)),
                marker = list(color = color_fill),
                hovertemplate = paste0(
                    "Date: %{x|%b %d}",
                    "<br>",
                    "Cumulative ", count_col, ": ", "%{y:,}",
                    "<extra></extra>"
                )
            )
    } else {
        p <- plot_ly(
            data = data
        ) %>%
            add_bars(
                x = ~ get(date_col),
                y = ~ get(count_col),
                marker = list(color = color_fill),
                hovertemplate = paste0(
                    "Date: %{x|%b %d}",
                    "<br>",
                    "Daily ", count_col, ": ", "%{y:,}",
                    "<extra></extra>"
                )
            )
    }

    p <- p %>%
        plotly::layout(
            hovermode = "x",
            title = list(
                text = paste0(
                    title,
                    "<br>",
                    "<sup>",
                    caption_line,
                    "</sup>"
                ),
                xref = "paper",
                yref = "paper",
                x = 0
            ),
            xaxis = list(
                title = "",
                type = "date",
                tickformat = "%b %d<br>(%a)",
                showspikes = TRUE,
                spikemode = "across",
                spikesnap = "cursor",
                ticks = "outside",
                ticklen = 5,
                tickwidth = 2,
                tickcolor = colors_custom$black,
                showline = TRUE,
                zeroline = FALSE,
                gridcolor = colors_custom$white
            ),
            yaxis = list(
                title = "",
                ticks = "outside",
                ticklen = 5,
                tickwidth = 2,
                tickcolor = colors_custom$black,
                showline = TRUE,
                zeroline = FALSE,
                gridcolor = colors_custom$white
            ),
            plot_bgcolor = colors_custom$blue_gray_2,
            paper_bgcolor = colors_custom$blue_gray_1,
            margin = list(l = 20, r = 20, t = 50, b = 50),
            legend = list(orientation = "h", traceorder = "normal", y = -0.2),
            font = list(color = "black")
        ) %>%
        plotly::config(
            displaylogo = FALSE # remove plotly logo
        )


    return(p)
}




#' Sample calibration data from a
#' Normal distribution (mean, std=sqrt(mean))
#' @param `sample_size`: int, default = 100
#' The number of observations sampled
#' @param `mu` : float, default = 2000
#' The mean value of the Normal distribution
#' @param `seed` : int, default = 0
#' Random number generator
#' @return a sequence of sampled calibration data
get_calibration_data <- function(sample_size = 100, mu = 2000, seed = 0) {
    set.seed(seed)
    cal_sample <- rnorm(n = sample_size, mean = mu, sd = sqrt(mu))

    return(cal_sample)
}


#' EWMA chart
#' @param `obs_data` : a sequence of observed data for inspecting
#' @param `obs_time`: a sequence of time of the observed data
#' @param `cal_data`: default = NULL
#' a sequence of sampled calibration data.
#' If specify the calibrated data, it is used to normalize the `obs_data` and
#' calculate the `center` and `std_dev` values.
#' @param `normalize` : boolean, default = TRUE
#' Whether or not scale the data
#' If set the calibrated data, the observed data would be normalized
#' by the calibrated data's mean and standard deviation
#' If there is no calibrated data, `norm_mean` and `norm_std`
#' are required to be specified (see the followings).
#' @param `norm_mean` : float, default = NULL
#' This argument serves as the mean value to normalize the observed data
#' if no calibrated data specified.
#' @param `norm_std` : float, default = NULL
#' This argument serves as the standard deviation value to normalize
#' the observed data if no calibrated data specified.
#' @param `center` : float, default = NULL
#' If specify `center`, it serves as the center value
#' of the EWMA chart. By default = NULL, it would be the mean value
#' of the (normalized) observed data or calibration data if specified
#' @param `std_dev` : float, default = NULL
#' If specify `std_dev`, it serves as the sigma value
#' to calculate the control limits. By default = NULL, it would be
#' the standard derivation (`sd.xbar.one()`) of
#' the (normalized) observed data or calibration data if specified
#' @param `lambda` : float, (0, 1]. default = 0.2
#' Discount rate of ewma, usually in (0.2, 0.3).
#' @param `nsigmas` : float, default = 3.
#' The width of the control limits, usually in (2.7, 3.0).
#' @param `span` : int, default = NULL.
#' If specify the `span`, `lambda` = 2/(`span` + 1)
#' @return The EWMA chart
ewma_chart <- function(obs_data,
                       obs_time = NULL,
                       cal_data = NULL,
                       normalize = TRUE,
                       norm_mean = NULL,
                       norm_std = NULL,
                       center = NULL,
                       std_dev = NULL,
                       lambda = 0.2,
                       nsigmas = 3,
                       span = NULL) {
    if (!is.null(span)) {
        lambda <- 2 / (span + 1)
    }

    # If no calibrated data
    if (is.null(cal_data)) {
        if (normalize) {
            if (is.null(norm_mean) | is.null(norm_std)) {
                stop("For normalizing 'obs_data', must specify 'norm_mean' and 'norm_std'")
            }

            obs_data <- scale(obs_data, center = norm_mean, scale = norm_std)
        }

        # If no center/target value
        if (is.null(center)) {
            # if no std value
            if (is.null(std_dev)) {
                qcc_ewma <- qcc::ewma(
                    data = obs_data,
                    lambda = lambda,
                    nsigmas = nsigmas,
                    plot = FALSE
                )
            } else {
                # if exists std
                qcc_ewma <- qcc::ewma(
                    data = obs_data,
                    std.dev = std_dev,
                    lambda = lambda,
                    nsigmas = nsigmas,
                    plot = FALSE
                )
            }
        } else {
            # If exists center/target value
            # if no std value
            if (is.null(std_dev)) {
                qcc_ewma <- qcc::ewma(
                    data = obs_data,
                    center = center,
                    lambda = lambda,
                    nsigmas = nsigmas,
                    plot = FALSE
                )
            } else {
                # if exists std
                qcc_ewma <- qcc::ewma(
                    data = obs_data,
                    center = center,
                    std.dev = std_dev,
                    lambda = lambda,
                    nsigmas = nsigmas,
                    plot = FALSE
                )
            }
        }
    } else {
        # If has calibrated data
        if (normalize) {
            # cal_mean <- mean(cal_data)
            # cal_sd <- sd(cal_data)
            cal_data <- scale(cal_data)
            cal_mean <- attributes(cal_data)$`scaled:center`
            cal_sd <- attributes(cal_data)$`scaled:scale`
            obs_data <- (obs_data - cal_mean) / cal_sd
        }

        # If no center/target value
        if (is.null(center)) {
            # if no std value
            if (is.null(std_dev)) {
                qcc_ewma <- qcc::ewma(
                    data = cal_data,
                    newdata = obs_data,
                    lambda = lambda,
                    nsigmas = nsigmas,
                    plot = FALSE
                )
            } else {
                # if exists std
                qcc_ewma <- qcc::ewma(
                    data = cal_data,
                    newdata = obs_data,
                    std.dev = std_dev,
                    lambda = lambda,
                    nsigmas = nsigmas,
                    plot = FALSE
                )
            }
        } else {
            # If exists center/target value
            # if no std value
            if (is.null(std_dev)) {
                qcc_ewma <- qcc::ewma(
                    data = cal_data,
                    newdata = obs_data,
                    center = center,
                    lambda = lambda,
                    nsigmas = nsigmas,
                    plot = FALSE
                )
            } else {
                # if exists std
                qcc_ewma <- qcc::ewma(
                    data = cal_data,
                    newdata = obs_data,
                    center = center,
                    std.dev = std_dev,
                    lambda = lambda,
                    nsigmas = nsigmas,
                    plot = FALSE
                )
            }
        }
    }

    # Shape EWMA data for plotting
    if (is.null(cal_data)) {
        df_limits <- data.frame(qcc_ewma$limits)
        if (!is.null(obs_time)) {
            df_xy <- data.frame(
                x = obs_time,
                y = qcc_ewma$y,
                org_data = obs_data
            )
        } else {
            df_xy <- data.frame(
                x = qcc_ewma$x,
                y = qcc_ewma$y,
                org_data = obs_data
            )
        }

        df <- cbind(df_xy, df_limits)
        df <- df %>%
            mutate(anomaly = case_when(
                # (y < LCL) | (y > UCL) ~ TRUE,
                y > UCL ~ TRUE,
                TRUE ~ FALSE
            ))
    } else {
        n_cal_data <- length(cal_data)

        df_limits <- data.frame(tail(qcc_ewma$limits, -n_cal_data))
        if (!is.null(obs_time)) {
            df_xy <- data.frame(
                x = obs_time,
                y = tail(qcc_ewma$y, -n_cal_data),
                org_data = obs_data
            )
        } else {
            df_xy <- data.frame(
                x = tail(qcc_ewma$x, -n_cal_data),
                y = tail(qcc_ewma$y, -n_cal_data),
                org_data = obs_data
            )
        }

        df <- cbind(df_xy, df_limits)
        df <- df %>%
            mutate(anomaly = case_when(
                # (y < LCL) | (y > UCL) ~ TRUE,
                y > UCL ~ TRUE,
                TRUE ~ FALSE
            ))
    }

    attr(df, "lambda") <- lambda
    attr(df, "nsigmas") <- nsigmas
    attr(df, "center") <- qcc_ewma$center
    attr(df, "std_dev") <- qcc_ewma$std.dev
    attr(df, "normalize") <- normalize
    attr(df, "norm_mean") <- norm_mean
    attr(df, "norm_std") <- norm_std

    return(df)
}

#' Get the anomaly table for report rendering
get_anomaly_table <- function(ewma_data) {
    anomaly_table <- ewma_data %>%
        dplyr::filter(anomaly == TRUE)

    normalize <- attr(ewma_data, "normalize")
    norm_mean <- attr(ewma_data, "norm_mean")
    norm_std <- attr(ewma_data, "norm_std")
    if (normalize) {
        anomaly_table <- anomaly_table %>%
            dplyr::mutate(`Case Count` = org_data * norm_std + norm_mean) %>%
            dplyr::select(x, y, `Case Count`, org_data) %>%
            dplyr::rename(
                `Onset Date` = x,
                `EWMA Statistic` = y,
                `Normalized Case Count` = org_data
            )
    } else {
        anomaly_table <- anomaly_table %>%
            dplyr::select(x, y, org_data) %>%
            dplyr::rename(
                `Onset Date` = x,
                `EWMA Statistic` = y,
                `Case Count` = org_data
            )
    }

    return(anomaly_table)
}


#' Merge EWMA data with the Historical ODRS (aggregated)
merge_ewma_hisODRS <- function(ewma_data, his_ODRS_agg) {
    check_agg <- attr(his_ODRS_agg, "aggregated")
    if (is.null(check_agg)) {
        stop("'his_ODRS_agg' must be aggregated data after calling 'super_agg' function")
    }

    his_ODRS_agg <- his_ODRS_agg %>%
        dplyr::select(`Onset Date`, `Case Count`) %>%
        dplyr::rename(
            x = `Onset Date`,
            his_data = `Case Count`
        )

    ewma_data <- ewma_data %>%
        dplyr::left_join(his_ODRS_agg, by = "x") %>%
        dplyr::mutate_if(is.numeric, coalesce, 0)

    return(ewma_data)
}




#' Plot EWMA chart
#'
#' @param ewma_data: dataframe
#' Obtained from `ewma_chart` function
#' @param last_update_date: string
#' The last updated date with format "YYYY-mm-dd"
#' @param his_ODRS_agg : dataframe, default = NULL
#' The aggregated historical ODRS data, which is used to
#' highlight the back-filling proportions
#' @param show_days: string or numerical, default = "all"
#' How many days to disply. By default, disply all days. By specifying
#' `show_days = 10` or any integer values, the plot
#' would only show the last number of days
#' @param recover_normalize: boolean, default = FALSE
#' If `ewma_data` is obtained from normalized counts, this option
#' is used to recover the counts to be the original scale
#' @param title: string, default = NULL
#' The plot title would be "Analysis Chart: {title}"
#' @param legend_position: string or two numerical values
#' Control the position of legend. By default, the position of
#' legend is "bottom".
#' @param legend_just: string or two numerical values
#' Control the "anchoring point" of the legend. By default,
#' the anchoring position of legend is "center".
plot_ewma_chart <- function(ewma_data,
                            last_update_date,
                            his_ODRS_agg = NULL,
                            show_days = "all",
                            recover_normalize = FALSE,
                            title = NULL,
                            legend_position = "bottom",
                            legend_just = "center") {

    # get attributes in ewma_data
    lamb <- attr(ewma_data, "lambda")
    nsigmas <- attr(ewma_data, "nsigmas")
    center <- attr(ewma_data, "center")
    # std_dev <- attr(ewma_data, "std_dev")
    normalize <- attr(ewma_data, "normalize")
    norm_mean <- attr(ewma_data, "norm_mean")
    norm_std <- attr(ewma_data, "norm_std")

    # filter ewma_data by days
    if (class(show_days) == "character") {
        if (show_days != "all") {
            stop("'show_days' must be either 'all' or interger value")
        }
    } else {
        show_days <- as.integer(show_days)
        if (is.na(show_days)) {
            stop("'show_days' must be either 'all' or integer value")
        }
        ewma_data <- ewma_data %>%
            dplyr::top_n(n = show_days, wt = x)
    }

    # set plot title
    base_title <- "Surveillance Chart"
    if (!is.null(title)) {
        title <- paste(base_title, title, sep = ": ")
    } else {
        title <- base_title
    }

    # set subtitle for showing parameters used in EWMA
    if (class(show_days) != "character") {
        subtitle <- bquote("Display last" ~ .(show_days) ~ "days." ~ "EWMA parameters: " ~ lambda == .(lamb) ~ "," ~ k == .(nsigmas))
    } else {
        subtitle <- bquote("EWMA parameters: " ~ lambda == .(lamb) ~ "," ~ k == .(nsigmas))
    }

    # subtitle <- bquote("EWMA parameters: " ~ lambda == .(lamb) ~ "," ~ k == .(nsigmas))

    # set caption
    caption_line_1 <- "Data Source: Ohio Disease Reporting System (ODRS) https://coronavirus.ohio.gov/wps/portal/gov/covid-19/dashboards"
    caption_line_2 <- paste("Last Updated", format(as.Date(last_update_date), "%B %d, %Y"), sep = ": ")
    caption <- paste(caption_line_1, caption_line_2, sep = "\n")


    # when data has been normalized
    if (normalize) {
        if (recover_normalize) {
            # covert the normalized data to be the orginal scale
            ewma_data <- ewma_data %>%
                mutate(org_data = org_data * norm_std + norm_mean)

            center <- round(center * norm_std + norm_mean)

            title <- paste(title, "(Expect", center, "Daily)", sep = " ")

            data_names <- c("Daily Cases", "EWMA Statistics")
        } else {
            center <- round(center)
            title <- paste(title, "(Expect", center, "Daily)", sep = " ")
            data_names <- c("Daily Cases (normalize)", "EWMA Statistics")
        }
    } else {
        # when data has not been normalized
        center <- round(center)
        title <- paste(title, "(Expect", center, "Daily)", sep = " ")
        data_names <- c("Daily Cases", "EWMA Statistics")
    }

    # UCL text position
    min_x <- min(ewma_data$x)
    max_y <- max(ewma_data$UCL) + 0.1

    # Plot
    if (!is.null(his_ODRS_agg)) {
        his_last_date <- attr(his_ODRS_agg, "last_update_date")
        his_last_date <- as.Date(his_last_date)

        ewma_data_merge <- merge_ewma_hisODRS(ewma_data, his_ODRS_agg)
        ewma_data_merge <- ewma_data_merge %>%
            dplyr::select(x, org_data, his_data) %>%
            dplyr::mutate(reported_cases_change = org_data - his_data) %>%
            # dplyr::mutate(reported_cases_change = case_when(
            #     reported_cases_change < 0 ~ 0,
            #     TRUE ~ reported_cases_change
            # )) %>%
            dplyr::mutate(
                daily_cases = case_when(
                    reported_cases_change < 0 ~ org_data,
                    TRUE ~ org_data - reported_cases_change
                )
            ) %>%
            dplyr::select(x, daily_cases, reported_cases_change) %>%
            tidyr::gather(key = "bartype", value = "count", -x)


        diff_days <- difftime(as.Date(last_update_date), his_last_date, units = c("days"))
        diff_days <- as.numeric(diff_days)

        if (diff_days == 1) {
            new_bar_name <- "Last 24 Hours Reported Cases Change"
        } else {
            new_bar_name <- paste("Last", diff_days, "Days Reported Cases Change")
        }

        # ewma_data_merge <- ewma_data_merge %>%
        #     mutate(bartype = case_when(
        #         bartype == "reported_cases_change" ~ new_bar_name,
        #         TRUE ~ "Daily Cases"
        #     ))

        p <- ggplot() +
            geom_col(
                data = ewma_data_merge,
                mapping = aes(
                    x = x,
                    y = count,
                    fill = bartype
                    # text = paste0(
                    #     "Date: ", x,
                    #     "<br>", bartype, ": ", count
                    # )
                ),
                position = position_stack(reverse = TRUE)
            ) +
            scale_fill_manual(
                values = c(colors_custom$light_blue, colors_custom$dark_orange),
                labels = c(data_names[1], new_bar_name)
            )
    } else {
        p <- ggplot() +
            geom_col(
                data = ewma_data,
                mapping = aes(x = x, y = org_data, fill = data_names[1])
            ) +
            scale_fill_manual(values = colors_custom$light_blue)
    }

    # Adding EWMA lines and dots
    p <- p +
        geom_line(
            data = ewma_data,
            mapping = aes(x = x, y = y, linetype = data_names[2])
        ) +
        geom_point(
            data = ewma_data,
            mapping = aes(x = x, y = y, color = anomaly)
        ) +
        geom_line(
            data = ewma_data,
            mapping = aes(x = x, y = UCL), linetype = "dashed"
        ) +
        annotate(
            geom = "text",
            x = min_x,
            y = max_y,
            label = "UCL",
            hjust = "left",
            vjust = "bottom"
        ) +
        # scale_fill_manual(values = colors_custom$light_blue) +
        scale_linetype_manual(values = 1)

    anomaly_type <- unique(ewma_data$anomaly)
    if (length(anomaly_type) > 1) {
        p <- p +
            scale_color_manual(
                values = c(colors_custom$black, colors_custom$red),
                labels = c("Normality", "Anomaly")
            )
    } else {
        if (anomaly_type) {
            p <- p +
                scale_color_manual(values = colors_custom$red, labels = "Anomaly")
        } else {
            p <- p +
                scale_color_manual(values = colors_custom$black, labels = "Normality")
        }
    }

    p <- p +
        labs(
            title = title,
            subtitle = subtitle,
            caption = caption,
            x = "Onset Date",
            y = "Summary Statistics",
            color = "Anomaly"
        ) +
        guides(
            fill = guide_legend(title = NULL, order = 1, direction = "vertical"),
            linetype = guide_legend(title = NULL, order = 2, direction = "vertical"),
            color = guide_legend(title = NULL, order = 3, direction = "vertical")
        ) +
        theme_customized(
            legend_position = legend_position,
            legend_just = legend_just
        )

    return(p)
}








# ' EWMA chart (Plotly version)
plotly_ewma_chart <- function(ewma_data,
                              last_update_date,
                              his_ODRS_agg = NULL,
                              show_days = "all",
                              recover_normalize = FALSE,
                              title = NULL) {

    # get attributes in ewma_data
    lamb <- attr(ewma_data, "lambda")
    nsigmas <- attr(ewma_data, "nsigmas")
    center <- attr(ewma_data, "center")
    # std_dev <- attr(ewma_data, "std_dev")
    normalize <- attr(ewma_data, "normalize")
    norm_mean <- attr(ewma_data, "norm_mean")
    norm_std <- attr(ewma_data, "norm_std")

    # filter ewma_data by days
    if (class(show_days) == "character") {
        if (show_days != "all") {
            stop("'show_days' must be either 'all' or interger value")
        }
    } else {
        show_days <- as.integer(show_days)
        if (is.na(show_days)) {
            stop("'show_days' must be either 'all' or integer value")
        }
        ewma_data <- ewma_data %>%
            dplyr::top_n(n = show_days, wt = x)
    }

    # set plot title
    base_title <- "Surveillance Chart"
    if (!is.null(title)) {
        title <- paste(base_title, title, sep = ": ")
    } else {
        title <- base_title
    }

    # set subtitle for showing parameters used in EWMA

    # subtitle <- bquote("EWMA parameters: " ~ lambda == .(lamb) ~ "," ~ k == .(nsigmas))
    subtitle <- paste0("\\text{EWMA parameters: } \\lambda = ", lamb, ", k = ", nsigmas)


    # set caption
    caption_line_1 <- "Data Source: Ohio Disease Reporting System (ODRS) https://coronavirus.ohio.gov/wps/portal/gov/covid-19/dashboards"
    caption_line_2 <- paste("Last Updated", format(as.Date(last_update_date), "%B %d, %Y"), sep = ": ")
    caption <- paste(caption_line_1, caption_line_2, sep = "<br>")


    # when data has been normalized
    if (normalize) {
        if (recover_normalize) {
            # covert the normalized data to be the orginal scale
            ewma_data <- ewma_data %>%
                mutate(org_data = org_data * norm_std + norm_mean)

            center <- round(center * norm_std + norm_mean)

            title <- paste(title, "(Expect", center, "Daily)", sep = " ")

            data_names <- c("Daily Cases", "EWMA Statistics")
        } else {
            center <- round(center)
            title <- paste(title, "(Expect", center, "Daily)", sep = " ")
            data_names <- c("Daily Cases (normalize)", "EWMA Statistics")
        }
    } else {
        # when data has not been normalized
        center <- round(center)
        title <- paste(title, "(Expect", center, "Daily)", sep = " ")
        data_names <- c("Daily Cases", "EWMA Statistics")
    }

    # UCL text position
    min_x <- min(ewma_data$x)
    max_y <- max(ewma_data$UCL) + 0.1

    # Plot
    # If highlight historical data
    if (!is.null(his_ODRS_agg)) {
        his_last_date <- attr(his_ODRS_agg, "last_update_date")
        his_last_date <- as.Date(his_last_date)

        ewma_data <- merge_ewma_hisODRS(ewma_data, his_ODRS_agg)
        ewma_data <- ewma_data %>%
            dplyr::mutate(reported_cases_change = org_data - his_data) %>%
            dplyr::mutate(
                daily_cases = case_when(
                    reported_cases_change < 0 ~ org_data,
                    TRUE ~ org_data - reported_cases_change
                )
            )

        diff_days <- difftime(as.Date(last_update_date), his_last_date, units = c("days"))
        diff_days <- as.numeric(diff_days)

        if (diff_days == 1) {
            new_bar_name <- "Last 24 Hours Reported Cases Change"
        } else {
            new_bar_name <- paste("Last", diff_days, "Days Reported Cases Change")
        }

        p <- plot_ly(
            data = ewma_data,
            x = ~x
        ) %>%
            add_bars(
                y = ~daily_cases,
                name = data_names[1],
                marker = list(color = colors_custom$light_blue),
                text = ~ paste0(
                    data_names[1], ": ", org_data
                ),
                # hoverinfo = "x+text"
                hovertemplate = paste0(
                    "%{x|%b %d}",
                    "<br>",
                    "%{text}",
                    "<extra></extra>"
                )
            ) %>%
            add_bars(
                y = ~reported_cases_change,
                name = new_bar_name,
                marker = list(color = colors_custom$dark_orange),
                text = ~ paste0(
                    new_bar_name, ": ", reported_cases_change
                ),
                hoverinfo = "text"
            ) %>%
            # setting "relative" would stack negative values below x
            plotly::layout(barmode = "relative")
    } else {
        # If no historical data
        p <- plot_ly(
            data = ewma_data,
            x = ~x
        ) %>%
            add_bars(
                y = ~org_data,
                name = data_names[1],
                marker = list(color = colors_custom$light_blue),
                text = ~ paste0(
                    data_names[1], ": ", org_data
                ),
                hovertemplate = paste0(
                    "%{x|%b %d}",
                    "<br>",
                    "%{text}",
                    "<extra></extra>"
                )
            )
    }

    # Adding EWMA lines
    p <- p %>%
        add_trace(
            y = ~y,
            name = data_names[2],
            type = "scatter",
            mode = "lines",
            line = list(color = colors_custom$black),
            hoverinfo = "none"
        )
    # Adding UCL
    p <- p %>%
        add_trace(
            y = ~UCL,
            name = "Upper Control Limit (UCL)",
            type = "scatter",
            mode = "lines",
            line = list(color = colors_custom$black, dash = "dash"),
            hoverinfo = "none",
            showlegend = FALSE
        )

    # UCL annotation
    p <- p %>%
        add_annotations(
            xref = "x",
            yref = "y",
            x = min_x,
            y = max_y,
            xanchor = "auto",
            yanchor = "bottom",
            showarrow = FALSE,
            text = "UCL"
        )

    anomaly_type <- unique(ewma_data$anomaly)
    if (length(anomaly_type) > 1) {
        p <- p %>%
            add_trace(
                data = ewma_data %>% filter(anomaly == TRUE),
                x = ~x,
                y = ~y,
                name = "Anomaly",
                type = "scatter",
                mode = "markers",
                marker = list(color = colors_custom$red),
                # hoverinfo = "x+name"
                hovertemplate = paste0(
                    "%{x|%b %d}"
                )
            ) %>%
            add_trace(
                data = ewma_data %>% filter(anomaly == FALSE),
                x = ~x,
                y = ~y,
                name = "Normality",
                type = "scatter",
                mode = "markers",
                marker = list(color = colors_custom$black),
                # hoverinfo = "x+name"
                hovertemplate = paste0(
                    "%{x|%b %d}"
                )
            )
    } else {
        if (anomaly_type) {
            p <- p %>%
                add_trace(
                    y = ~y,
                    name = "Anomaly",
                    type = "scatter",
                    mode = "markers",
                    marker = list(color = colors_custom$red),
                    # hoverinfo = "x+name"
                    hovertemplate = paste0(
                        "%{x|%b %d}"
                    )
                )
        } else {
            p <- p %>%
                add_trace(
                    y = ~y,
                    name = "Normality",
                    type = "scatter",
                    mode = "markers",
                    marker = list(color = colors_custom$black),
                    # hoverinfo = "x+name"
                    hovertemplate = paste0(
                        "%{x|%b %d}"
                    )
                )
        }
    }

    p <- p %>%
        plotly::layout(
            hovermode = "x",
            title = list(
                text = paste0(
                    title,
                    "<br>",
                    "<sup>",
                    caption_line_2,
                    "</sup>"
                ),
                xref = "paper",
                yref = "paper",
                x = 0
            ),

            # annotations = list(
            #     text = caption,
            #     showarrow = FALSE,
            #     align = "left",
            #     xref = "paper",
            #     yref = "paper",
            #     xanchor = "auto",
            #     yanchor = "auto",
            #     x = 0, y = -0.25
            # ),
            xaxis = list(
                title = "",
                type = "date",
                tickformat = "%b %d<br>(%a)",
                showspikes = TRUE,
                spikemode = "toaxis",
                spikesnap = "cursor",
                ticks = "outside",
                ticklen = 5,
                tickwidth = 2,
                tickcolor = colors_custom$black,
                showline = TRUE,
                zeroline = FALSE,
                gridcolor = colors_custom$white
            ),
            yaxis = list(
                title = "",
                ticks = "outside",
                ticklen = 5,
                tickwidth = 2,
                tickcolor = colors_custom$black,
                showline = TRUE,
                zeroline = FALSE,
                gridcolor = colors_custom$white
            ),
            plot_bgcolor = colors_custom$blue_gray_2,
            paper_bgcolor = colors_custom$blue_gray_1,
            margin = list(l = 20, r = 20, t = 50, b = 80),
            legend = list(orientation = "h", traceorder = "normal", y = -0.2),
            font = list(color = "black")
        ) %>%
        plotly::config(
            displaylogo = FALSE, # remove plotly logo
            mathjax = "cdn"
        )

    return(p)
}


###############################################
########### Map Related Functions #############
###############################################


# #' Get merged geo-data with cummulative cases counts
# #' @param `data`: dataframe
# #' The ODRS COVID dataframe
# get_merged_geo_count <- function(data) {
#     ohio_map_df <- ggplot2::map_data("county", "ohio")
#     df_agg_count <- data %>%
#         group_by(County, `Hospital Preparedness Region`) %>%
#         summarise_at("Case Count", list(sum)) %>%
#         mutate(subregion = tolower(County))

#     ohio_map_df <- ohio_map_df %>%
#         left_join(df_agg_count, by = "subregion") %>%
#         select(-region)

#     return(ohio_map_df)
# }


#' Get merged geo-data with cummulative cases counts
#' @param `data`: dataframe
#' The ODRS COVID dataframe
#' @param ohio_county_shapefile_path string, default = NULL
#' By default, it would use maps::map(database = "county",
#' regions = "ohio", plot = FALSE, fill = TRUE) to load
#' the map data.
#' It also allows to specify users' own shapefile path
#' @return a sf object Ohio map data with counts
get_merged_geo_count <- function(data, ohio_county_shapefile_path = NULL) {

    # If do not specify ohio_county_shapefile_path
    if (is.null(ohio_county_shapefile_path)) {
        # get ohio map data as sf object
        ohio_map_sf <- sf::st_as_sf(
            maps::map(
                database = "county",
                regions = "ohio",
                plot = FALSE,
                fill = TRUE
            )
        )

        # form the county name and obtain the center long and lat
        ohio_map_sf <- ohio_map_sf %>%
            mutate(
                County = str_replace(
                    ID,
                    pattern = "ohio,",
                    replacement = ""
                )
            ) %>%
            mutate(County = str_to_title(County)) %>%
            rename(geometry = geom) %>%
            mutate(
                center_long = map_dbl(geometry, ~ st_centroid(.x)[[1]]),
                center_lat = map_dbl(geometry, ~ st_centroid(.x)[[2]])
            )
    } else {
        # If specify ohio_county_shapefile_path
        ohio_map_sf <- sf::st_read(ohio_county_shapefile_path)

        ohio_map_sf <- ohio_map_sf %>%
            dplyr::rename(County = NAME) %>%
            dplyr::mutate(
                center_long = map_dbl(geometry, ~ st_centroid(.x)[[1]]),
                center_lat = map_dbl(geometry, ~ st_centroid(.x)[[2]])
            )
    }

    # get aggregated case counts
    df_agg_count <- data %>%
        group_by(County, `Hospital Preparedness Region`) %>%
        summarise_at("Case Count", list(sum))

    # get aggregated death counts
    df_agg_death <- data %>%
        group_by(County) %>%
        summarise_at("Death Count", list(sum))

    # get aggregated hospitalized counts
    df_agg_hosp <- data %>%
        group_by(County) %>%
        summarise_at("Hospitalized Count", list(sum))

    df_agg_all <- reduce(
        list(df_agg_count, df_agg_death, df_agg_hosp),
        left_join,
        by = "County"
    )

    df_agg_all <- df_agg_all %>%
        dplyr::ungroup()

    ohio_map_sf <- ohio_map_sf %>%
        dplyr::mutate_at("County", as.character)


    # Merge counts
    ohio_map_sf <- ohio_map_sf %>%
        dplyr::left_join(df_agg_all, by = "County") %>%
        dplyr::mutate_at("County", as.character)

    # Merge Zone numbers
    region_zone_df <- get_region_zone_county_df() %>%
        dplyr::select(`Hospital Preparedness Region`, Zone)

    ohio_map_sf <- ohio_map_sf %>%
        dplyr::left_join(region_zone_df, by = "Hospital Preparedness Region")


    attr(ohio_map_sf, "last_update_date") <- attr(data, "last_update_date")

    return(ohio_map_sf)
}


#' Get Map centroid data in the State of Ohio
#' @param ohio_county_shapefile_path string, default = NULL
#' By default, it would use maps::map(database = "county",
#' regions = "ohio", plot = FALSE, fill = TRUE) to load
#' the map data.
#' It also allows to specify users' own shapefile path
get_centroid_ohio <- function(ohio_county_shapefile_path = NULL) {
    # If do not specify ohio_county_shapefile_path
    # Use default ohio county map
    # NOTE: the default one does not have high resolution
    if (is.null(ohio_county_shapefile_path)) {
        ohio_map_sf <- sf::st_as_sf(
            maps::map(
                database = "county",
                regions = "ohio",
                plot = FALSE,
                fill = TRUE
            )
        )

        ohio_map_sf <- ohio_map_sf %>%
            mutate(County = str_replace(ID, pattern = "ohio,", replacement = "")) %>%
            mutate(County = str_to_title(County)) %>%
            rename(geometry = geom)
    } else {
        # If specify ohio_county_shapefile_path
        ohio_map_sf <- sf::st_read(ohio_county_shapefile_path)
        ohio_map_sf <- ohio_map_sf %>%
            dplyr::rename(County = NAME)
    }


    center_ohio_df <- sf::st_coordinates(sf::st_centroid(ohio_map_sf))
    center_ohio_df <- as.data.frame(center_ohio_df) %>%
        mutate(County = ohio_map_sf$County) %>%
        rename(center_long = X, center_lat = Y) %>%
        mutate_if(is.factor, as.character)

    center_ohio_df <- center_ohio_df %>%
        left_join(get_hospital_region(), by = "County")

    return(center_ohio_df)
}

#' Map plot theme
theme_customized_map <- function(base_size = 11, base_family = "sans",
                                 legend_position = "none", legend_just = "center") {
    half_line <- base_size / 2
    ret <- ggthemes::theme_foundation(base_size = base_size, base_family = base_family) +
        theme(
            ## Base setting for lines, rect, text
            line = element_line(colour = "black"),
            # remove all rectangular elements' fill value and no lines
            rect = element_rect(fill = NA, colour = NA, linetype = 0),
            text = element_text(colour = "black"),
            ## Plot setting: background, margin, titles
            plot.background = element_rect(fill = colors_custom$blue_gray_1, color = NA),
            plot.margin = unit(c(6, 5, 6, 5) * 2, "points"),
            plot.title = element_text(size = base_size, hjust = 0, vjust = 1, face = "bold", margin = margin(b = half_line)),
            plot.title.position = "panel",
            plot.subtitle = element_text(size = rel(0.8), hjust = 0, vjust = 1, margin = margin(b = half_line)),
            plot.caption = element_text(size = rel(0.6), hjust = 0, vjust = 1, margin = margin(t = half_line)),
            plot.caption.position = "panel",
            plot.tag = element_text(size = rel(1), hjust = 0.5, vjust = 0.5),
            plot.tag.position = "topleft",

            ## Panel setting: background, grid, border...
            panel.background = element_rect(fill = colors_custom$blue_gray_2, linetype = 0),
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            # panel.grid.major.x = element_blank(),
            # panel.grid.major.y = element_blank(),
            # panel.spacing = unit(0.25, "lines"),

            ## Axis setting: text, lines, ticks
            axis.line = element_blank(),
            axis.text = element_blank(),
            # axis.text.x = element_text(vjust = 0, margin = margin(b = half_line, unit = "pt")),
            # axis.text.y = element_text(hjust = 0, margin = margin(r = half_line, unit = "pt")),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            # axis.title.x = element_blank(), # remove x title
            # axis.title.y = element_text(angle = 90),
            # axis.ticks.length = unit(-base_size * 0.5, "points"),

            # Legend setting: background, margin, position
            legend.background = element_rect(linetype = 0),
            legend.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
            # legend.box.background = element_rect(fill = "#e5e5e5", size = .1),
            legend.position = legend_position,
            legend.justification = legend_just,
            legend.direction = NULL,
            legend.title = element_text(size = rel(0.8), hjust = 0),
            legend.title.align = NULL,
            legend.text = element_text(size = rel(0.8)),
            legend.text.align = NULL,
            legend.spacing = unit(base_size, "points"),
            legend.key = element_rect(fill = colors_custom$blue_gray_2, linetype = 0),
            legend.key.size = unit(1.2, "lines"),
            legend.key.height = NULL,
            legend.key.width = NULL,

            # strip.background = element_rect(fill = bgcolors["ebg"], colour = NA, linetype = 0),
            # strip.text = element_text(size = rel(1.25)),
            # strip.text.x = element_text(),
            # strip.text.y = element_text(angle = -90),
            complete = TRUE
        )

    return(ret)
}


#' Map plot theme (no background)
theme_customized_map_nobg <- function(base_size = 11, base_family = "sans",
                                      legend_position = "none", legend_just = "center") {
    half_line <- base_size / 2
    ret <- ggthemes::theme_foundation(base_size = base_size, base_family = base_family) +
        theme(
            ## Base setting for lines, rect, text
            line = element_line(colour = "black"),
            # remove all rectangular elements' fill value and no lines
            rect = element_rect(fill = NA, colour = NA, linetype = 0),
            text = element_text(colour = "black"),
            ## Plot setting: background, margin, titles
            plot.background = element_blank(),
            plot.margin = unit(c(6, 5, 6, 5) * 2, "points"),
            plot.title = element_text(size = base_size, hjust = 0, vjust = 1, face = "bold", margin = margin(b = half_line)),
            plot.title.position = "panel",
            plot.subtitle = element_text(size = rel(0.8), hjust = 0, vjust = 1, margin = margin(b = half_line)),
            plot.caption = element_text(size = rel(0.6), hjust = 0, vjust = 1, margin = margin(t = half_line)),
            plot.caption.position = "panel",
            plot.tag = element_text(size = rel(1), hjust = 0.5, vjust = 0.5),
            plot.tag.position = "topleft",

            ## Panel setting: background, grid, border...
            panel.background = element_rect(fill = colors_custom$blue_gray_2, linetype = 0),
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            # panel.grid.major.x = element_blank(),
            # panel.grid.major.y = element_blank(),
            # panel.spacing = unit(0.25, "lines"),

            ## Axis setting: text, lines, ticks
            axis.line = element_blank(),
            axis.text = element_blank(),
            # axis.text.x = element_text(vjust = 0, margin = margin(b = half_line, unit = "pt")),
            # axis.text.y = element_text(hjust = 0, margin = margin(r = half_line, unit = "pt")),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            # axis.title.x = element_blank(), # remove x title
            # axis.title.y = element_text(angle = 90),
            # axis.ticks.length = unit(-base_size * 0.5, "points"),

            # Legend setting: background, margin, position
            legend.background = element_rect(linetype = 0),
            legend.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
            # legend.box.background = element_rect(fill = "#e5e5e5", size = .1),
            legend.position = legend_position,
            legend.justification = legend_just,
            legend.direction = NULL,
            legend.title = element_text(size = rel(0.8), hjust = 0),
            legend.title.align = NULL,
            legend.text = element_text(size = rel(0.8)),
            legend.text.align = NULL,
            legend.spacing = unit(base_size, "points"),
            legend.key = element_rect(fill = colors_custom$blue_gray_2, linetype = 0),
            legend.key.size = unit(1.2, "lines"),
            legend.key.height = NULL,
            legend.key.width = NULL,

            # strip.background = element_rect(fill = bgcolors["ebg"], colour = NA, linetype = 0),
            # strip.text = element_text(size = rel(1.25)),
            # strip.text.x = element_text(),
            # strip.text.y = element_text(angle = -90),
            complete = TRUE
        )

    return(ret)
}


#' Map Plot Case Counts
#' @param `data`: dataframe
#' The ODRS COVID dataframe
#' @param last_update_date : default = auto
#' The last updated date for the data
#' By default, the `last_update_date` = attr(data, "last_update_date")
#' It also allow to specify the `last_update_date` with format "%Y-%m-%d"
#' @param title : string, default = NULL
#' By default, the title name is "Cumulative COVID-19 Cases by County"
#' Do not change the title value since it would be automatically
#' changed once specifying `region`
#' @param region_highlight : str or vector, default = NULL
#' Highlight specific regions. By default, no regions would be highlighted.
#' 1. Allow to specify a vector of region numbers, e.g., c(1,2,3) or c(1)
#' aggragate data for specific hospital regions. The valid number is from 1 to 8
#' 2. Allow to specify a string or a vector of region names, i.e., county names
#' @param filter_region : boolean, default = FALSE
#' Whether or not only display the highlighted regions if specifying
#' `region_highlight`
plot_counts_map <- function(data,
                            last_update_date = "auto",
                            title = NULL,
                            region_highlight = NULL,
                            filter_region = FALSE) {
    if (last_update_date == "auto") {
        last_update_date <- attr(data, "last_update_date")
    } else {
        check_valid_date <- as.Date(last_update_date, "%Y-%m-%d")
        if (is.na(check_valid_date)) {
            stop("'last_update_date' is either 'auto' or date format with '%Y-%m-%d'")
        }
    }

    ohio_map_df <- get_merged_geo_count(data = data)

    # set plot title
    base_title <- "Cumulative COVID-19 Cases by County"
    if (!is.null(title)) {
        title <- paste(base_title, title, sep = ": ")
    } else {
        title <- base_title
    }

    # set caption
    caption <- paste("Last Updated", last_update_date, sep = ": ")

    # For all Ohio
    if (is.null(region_highlight)) {
        p <- ggplot(
            data = ohio_map_df,
            aes(
                text = paste0(
                    County,
                    "<br>Total Cases: ", `Case Count`,
                    "<br>Total Deaths: ", `Death Count`,
                    "<br>Total Hospitalizations: ", `Hospitalized Count`
                )
            )
        ) +
            geom_sf(mapping = aes(fill = `Case Count`), color = "black") +
            labs(
                # title = title,
                caption = caption
            )
    } else {
        # If specify the region to highlight: county names
        county_names <- sort(unique(ohio_map_df$County))
        colors_list <- rainbow(length(county_names))
        # colors_list <- hcl.colors(length(county_names), "Set 2")
        if (typeof(region_highlight) == "character") {
            if (all(region_highlight %in% county_names)) {
                color_values <- rep("black", length(county_names))
                color_values[county_names %in% region_highlight] <- colors_list[county_names %in% region_highlight]

                if (filter_region) {
                    ohio_map_df <- ohio_map_df %>%
                        dplyr::filter(County %in% region_highlight)

                    region_highlight <- paste(region_highlight, collapse = " & ")
                    base_title <- "Cumulative COVID-19 Cases"
                    title <- paste(base_title, region_highlight, sep = ": ")
                    subtitle <- paste0("Highlight County: ", region_highlight)


                    p <- ggplot() +
                        geom_sf(
                            data = ohio_map_df,
                            mapping = aes(fill = `Case Count`),
                            color = "black"
                        ) +
                        geom_text(
                            data = ohio_map_df,
                            mapping = aes(x = center_long, y = center_lat, label = County),
                            check_overlap = TRUE,
                            size = 2
                        ) +
                        labs(
                            # title = title,
                            title = subtitle,
                            caption = caption
                        )
                } else {
                    region_highlight <- paste(region_highlight, collapse = " & ")
                    subtitle <- paste0("Highlight County: ", region_highlight)
                    p <- ggplot() +
                        geom_sf(
                            data = ohio_map_df,
                            mapping = aes(fill = `Case Count`, color = County)
                        ) +
                        scale_color_manual(values = color_values) +
                        labs(
                            # title = title,
                            # subtitle = subtitle,
                            title = subtitle,
                            caption = caption
                        )
                }
            } else {
                incorrect_names <- region_highlight[!region_highlight %in% county_names]
                incorrect_names <- paste(incorrect_names, collapse = ", ")
                txt <- paste0("Incorrect county names: ", incorrect_names, ". Please specify the correct county names in the State of Ohio.")
                stop(txt)
            }
        } else {
            # If specify the region to highlight: region number
            region_numbers <- c(1:8)
            # colors_list <- rainbow(length(region_numbers))
            colors_list <- hcl.colors(length(region_numbers), "Dark 3")
            if (all(region_highlight %in% region_numbers)) {
                color_values <- rep("black", length(region_numbers))
                color_values[region_numbers %in% region_highlight] <- colors_list[region_numbers %in% region_highlight]

                if (filter_region) {
                    ohio_map_df <- ohio_map_df %>%
                        dplyr::filter(`Hospital Preparedness Region` %in% region_highlight)

                    region_highlight <- paste(region_highlight, collapse = " & ")
                    subtitle <- paste0("Highlight Region: ", region_highlight)

                    region_highlight <- paste("Region", region_highlight)
                    base_title <- "Cumulative COVID-19 Cases"
                    title <- paste(base_title, region_highlight, sep = ": ")


                    p <- ggplot() +
                        geom_sf(
                            data = ohio_map_df,
                            mapping = aes(fill = `Case Count`), color = "black"
                        ) +
                        geom_text(
                            data = ohio_map_df,
                            mapping = aes(x = center_long, y = center_lat, label = County),
                            check_overlap = TRUE,
                            size = 2
                        ) +
                        labs(
                            # title = title,
                            title = subtitle,
                            caption = caption
                        )
                } else {
                    region_highlight <- paste(region_highlight, collapse = " & ")
                    subtitle <- paste0("Highlight Region: ", region_highlight)
                    p <- ggplot() +
                        geom_sf(
                            data = ohio_map_df,
                            mapping = aes(fill = `Case Count`, color = factor(`Hospital Preparedness Region`))
                        ) +
                        scale_color_manual(values = color_values) +
                        labs(
                            # title = title,
                            # subtitle = subtitle,
                            title = subtitle,
                            caption = caption
                        )
                }
            } else {
                incorrect_names <- region_highlight[!region_highlight %in% region_numbers]
                incorrect_names <- paste(incorrect_names, collapse = ", ")
                txt <- paste0("Incorrect region numbers: ", incorrect_names, ". Please specify the region numbers from 1 to 8.")
                stop(txt)
            }
        }
    }

    p <- p +
        scale_fill_gradient_tableau(palette = "Blue") +
        theme_customized_map(legend_position = "none", legend_just = "center")

    return(p)
}





#' Map Plot Case Counts based on Plotly
plotly_counts_map <- function(ohio_map_df, last_update_date = "auto",
                              title = NULL, show_county_names = FALSE,
                              region_highlight = NULL,
                              filter_region = FALSE) {
    if (last_update_date == "auto") {
        last_update_date <- attr(ohio_map_df, "last_update_date")
    } else {
        check_valid_date <- as.Date(last_update_date, "%Y-%m-%d")
        if (is.na(check_valid_date)) {
            stop("'last_update_date' is either 'auto' or date format with '%Y-%m-%d'")
        }
    }


    # # set plot title
    # base_title <- "Cumulative COVID-19 Cases by County"
    # if (!is.null(title)) {
    #     title <- paste(base_title, title, sep = ": ")
    # } else {
    #     title <- base_title
    # }

    # # set caption
    # caption <- paste("Last Updated", last_update_date, sep = ": ")

    # For all Ohio
    if (is.null(region_highlight)) {
        subtitle <- NULL

        # county_names_xy <- data.frame(
        #     County = ohio_map_df$County,
        #     center_long = ohio_map_df$center_long,
        #     center_lat = ohio_map_df$center_lat
        # )

        ohio_map_highlight <- plotly::highlight_key(ohio_map_df, ~`Hospital Preparedness Region`)

        p <- plot_ly(
            data = ohio_map_highlight,
            type = "scatter",
            mode = "lines",
            stroke = I("black"),
            colors = "Blues"
        ) %>%
            plotly::add_sf(
                split = ~County,
                color = ~`Case Count`,
                text = ~ paste0(
                    "<b>", County, "</b>",
                    "<br>Region: ", `Hospital Preparedness Region`,
                    "<br>Total Cases: ", `Case Count`,
                    "<br>Total Deaths: ", `Death Count`,
                    "<br>Total Hospitalizations: ", `Hospitalized Count`
                ),
                hoveron = "fills",
                hoverinfo = "text"
            )

        p <- p %>%
            plotly::highlight(
                on = "plotly_click",
                off = "plotly_doubleclick"
            )
    } else {
        # If specify the region to highlight: county names
        county_names <- sort(unique(ohio_map_df$County))
        colors_list <- rainbow(length(county_names))
        # colors_list <- hcl.colors(length(county_names), "Set 2")
        if (typeof(region_highlight) == "character") {
            if (all(region_highlight %in% county_names)) {
                color_values <- rep("black", length(county_names))
                color_values[county_names %in% region_highlight] <- colors_list[county_names %in% region_highlight]

                if (filter_region) {
                    ohio_map_df <- ohio_map_df %>%
                        dplyr::filter(County %in% region_highlight)

                    # region_highlight <- paste(region_highlight, collapse = " & ")
                    # base_title <- "Cumulative COVID-19 Cases"
                    # title <- paste(base_title, region_highlight, sep = ": ")
                    # subtitle <- paste0("Highlight County: ", region_highlight)

                    ohio_map_highlight <- plotly::highlight_key(ohio_map_df, ~County)

                    p <- plot_ly(
                        data = ohio_map_highlight,
                        type = "scatter",
                        mode = "lines",
                        stroke = I("black"),
                        colors = "Blues"
                    ) %>%
                        plotly::add_sf(
                            split = ~County,
                            color = ~`Case Count`,
                            text = ~ paste0(
                                "<b>", County, "</b>",
                                "<br>Region: ", `Hospital Preparedness Region`,
                                "<br>Total Cases: ", `Case Count`,
                                "<br>Total Deaths: ", `Death Count`,
                                "<br>Total Hospitalizations: ", `Hospitalized Count`
                            ),
                            hoveron = "fills",
                            hoverinfo = "text"
                        )

                    p <- p %>%
                        plotly::highlight(
                            on = "plotly_click",
                            off = "plotly_doubleclick"
                        )
                } else {
                    # region_highlight <- paste(region_highlight, collapse = " & ")
                    # subtitle <- paste0("Highlight County: ", region_highlight)

                    ohio_map_highlight <- plotly::highlight_key(ohio_map_df, ~County)
                    p <- plot_ly(
                        data = ohio_map_highlight,
                        type = "scatter",
                        mode = "lines",
                        stroke = ~County,
                        strokes = color_values,
                        colors = "Blues"
                    ) %>%
                        plotly::add_sf(
                            split = ~County,
                            color = ~`Case Count`,
                            text = ~ paste0(
                                "<b>", County, "</b>",
                                "<br>Region: ", `Hospital Preparedness Region`,
                                "<br>Total Cases: ", `Case Count`,
                                "<br>Total Deaths: ", `Death Count`,
                                "<br>Total Hospitalizations: ", `Hospitalized Count`
                            ),
                            hoveron = "fills",
                            hoverinfo = "text"
                        )

                    p <- p %>%
                        plotly::highlight(
                            on = NULL,
                            off = NULL,
                            defaultValues = region_highlight
                        )
                }
            } else {
                incorrect_names <- region_highlight[!region_highlight %in% county_names]
                incorrect_names <- paste(incorrect_names, collapse = ", ")
                txt <- paste0("Incorrect county names: ", incorrect_names, ". Please specify the correct county names in the State of Ohio.")
                stop(txt)
            }
        } else {
            # If specify the region to highlight: region number
            region_numbers <- c(1:8)
            # colors_list <- rainbow(length(region_numbers))
            colors_list <- hcl.colors(length(region_numbers), "Dark 3")
            if (all(region_highlight %in% region_numbers)) {
                color_values <- rep("black", length(region_numbers))
                color_values[region_numbers %in% region_highlight] <- colors_list[region_numbers %in% region_highlight]

                if (filter_region) {
                    ohio_map_df <- ohio_map_df %>%
                        dplyr::filter(`Hospital Preparedness Region` %in% region_highlight)

                    ohio_map_highlight <- plotly::highlight_key(ohio_map_df, ~`Hospital Preparedness Region`)

                    # region_highlight <- paste(region_highlight, collapse = " & ")
                    # subtitle <- paste0("Highlight Region: ", region_highlight)

                    # region_highlight <- paste("Region", region_highlight)
                    # base_title <- "Cumulative COVID-19 Cases"
                    # title <- paste(base_title, region_highlight, sep = ": ")

                    p <- plot_ly(
                        data = ohio_map_highlight,
                        type = "scatter",
                        mode = "lines",
                        stroke = I("black"),
                        colors = "Blues"
                    ) %>%
                        plotly::add_sf(
                            split = ~County,
                            color = ~`Case Count`,
                            text = ~ paste0(
                                "<b>", County, "</b>",
                                "<br>Region: ", `Hospital Preparedness Region`,
                                "<br>Total Cases: ", `Case Count`,
                                "<br>Total Deaths: ", `Death Count`,
                                "<br>Total Hospitalizations: ", `Hospitalized Count`
                            ),
                            hoveron = "fills",
                            hoverinfo = "text"
                        )

                    p <- p %>%
                        plotly::highlight(
                            on = "plotly_click",
                            off = "plotly_doubleclick"
                        )
                } else {
                    # region_highlight <- paste(region_highlight, collapse = " & ")
                    # subtitle <- paste0("Highlight Region: ", region_highlight)
                    ohio_map_highlight <- plotly::highlight_key(ohio_map_df, ~`Hospital Preparedness Region`)
                    p <- plot_ly(
                        data = ohio_map_highlight,
                        type = "scatter",
                        mode = "lines",
                        stroke = ~ as.factor(`Hospital Preparedness Region`),
                        strokes = color_values,
                        colors = "Blues"
                    ) %>%
                        plotly::add_sf(
                            split = ~County,
                            color = ~`Case Count`,
                            text = ~ paste0(
                                "<b>", County, "</b>",
                                "<br>Region: ", `Hospital Preparedness Region`,
                                "<br>Total Cases: ", `Case Count`,
                                "<br>Total Deaths: ", `Death Count`,
                                "<br>Total Hospitalizations: ", `Hospitalized Count`
                            ),
                            hoveron = "fills",
                            hoverinfo = "text"
                        )

                    p <- p %>%
                        plotly::highlight(
                            on = NULL,
                            off = NULL,
                            defaultValues = region_highlight
                        )
                }
            } else {
                incorrect_names <- region_highlight[!region_highlight %in% region_numbers]
                incorrect_names <- paste(incorrect_names, collapse = ", ")
                txt <- paste0("Incorrect region numbers: ", incorrect_names, ". Please specify the region numbers from 1 to 8.")
                stop(txt)
            }
        }
    }

    if (filter_region & !is.null(region_highlight)) {
        font_size <- 8
    } else {
        font_size <- 6
    }

    county_name_annotations <- list(
        visible = show_county_names,
        x = ohio_map_df$center_long,
        y = ohio_map_df$center_lat,
        text = ohio_map_df$County,
        xref = "x",
        yref = "y",
        showarrow = FALSE,
        font = list(size = font_size, color = "black")
    )


    p <- p %>%
        plotly::layout(
            annotations = county_name_annotations,
            showlegend = FALSE,
            plot_bgcolor = colors_custom$blue_gray_2,
            paper_bgcolor = colors_custom$blue_gray_1,
            margin = list(l = 10, r = 10, t = 10, b = 10)
        ) %>%
        plotly::hide_colorbar() %>%
        plotly::config(
            displayModeBar = FALSE # remove modebar
            # displaylogo = FALSE # remove plotly logo
        )

    return(p)
}



#' Convert ggplot to plotly
convert_ggplot2_plotly <- function(p, hide_xtitle = TRUE, hide_ytitle = TRUE) {
    xaxis <- list(
        showspikes = TRUE,
        spikemode = "across",
        spikesnap = "cursor",
        ticks = "outside",
        ticklen = 5,
        tickwidth = 2,
        tickcolor = colors_custom$black,
        showline = TRUE,
        zeroline = FALSE,
        gridcolor = colors_custom$white
    )

    yaxis <- list(
        # showspikes = TRUE,
        # spikemode = "across",
        # spikesnap = "cursor",
        ticks = "outside",
        ticklen = 5,
        tickwidth = 2,
        tickcolor = colors_custom$black,
        showline = TRUE,
        zeroline = FALSE,
        gridcolor = colors_custom$white
    )

    if (hide_xtitle) {
        xaxis$title <- ""
    }

    if (hide_ytitle) {
        yaxis$title <- ""
    }

    p <- ggplotly(p, tooltip = "text") %>%
        plotly::layout(
            hovermode = "x",
            xaxis = xaxis,
            yaxis = yaxis
        ) %>%
        plotly::config(
            # displayModeBar = FALSE # remove modebar
            displaylogo = FALSE # remove plotly logo
        )

    return(p)
}


#' Plotly daily counts for county levels
#' If specify the Date, a line plot would be rendered
#' Otherwise, a bar ploy would be rendered
#' @param data dataframe
#' ODRS data
#' @param county_select string, default = "all"
#' Specify county names
#' @param top_k, default = 10
#' Display top k counties based on the total counts
plotly_counts_county <- function(data,
                                 county_select = "all",
                                 top_k = 10,
                                 last_update_date = "auto",
                                 county_col = "County",
                                 count_col = "Case Count",
                                 date_col = NULL,
                                 show_days = "all",
                                 evenly_space = TRUE) {
    if (last_update_date == "auto") {
        last_update_date <- attr(data, "last_update_date")
    } else {
        check_valid_date <- as.Date(last_update_date, "%Y-%m-%d")
        if (is.na(check_valid_date)) {
            stop("'last_update_date' is either 'auto' or date format with '%Y-%m-%d'")
        }
    }

    if (is.null(date_col)) {
        # cumulative counts for county
        df_agg <- agg_cumu_counts_by_cols(
            data = data,
            group_col = county_col,
            count_col = count_col,
            last_update_date = last_update_date
        )

        county_col_sym <- dplyr::sym(county_col)
        count_col_sym <- dplyr::sym(count_col)

        # filter counties
        if (length(county_select) == 1) {
            if (county_select != "all") {
                df_agg <- df_agg %>%
                    dplyr::filter(!!county_col_sym %in% county_select)
            }
        } else {
            df_agg <- df_agg %>%
                dplyr::filter(!!county_col_sym %in% county_select)
        }

        # filter top k
        if (top_k != "all") {
            df_agg <- df_agg %>%
                dplyr::top_n(n = top_k, wt = !!count_col_sym)
        }


        # Bar plot
        x <- df_agg[count_col][[1]]
        y <- df_agg[county_col][[1]]

        new_df <- data.frame(
            county = y,
            counts = x
        )

        plot_height <- 500 + 10 * nrow(df_agg)

        p <- plot_ly(
            data = new_df,
            x = ~counts,
            y = ~ reorder(county, counts),
            type = "bar",
            marker = list(color = colors_custom$light_blue),
            orientation = "h",
            height = plot_height,
            hovertemplate = ~ paste0(
                "%{y}",
                "<br>",
                "Total Cases: %{x:,}",
                "<extra></extra>"
            )
        )

        p <- p %>%
            plotly::layout(
                xaxis = list(
                    title = "",
                    ticks = "outside",
                    ticklen = 5,
                    tickwidth = 2,
                    tickcolor = colors_custom$black,
                    showline = TRUE,
                    zeroline = FALSE,
                    gridcolor = colors_custom$white
                ),
                yaxis = list(
                    title = "",
                    ticks = "outside",
                    ticklen = 5,
                    tickwidth = 2,
                    tickcolor = colors_custom$black,
                    showline = TRUE,
                    zeroline = FALSE
                    # gridcolor = colors_custom$white
                ),
                plot_bgcolor = colors_custom$blue_gray_2,
                paper_bgcolor = colors_custom$blue_gray_1,
                margin = list(l = 20, r = 20, t = 20, b = 20),
                # legend = list(orientation = "h", traceorder = "normal", y = -0.2),
                font = list(color = "black")
            ) %>%
            plotly::config(
                # remove plotly logo
                displaylogo = FALSE
            )
    } else {
        # ------- daily counts for county ---------
        df_agg <- agg_daily_counts_by_cols(
            data = data,
            date_col = date_col,
            group_col = county_col,
            count_col = count_col,
            last_update_date = last_update_date,
            evenly_space = evenly_space
        )

        county_col_sym <- dplyr::sym(county_col)
        count_col_sym <- dplyr::sym(count_col)
        date_col_sym <- dplyr::sym(date_col)


        # filter by days
        if (class(show_days) == "character") {
            if (show_days != "all") {
                stop("'show_days' must be either 'all' or interger value")
            }
        } else {
            show_days <- as.integer(show_days)
            if (is.na(show_days)) {
                stop("'show_days' must be either 'all' or integer value")
            }
            df_agg <- df_agg %>%
                dplyr::group_by(!!county_col_sym) %>%
                dplyr::top_n(n = show_days, wt = !!date_col_sym) %>%
                dplyr::ungroup()
        }


        # filter counties
        if (length(county_select) == 1) {
            if (county_select != "all") {
                df_agg <- df_agg %>%
                    dplyr::filter(!!county_col_sym %in% county_select)
            }
        } else {
            df_agg <- df_agg %>%
                dplyr::filter(!!county_col_sym %in% county_select)
        }

        # filter top k based on the total cases within all dates or n-days
        if (top_k != "all") {
            county_top_k <- df_agg %>%
                dplyr::group_by(!!county_col_sym) %>%
                dplyr::summarise_at(count_col, list(sum)) %>%
                dplyr::top_n(n = top_k, wt = !!count_col_sym) %>%
                dplyr::pull(!!county_col_sym)

            df_agg <- df_agg %>%
                dplyr::filter(!!county_col_sym %in% county_top_k)
        }

        county_order <- df_agg %>%
            dplyr::group_by(!!county_col_sym) %>%
            dplyr::summarise_at(count_col, list(sum)) %>%
            dplyr::arrange(desc(!!count_col_sym)) %>%
            dplyr::pull(!!county_col_sym)


        df_agg[county_col][[1]] <- factor(df_agg[county_col][[1]], levels = county_order)

        date_v <- df_agg[date_col][[1]]
        counts_v <- df_agg[count_col][[1]]
        county_v <- df_agg[county_col][[1]]
        new_df <- data.frame(
            date = date_v,
            counts = counts_v,
            county = county_v
        )

        # new_df_highlight <- plotly::highlight_key(new_df, ~county)

        colors <- hcl.colors(length(county_v), "Dark 3")

        p <- plot_ly(
            data = new_df,
            x = ~date,
            y = ~counts,
            color = ~county,
            colors = colors,
            type = "scatter",
            mode = "lines",
            hovertemplate = ~ paste0(
                "%{x|%b %d}",
                "<br>",
                "Daily Cases: %{y:,}"
            )
        )

        p <- p %>%
            plotly::layout(
                hovermode = "x",
                xaxis = list(
                    title = "",
                    type = "date",
                    tickformat = "%b %d<br>(%a)",
                    showspikes = TRUE,
                    spikemode = "toaxis",
                    spikesnap = "cursor",
                    ticks = "outside",
                    ticklen = 5,
                    tickwidth = 2,
                    tickcolor = colors_custom$black,
                    showline = TRUE,
                    zeroline = FALSE,
                    gridcolor = colors_custom$white
                    # autorange = TRUE,
                    # fixedrange = FALSE,
                    # rangeslider = list(type = "date")
                ),
                yaxis = list(
                    title = "",
                    ticks = "outside",
                    ticklen = 5,
                    tickwidth = 2,
                    tickcolor = colors_custom$black,
                    showline = TRUE,
                    zeroline = FALSE,
                    gridcolor = colors_custom$white
                    # autorange = TRUE,
                    # fixedrange = FALSE
                ),
                plot_bgcolor = colors_custom$blue_gray_2,
                paper_bgcolor = colors_custom$blue_gray_1,
                margin = list(l = 20, r = 20, t = 20, b = 20),
                # legend = list(orientation = "h", traceorder = "normal", y = -0.2),
                font = list(color = "black")
            ) %>%
            plotly::config(
                # remove plotly logo
                displaylogo = FALSE
            )

        # p <- p %>%
        #     plotly::highlight(
        #         on = "plotly_click",
        #         off = "plotly_doubleclick"
        #     )
    }


    return(p)
}



#' Plotly mobility data
plotly_mobility <- function(mob_data, type_geo, region_name) {
    df <- mob_data %>%
        dplyr::filter(geo_type == type_geo & region == region_name)

    df <- df %>%
        dplyr::mutate(Baseline = 0)

    trans_types <- unique(df$transportation_type)
    colors <- hcl.colors(length(trans_types), "Dark 3")

    p <- plot_ly(
        data = df,
        x = ~Date
    )

    p <- p %>%
        add_trace(
            y = ~ (Routing_Requests_Change - 100) / 100,
            color = ~transportation_type,
            colors = colors,
            type = "scatter",
            mode = "lines",
            hovertemplate = ~ paste0(
                "%{x|%b %d}",
                "<br>",
                "Mobility Trends: %{y:.0%}"
            )
        )

    p <- p %>%
        add_trace(
            y = ~Baseline,
            type = "scatter",
            mode = "lines",
            line = list(color = colors_custom$black, dash = "dash"),
            hoverinfo = "none",
            showlegend = FALSE
        )


    p <- p %>%
        plotly::layout(
            hovermode = "x",
            xaxis = list(
                title = "",
                type = "date",
                tickformat = "%b %d<br>(%a)",
                showspikes = TRUE,
                spikemode = "toaxis",
                spikesnap = "cursor",
                ticks = "outside",
                ticklen = 5,
                tickwidth = 2,
                tickcolor = colors_custom$black,
                showline = TRUE,
                zeroline = FALSE,
                gridcolor = colors_custom$white
                # autorange = TRUE,
                # fixedrange = FALSE,
                # rangeslider = list(type = "date")
            ),
            yaxis = list(
                title = "",
                ticks = "outside",
                ticklen = 5,
                tickwidth = 2,
                tickformat = "%",
                tickcolor = colors_custom$black,
                showline = TRUE,
                zeroline = FALSE,
                gridcolor = colors_custom$white
                # autorange = TRUE,
                # fixedrange = FALSE
            ),
            plot_bgcolor = colors_custom$blue_gray_2,
            paper_bgcolor = colors_custom$blue_gray_1,
            margin = list(l = 20, r = 20, t = 20, b = 20),
            # legend = list(orientation = "h", traceorder = "normal", y = -0.2),
            font = list(color = "black")
        ) %>%
        plotly::config(
            # remove plotly logo
            displaylogo = FALSE
        )

    return(p)
}





#' Plot the vulnerable census tract
plotly_vuln_census_tract <- function(vuln_ct_df, ohio_zip_df) {
    # df_sf <- sf::read_sf(vuln_ct_geojson_file)
    # df_sf <- df_sf %>%
    #     dplyr::mutate_at("COUNTYFP", as.numeric)

    # ohio_zip_df <- readr::read_csv(ohio_zip_file)
    df_sf <- vuln_ct_df %>%
        dplyr::mutate_at("COUNTYFP", as.numeric)

    ohio_zip_df <- ohio_zip_df %>%
        dplyr::select(county_fsip, county_name) %>%
        dplyr::distinct_all()

    df_sf <- df_sf %>%
        dplyr::left_join(ohio_zip_df, by = c("COUNTYFP" = "county_fsip"))

    county_names <- sort(unique(df_sf$county_name))
    colors_list <- hcl.colors(length(county_names), "Dark 3")

    df_sf_highlight <- plotly::highlight_key(df_sf, ~county_name)

    p <- plot_ly(
        data = df_sf_highlight
    ) %>%
        plotly::add_sf(
            type = "scatter",
            mode = "lines",
            stroke = ~ I("black"),
            split = ~GEOID,
            color = ~county_name,
            colors = colors_list,
            text = ~ paste0(
                "<b>", county_name, "</b>",
                "<br>GEOID: ", GEOID
            ),
            hoveron = "fills",
            hoverinfo = "text"
        )

    p <- p %>%
        plotly::highlight(
            on = "plotly_click",
            off = "plotly_doubleclick"
        )

    p <- p %>%
        plotly::layout(
            showlegend = FALSE,
            plot_bgcolor = colors_custom$blue_gray_2,
            paper_bgcolor = colors_custom$blue_gray_1,
            margin = list(l = 10, r = 10, t = 10, b = 10)
        ) %>%
        # plotly::hide_colorbar() %>%
        plotly::config(
            displayModeBar = FALSE # remove modebar
            # displaylogo = FALSE # remove plotly logo
        )

    return(p)
}


#' Leaflet Plot the vulnerable census tract
#' @param vlun_ct_df: a sf object
#' The vulnerable census tracts (geojson)
#' must be imported by sf::read_sf()
#' @param ohio_zip_df: dataframe
#' The ohio zip data with county FSIP
leaflet_vuln_census_tract <- function(vuln_ct_df, ohio_zip_df) {
    df_sf <- vuln_ct_df %>%
        dplyr::mutate_at("COUNTYFP", as.character) %>%
        dplyr::mutate_at("COUNTYFP", parse_number)

    ohio_zip_df <- ohio_zip_df %>%
        dplyr::select(county_fsip, county_name) %>%
        dplyr::distinct_all()

    hospital_county_df <- get_hospital_region()
    ohio_zip_df <- ohio_zip_df %>%
        dplyr::left_join(hospital_county_df, by = c("county_name" = "County"))

    df_sf <- df_sf %>%
        dplyr::left_join(ohio_zip_df, by = c("COUNTYFP" = "county_fsip"))

    county_names <- sort(unique(df_sf$county_name))
    colors_list <- hcl.colors(length(county_names), "Dark 3")

    # pal <- leaflet::colorFactor("viridis", domain = df_sf$county_name)
    pal <- leaflet::colorFactor(colors_list, domain = df_sf$county_name)

    map <- leaflet::leaflet(df_sf) %>%
        leaflet::addTiles(
            # urlTemplate = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png"
            # urlTemplate = "mytiles/{z}_{x}_{y}.png",
            # options = tileOptions(minZoom = 5, maxZoom = 8)
        ) %>%
        leaflet::addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lat = 40.367474, lng = -82.996216, zoom = 7) %>%
        leaflet::addPolygons(
            stroke = TRUE,
            color = "black",
            weight = 1,
            fillColor = ~ pal(county_name),
            smoothFactor = 1,
            label = ~ paste0(
                "<b>", county_name, "</b>",
                "<br>Region: ", `Hospital Preparedness Region`,
                "<br>GEOID: ", GEOID
            ) %>% lapply(htmltools::HTML),
            labelOptions = labelOptions(
                style = list(
                    "font-weight" = "normal",
                    padding = "3px 8px"
                ),
                textsize = "15px",
                direction = "auto"
            )
        )

    return(map)
}




#' Shape a sf object for mapping the SatScan results
#' @param satscan_cluster_df dataframe
#' The cluster dataframe obtained by calling
#' shape_satscan_df() function
#' @param ohio_map_sf sf object
#' The Ohio Map sf data (county level) with polygons in county level
shape_map_satscan_county <- function(satscan_cluster_df, ohio_map_sf) {
    # date <- as.Date(date)
    cluster_county <- unique(satscan_cluster_df$local_id)

    ohio_map_sf <- ohio_map_sf %>%
        dplyr::filter(County %in% cluster_county)

    scan_map_sf <- dplyr::full_join(
        ohio_map_sf,
        satscan_cluster_df,
        by = c("County" = "local_id")
    )

    return(scan_map_sf)
}






#' Shape a sf object for mapping the SatScan results: ZIP code Level
#' @param satscan_cluster_df dataframe
#' The cluster dataframe obtained by calling
#' shape_satscan_df() function
#' @param ohio_map_zip_sf sf object
#' The Ohio Map sf data (zip code level) with polygons in zip code level
shape_map_satscan_zip <- function(satscan_cluster_df, ohio_map_zip_sf) {
    # date <- as.Date(date)
    cluster_zip <- unique(satscan_cluster_df$local_id)

    ohio_map_zip_sf <- ohio_map_zip_sf %>%
        dplyr::mutate_at("ZCTA5CE10", as.character) %>%
        dplyr::filter(ZCTA5CE10 %in% cluster_zip)

    scan_map_sf <- dplyr::full_join(
        ohio_map_zip_sf,
        satscan_cluster_df,
        by = c("ZCTA5CE10" = "local_id")
    )

    return(scan_map_sf)
}


#' Calculate reporduction number for county levels
#' This method is based on https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3816335/
#' @param data dataframe
#' The ODRS data
#' @param mean_covid_si float, default = 6.48
#' The prior mean value for R_t
#' based on https://www.imperial.ac.uk/media/imperial-college/medicine/sph/ide/gida-fellowships/Imperial-College-COVID19-Europe-estimates-and-NPI-impact-30-03-2020.pdf
#' @param sd_covid_si float, default = 3.83
#' the prior standard deviation value for R_t
#' based on https://www.imperial.ac.uk/media/imperial-college/medicine/sph/ide/gida-fellowships/Imperial-College-COVID19-Europe-estimates-and-NPI-impact-30-03-2020.pdf
#' @param window integer, default = 14
#' The length of the sliding time window over which R_t is estimated
#' @param threshold_total_counts integer, default = 50
#' Minimum cases for calculating R_t
#' If the total cases are less than the minimum threshold,
#' R_t value would be NA.
#' @param max_seq_number integer, default = 25
#' Do not change this value unless you understand
#' what you want to do.
calculate_reproduction_num <- function(data,
                                       mean_covid_si = 6.48,
                                       sd_covid_si = 3.83,
                                       window = 7,
                                       threshold_total_counts = 50,
                                       max_seq_number = 25) {
    # check if valid columns
    col_names <- colnames(data)
    require_cols <- c("County", "Case Count", "Onset Date")
    if (!all(require_cols %in% col_names)) {
        incorrect_col_names <- require_cols[!require_cols %in% col_names]
        incorrect_col_names <- paste(incorrect_col_names, collapse = ", ")
        txt <- paste0("Missing columns in the 'data': ", incorrect_col_names)
        stop(txt)
    }

    all_county <- sort(unique(data$County))

    LAST_UPDATE_DATE <- attr(data, "last_update_date")

    # --- Calculate R_t for each county ----
    # Distribution
    covid_si_distr <- EpiEstim::discr_si(
        seq(0, max_seq_number),
        mean_covid_si,
        sd_covid_si
    )
    # --- Calculation ----
    rep_number_list <- lapply(all_county, function(x) {
        county_df <- data %>%
            dplyr::filter(County == x)

        suppressMessages(
            agg_county_df <- agg_daily_counts_by_cols(
                data = county_df,
                group_col = "County",
                date_col = "Onset Date",
                count_col = "Case Count",
                last_update_date = LAST_UPDATE_DATE,
                evenly_space = TRUE
            )
        )

        total_cases <- sum(agg_county_df$`Case Count`)

        if (total_cases < threshold_total_counts) {
            rep_num_df <- agg_county_df %>%
                dplyr::distinct_at("County") %>%
                dplyr::mutate(
                    R_t = NA_real_,
                    Std_R_t = NA_real_,
                    Lower_CI = NA_real_,
                    Upper_CI = NA_real_
                )
        } else {

            # Calculate reporduction number
            t_start <- seq(2, nrow(agg_county_df) - window)

            suppressWarnings(
                res <- EpiEstim::estimate_R(
                    incid = agg_county_df$`Case Count`,
                    method = c("non_parametric_si"),
                    config = EpiEstim::make_config(list(
                        si_distr = covid_si_distr,
                        t_start = t_start,
                        t_end = t_start + window
                    ))
                )
            )


            r_value <- tail(res$R$`Mean(R)`, 1)

            std_r_value <- tail(res$R$`Std(R)`, 1)

            q_25 <- tail(res$R$`Quantile.0.025(R)`, 1)

            q_975 <- tail(res$R$`Quantile.0.975(R)`, 1)

            rep_num_df <- agg_county_df %>%
                dplyr::distinct_at("County") %>%
                dplyr::mutate(
                    R_t = r_value,
                    Std_R_t = std_r_value,
                    Lower_CI = q_25,
                    Upper_CI = q_975
                )
        }

        return(rep_num_df)
    })

    all_rep_num_df <- dplyr::bind_rows(rep_number_list)

    return(all_rep_num_df)
}



#' Get Summary of EWMA for county levels
#' Return a dataframe containing
#' the anomaly (TRUE or FALSE) sequence
#' obtained from EWMA for all counties
#' @param data dataframe
#' The ODRS data
#' @param baseline
calc_ewma_summary_county <- function(
                                     data,
                                     pop_df,
                                     baseline = 500,
                                     lambda = 0.1,
                                     nsigmas = 3) {

    # check if valid columns
    col_names <- colnames(data)
    require_cols <- c("County", "Case Count", "Onset Date")
    if (!all(require_cols %in% col_names)) {
        incorrect_col_names <- require_cols[!require_cols %in% col_names]
        incorrect_col_names <- paste(incorrect_col_names, collapse = ", ")
        txt <- paste0("Missing columns in the 'data': ", incorrect_col_names)
        stop(txt)
    }

    all_county <- sort(unique(data$County))
    LAST_UPDATE_DATE <- attr(data, "last_update_date")

    # Get the population proportion dataframe
    total_pop <- sum(pop_df$Count)
    pop_df <- pop_df %>%
        dplyr::mutate(ratio = Count / total_pop)

    # EWMA Summary
    ewma_summary_list <- lapply(all_county, function(county_x) {
        county_df <- data %>%
            dplyr::filter(County == county_x)

        prop_population_region <- pop_df %>%
            dplyr::filter(County == county_x) %>%
            dplyr::pull(ratio)

        ### Specify baseline mean and standard deviation in County level
        scaled_baseline_value <- round(baseline * prop_population_region)

        scaled_baseline_std <- sqrt(scaled_baseline_value)

        ### Extract observed case counts and corresponding date
        suppressMessages(
            agg_county_df <- agg_daily_counts_by_cols(
                data = county_df,
                group_col = "County",
                date_col = "Onset Date",
                count_col = "Case Count",
                last_update_date = LAST_UPDATE_DATE,
                evenly_space = TRUE
            )
        )

        ### Calculate EWMA data
        ewma_data <- ewma_chart(
            obs_data = agg_county_df$`Case Count`,
            obs_time = agg_county_df$`Onset Date`,
            normalize = FALSE,
            # norm_mean = NULL,
            # norm_std = NULL,
            center = scaled_baseline_value,
            std_dev = scaled_baseline_std,
            lambda = lambda,
            nsigmas = nsigmas
        )

        ewma_data <- ewma_data %>%
            dplyr::select(x, anomaly) %>%
            dplyr::mutate(County = county_x) %>%
            dplyr::rename(date = x)

        return(ewma_data)
    })

    ewma_summary_df <- dplyr::bind_rows(ewma_summary_list)

    attr(ewma_summary_df, "last_update_date") <- as.character(max(ewma_summary_df$date))

    attr(ewma_summary_df, "lambda") <- lambda
    attr(ewma_summary_df, "nsigmas") <- nsigmas
    attr(ewma_summary_df, "center") <- baseline


    return(ewma_summary_df)
}



# ' Plotly for Summary of EWMA for every county
plotly_summary_ewma <- function(ewma_summary_df) {

    # lamb <- attr(ewma_summary_df, "lambda")
    # nsigmas <- attr(ewma_summary_df, "nsigmas")

    baseline <- attr(ewma_summary_df, "center")
    last_update_date <- attr(ewma_summary_df, "last_update_date")

    caption_line <- paste("Last Updated", format(as.Date(last_update_date), "%B %d, %Y"), sep = ": ")
    title <- paste("Temporal Surveillance Overview (Expect", baseline, "Daily in Ohio)")

    all_county <- sort(unique(ewma_summary_df$County))

    plot_height <- 500 + 30 * length(all_county)

    # Order county by the number of anomalies
    county_order <- ewma_summary_df %>%
        dplyr::group_by(County) %>%
        dplyr::summarise_at("anomaly", list(sum)) %>%
        dplyr::arrange(desc(anomaly)) %>%
        dplyr::rename(num_anomaly = anomaly)

    # Make evenly-space
    ewma_summary_df <- ewma_summary_df %>%
        dplyr::mutate(
            anomaly = case_when(
                anomaly ~ 1,
                TRUE ~ 0
            )
        ) %>%
        tidyr::complete(
            date = seq.Date(min(date), max(date), by = "day"),
            County
            # fill = list(anomaly = 0)
        )

    # Create a sequence: each row represent one county
    ewma_summary_df <- ewma_summary_df %>%
        tidyr::spread(date, anomaly)

    ewma_summary_df <- ewma_summary_df %>%
        dplyr::left_join(county_order, by = "County")

    ewma_summary_df <- ewma_summary_df %>%
        dplyr::arrange(desc(num_anomaly))

    ewma_summary_df <- as.data.frame(ewma_summary_df)
    rownames(ewma_summary_df) <- ewma_summary_df$County
    ewma_summary_df <- ewma_summary_df %>%
        select(-County, -num_anomaly)

    col_names <- colnames(ewma_summary_df)
    colnames(ewma_summary_df) <- format(as.Date(col_names), "%b %d")

    ewma_summary_matrix <- as.matrix(ewma_summary_df)

    p <- heatmaply(
        ewma_summary_matrix,
        dendrogram = "none",
        grid_color = "white",
        grid_size = 0.1,
        # grid_gap = 0,
        colors = c(
            colors_custom$light_blue, colors_custom$light_blue, colors_custom$light_blue,
            colors_custom$dark_red_1, colors_custom$dark_red_1, colors_custom$dark_red_1
        ),
        label_names = c("County", "Date", "Anomaly"),
        hide_colorbar = TRUE,
        na.value = "grey50"
        # plot_method = "plotly"
    )

    p <- p %>%
        plotly::layout(
            height = plot_height,
            title = list(
                text = paste0(
                    title,
                    "<br>",
                    "<sup>",
                    caption_line,
                    "</sup>"
                ),
                xref = "paper",
                yref = "paper",
                x = 0
            ),
            font = list(
                size = 12
            ),
            xaxis = list(
                title = "",
                spikemode = "toaxis",
                spikesnap = "cursor",
                visible = FALSE,
                showline = TRUE,
                zeroline = FALSE,
                showgrid = TRUE,
                gridcolor = colors_custom$white
            ),
            yaxis = list(
                title = "",
                spikemode = "toaxis",
                spikesnap = "cursor",
                ticklen = 5,
                tickwidth = 2,
                tickcolor = colors_custom$black,
                showline = TRUE,
                zeroline = FALSE,
                showgrid = TRUE,
                gridcolor = colors_custom$white
            ),
            plot_bgcolor = colors_custom$blue_gray_2,
            paper_bgcolor = colors_custom$blue_gray_1,
            margin = list(l = 20, r = 20, t = 50, b = 20),
            # legend = list(orientation = "h", traceorder = "normal", y = -0.2),
            font = list(color = "black")
        ) %>%
        plotly::config(
            # remove plotly logo
            displaylogo = FALSE
        )

    return(p)
}





#' Get backfill cases for all counties
#' @param new_ODRS dataframe
#' The latest ODRS dataframe
#' @param his_ODRS dataframe
#' The previous ODRS dataframe
calc_backfill_county <- function(new_ODRS, his_ODRS) {
    # check if valid columns
    col_names <- colnames(new_ODRS)
    require_cols <- c("County", "Case Count", "Onset Date")
    if (!all(require_cols %in% col_names)) {
        incorrect_col_names <- require_cols[!require_cols %in% col_names]
        incorrect_col_names <- paste(incorrect_col_names, collapse = ", ")
        txt <- paste0("Missing columns in the 'data': ", incorrect_col_names)
        stop(txt)
    }

    # Get update dates
    last_update_date <- attr(new_ODRS, "last_update_date")


    his_last_date <- attr(his_ODRS, "last_update_date")


    # Get all counties
    all_county <- sort(unique(new_ODRS$County))

    # Loop every county and get the backfill dataframe into a list
    backfill_summary_list <- lapply(all_county, function(county_x) {

        # Filter ODRS by county
        county_df_new <- new_ODRS %>%
            dplyr::filter(County == county_x)

        county_df_his <- his_ODRS %>%
            dplyr::filter(County == county_x)

        # Aggregate
        suppressMessages(
            agg_county_df_new <- agg_daily_counts_by_cols(
                data = county_df_new,
                group_col = "County",
                date_col = "Onset Date",
                count_col = "Case Count",
                last_update_date = last_update_date,
                evenly_space = TRUE
            )
        )

        suppressMessages(
            agg_county_df_his <- agg_daily_counts_by_cols(
                data = county_df_his,
                group_col = "County",
                date_col = "Onset Date",
                count_col = "Case Count",
                last_update_date = his_last_date,
                evenly_space = TRUE
            )
        )

        # Rename
        agg_county_df_new <- agg_county_df_new %>%
            dplyr::rename(obs_case = `Case Count`)

        agg_county_df_his <- agg_county_df_his %>%
            dplyr::rename(his_case = `Case Count`) %>%
            dplyr::select(-County)

        # Merge
        agg_county_df_merge <- agg_county_df_new %>%
            dplyr::left_join(agg_county_df_his, by = "Onset Date") %>%
            dplyr::mutate_if(is.numeric, coalesce, 0)

        agg_county_df_merge <- agg_county_df_merge %>%
            dplyr::mutate(reported_cases_change = obs_case - his_case)

        return(agg_county_df_merge)
    })



    backfill_summary_df <- dplyr::bind_rows(backfill_summary_list)

    attr(backfill_summary_df, "last_update_date") <- as.character(last_update_date)
    attr(backfill_summary_df, "his_update_date") <- as.character(his_last_date)

    return(backfill_summary_df)
}




#' Backfill Heatmap colors
backfill_heatmap_colors <- function(unique_cases_change) {
    unique_cases_change <- sort(unique_cases_change)
    colors_postion <- scales::rescale(unique_cases_change, to = c(0, 1))

    colors_backfill <- lapply(unique_cases_change, function(x) {
        if (x <= -10) {
            return(colors_custom$dark_orange)
        }

        if (x > -10 & x < 0) {
            return(colors_custom$orange)
        }

        if (x == 0) {
            return(colors_custom$blue_gray_3)
        }

        if (x > 0 & x <= 5) {
            return(colors_custom$pink_gray)
        }

        if (x > 5 & x < 10) {
            return(colors_custom$pink)
        }

        if (x >= 10 & x < 15) {
            return(colors_custom$light_red)
        }

        if (x >= 15) {
            return(colors_custom$dark_red_1)
        }
    })

    colors_backfill <- unlist(colors_backfill)

    if (length(colors_backfill) == 1) {
        colors_backfill <- rep(colors_backfill, 2)
        colors_postion <- c(0, colors_postion)
    }

    return(list(colors_postion = colors_postion, colors_backfill = colors_backfill))
}



# ' Plotly for backfill cases for every county
plotly_summary_backfill <- function(backfill_summary_df, show_days = "all") {
    last_update_date <- attr(backfill_summary_df, "last_update_date")
    his_last_date <- attr(backfill_summary_df, "his_update_date")
    backfill_summary_df <- backfill_summary_df %>%
        dplyr::select(-obs_case, -his_case)

    # filter by days
    if (class(show_days) == "character") {
        if (show_days != "all") {
            stop("'show_days' must be either 'all' or interger value")
        }
    } else {
        show_days <- as.integer(show_days)
        if (is.na(show_days)) {
            stop("'show_days' must be either 'all' or integer value")
        }
        backfill_summary_df <- backfill_summary_df %>%
            dplyr::group_by(County) %>%
            dplyr::top_n(n = show_days, wt = `Onset Date`) %>%
            dplyr::ungroup()
    }

    caption_line <- paste("Last Updated", format(as.Date(last_update_date), "%B %d, %Y"), sep = ": ")
    title <- paste("Backfill Overview")

    unique_cases_change <- unique(backfill_summary_df$reported_cases_change)

    all_county <- sort(unique(backfill_summary_df$County))

    plot_height <- 500 + 30 * length(all_county)

    # Order county by the number of anomalies
    county_order <- backfill_summary_df %>%
        dplyr::group_by(County) %>%
        dplyr::summarise_at("reported_cases_change", list(sum)) %>%
        dplyr::arrange(desc(reported_cases_change)) %>%
        dplyr::rename(total_change = reported_cases_change)

    # Make evenly-space
    backfill_summary_df <- backfill_summary_df %>%
        tidyr::complete(
            `Onset Date` = seq.Date(min(`Onset Date`), max(`Onset Date`), by = "day"),
            County
            # fill = list(anomaly = 0)
        )

    # Create a sequence: each row represent one county
    backfill_summary_df <- backfill_summary_df %>%
        tidyr::spread(`Onset Date`, reported_cases_change)

    backfill_summary_df <- backfill_summary_df %>%
        dplyr::left_join(county_order, by = "County")

    backfill_summary_df <- backfill_summary_df %>%
        dplyr::arrange(desc(total_change))

    backfill_summary_df <- as.data.frame(backfill_summary_df)
    rownames(backfill_summary_df) <- backfill_summary_df$County
    backfill_summary_df <- backfill_summary_df %>%
        select(-County, -total_change)

    col_names <- colnames(backfill_summary_df)
    colnames(backfill_summary_df) <- format(as.Date(col_names), "%b %d")

    backfill_summary_matrix <- as.matrix(backfill_summary_df)

    # Heatmap
    last_update_date <- as.Date(last_update_date)
    his_last_date <- as.Date(his_last_date)

    diff_days <- difftime(last_update_date, his_last_date, units = c("days"))
    diff_days <- as.numeric(diff_days)

    if (diff_days == 1) {
        case_change_name <- "Last 24 Hours Reported Cases Change"
    } else {
        case_change_name <- paste("Last", diff_days, "Days Reported Cases Change")
    }


    colors_list <- backfill_heatmap_colors(unique_cases_change)
    colors_postion <- colors_list$colors_postion
    colors_backfill <- colors_list$colors_backfill

    p <- heatmaply(
        backfill_summary_matrix,
        dendrogram = "none",
        grid_color = "white",
        grid_size = 0.1,
        # grid_gap = 0,
        scale_fill_gradient_fun = ggplot2::scale_fill_gradientn(
            colors = colors_backfill,
            # breaks = c(-10, -1, 0, 1, 10),
            values = colors_postion,
            # limits = c(min_change - 1, max_change),
            na.value = colors_custom$dark_red_1
        ),
        label_names = c("County", "Date", case_change_name),
        hide_colorbar = TRUE,
        na.value = "grey50"
    )

    p <- p %>%
        plotly::layout(
            height = plot_height,
            title = list(
                text = paste0(
                    title,
                    "<br>",
                    "<sup>",
                    caption_line,
                    "</sup>"
                ),
                xref = "paper",
                yref = "paper",
                x = 0
            ),
            font = list(
                size = 12
            ),
            xaxis = list(
                title = "",
                spikemode = "toaxis",
                spikesnap = "cursor",
                visible = FALSE,
                showline = TRUE,
                zeroline = FALSE,
                showgrid = TRUE,
                gridcolor = colors_custom$white
            ),
            yaxis = list(
                title = "",
                spikemode = "toaxis",
                spikesnap = "cursor",
                ticks = "outside",
                ticklen = 5,
                tickwidth = 2,
                tickcolor = colors_custom$black,
                showline = TRUE,
                zeroline = FALSE,
                showgrid = TRUE,
                gridcolor = colors_custom$white
            ),
            plot_bgcolor = colors_custom$blue_gray_2,
            paper_bgcolor = colors_custom$blue_gray_1,
            margin = list(l = 20, r = 20, t = 50, b = 20),
            # legend = list(orientation = "h", traceorder = "normal", y = -0.2),
            font = list(color = "black")
        ) %>%
        plotly::config(
            # remove plotly logo
            displaylogo = FALSE
        )

    return(p)
}



## ===================================================##
##### Patrick's Relative Risk (RR) model ####################
## ===================================================##

#' Convert ODRS data as the formatting used in RR model
#' @param data
#' The ODRS data
#' @param pop_df
#' The population obtained from get_ohio_pop()
#' which loads the population data: Ohio_population.csv
#' It contains two columns: County, Count
convert2RR_agg_ODRS <- function(data, pop_df, cutoff_date = "2020-03-01") {

    # Get aggregated cases by date and county
    df_agg <- agg_daily_counts_by_cols(
        data,
        group_col = "County",
        date_col = "Onset Date",
        count_col = "Case Count",
        last_update_date = "auto",
        evenly_space = TRUE
    )

    df_agg <- df_agg %>%
        dplyr::filter(`Onset Date` >= as.Date(cutoff_date))

    # Add State Column
    df_agg <- df_agg %>%
        dplyr::mutate(State = "OH")

    # Merge Region numbers
    df_agg <- df_agg %>%
        dplyr::left_join(
            get_hospital_region(),
            by = "County"
        )

    # Merge Zone numbers
    region_zone_df <- get_region_zone_county_df() %>%
        dplyr::select(`Hospital Preparedness Region`, Zone)

    df_agg <- df_agg %>%
        dplyr::left_join(
            region_zone_df,
            by = "Hospital Preparedness Region"
        )

    # Merge population
    df_agg <- df_agg %>%
        dplyr::left_join(
            pop_df,
            by = "County"
        )

    # Renanme columns
    df_agg <- df_agg %>%
        dplyr::rename(
            Date = `Onset Date`,
            Daily_new = `Case Count`,
            Region = `Hospital Preparedness Region`,
            Population = Count
        )

    return(df_agg)
}


#' Construct dataset by specify counties or regions of interest
##===================================================##

############### insert Patrick's model ################

##===================================================##
construct.focus.dataset <- function(
    data,
    aggregation.variable = "State",
    aggregation.level = "OH",
    focus.variable = "State",
    focus.levels = c("OH"),
    leave.focus.out = TRUE) {
    
    # check for existence of variables in dataset
    if (!(aggregation.variable %in% colnames(data))) {
        stop("Aggregation variable `", aggregation.variable, "`` not in data.")
    }
    if (!(focus.variable %in% colnames(data))) {
        stop("Focus variable `", focus.variable, "` not in data.")
    }
    if (!("Daily_new" %in% colnames(data))) {
        stop("Outcome variable `Daily_new`` not in data.")
    }
    if (!("Population" %in% colnames(data))) {
        stop("Normalization variable `Population`` not in data.")
    }
    
    data.universe <- data %>%
        filter(get(aggregation.variable) == aggregation.level)
    
    if (nrow(data.universe) == 0) {
        stop("No rows matching `",
             aggregation.variable, " == '", aggregation.level,
             "'` in data.")
    }
    
    ### standardization ###
    
    if (leave.focus.out) {
        data.comparator <- data.universe %>%
            filter(!(get(focus.variable) %in% focus.levels))
    } else {
        data.comparator <- data.universe
    }
    
    data.agg.Daily_new <- data.comparator %>%
        dplyr::group_by(Date) %>%
        dplyr::summarize(agg_Daily_new = sum(Daily_new), .groups = "drop") %>%
        ungroup()
    
    
    agg.Population <- data.comparator %>%
        distinct(County, .keep_all = TRUE) %>%
        pull(Population) %>% sum(na.rm = TRUE)
    
    data.focus <- data.universe %>%
        filter(get(focus.variable) %in% focus.levels) %>%
        dplyr::group_by(Date) %>%
        dplyr::summarize(focus_Population = sum(Population, na.rm = TRUE),
                         focus_Daily_new = sum(Daily_new),
                         .groups = "drop") %>%
        ungroup() %>%
        left_join(data.agg.Daily_new, by = "Date") %>%
        mutate(agg_Population = agg.Population,
               Expected = focus_Population * (agg_Daily_new / agg_Population),
               Crude_rr = focus_Daily_new / Expected,
               Rel_Date = as.numeric(Date - min(Date)))
    
    if (nrow(data.focus) == 0) {
        stop("No rows matching `",
             focus.variable, " %in% c('",
             paste(focus.levels, collapse = "', '"),
             "')` in data.")
    }
    
    data.focus
}

test.rr.positive <- function(
    data,
    aggregation.variable = "State",
    aggregation.level = "OH",
    focus.variable = "County",
    focus.levels = c("Franklin"),
    leave.focus.out = TRUE
) {
    
    data.focus <- construct.focus.dataset(
        data = data,
        aggregation.variable = aggregation.variable,
        aggregation.level = aggregation.level,
        focus.variable = focus.variable,
        focus.levels = focus.levels,
        leave.focus.out = leave.focus.out
    )
    
    fit <- tryCatch({
        gam(focus_Daily_new ~ 1 + offset(log(Expected)) + s(Rel_Date),
            family = nb(),
            data = data.focus %>% mutate(
                Expected = if_else(Expected == 0, 1 / 100, Expected)))
    }, error = function(cond) {
        gam(Daily_new ~ 1 + offset(log(Expected)) + Rel.Date,
            family = nb(),
            data = data.focus %>% mutate(
                Expected = if_else(Expected == 0, 1 / 100, Expected)))
    })
    
    newdata <- data.focus
    newdata$Expected[newdata$Expected == 0] <- 1 / 100
    
    pred <- predict(fit, newdata = newdata, type = "link", se.fit = TRUE)
    
    pred$fit <- pred$fit - log(newdata$Expected)
    
    last <- length(pred$fit)
    ci <- exp(pred$fit[last] + qnorm(c(0.025, 0.975)) * pred$se.fit[last])
    prob.rr.gt.1 <- pnorm(pred$fit[last] / pred$se.fit[last])
    
    rel.risk <- c(
        "Estimate" = unname(log2(exp(pred$fit[last]))),
        "2.5%" = log2(ci[1]),
        "97.5%" = log2(ci[2]),
        "Post prob pos" = unname(prob.rr.gt.1)
    )
    
    
    Xp <- predict(fit, newdata = newdata, type = "lpmatrix")
    a <- c(rep(0, nrow(Xp) - 2), c(-1, 1))
    Xs <- t(a) %*% Xp
    fdiff <- drop(Xs %*% coef(fit))
    var.fdiff <- drop(Xs %*% fit$Vp %*% t(Xs))
    se.fdiff <- sqrt(var.fdiff)
    ci <- exp(fdiff + qnorm(c(0.025, 0.975)) * se.fdiff)
    prob.fdiff.gt.1 <- pnorm(fdiff / se.fdiff)
    
    rr.trend <- c(
        "Estimate" = unname(log2(exp(fdiff))),
        "2.5%" = log2(ci[1]),
        "97.5%" = log2(ci[2]),
        "Post prob pos" = unname(prob.fdiff.gt.1)
    )
    
    rbind(
        "Log2 rel risk" = rel.risk,
        "Deriv log2 rel risk" = rr.trend
    )
}


ggplot.local.trend <-
    function(data,
             aggregation.variable = "State",
             aggregation.level = "OH",
             focus.variable = "State",
             focus.levels = c("OH"),
             log.scale = TRUE,
             rel.risk = TRUE,
             leave.focus.out = TRUE,
             title = NULL,
             confint = TRUE) {
        
        data.focus <- construct.focus.dataset(
            data = data,
            aggregation.variable = aggregation.variable,
            aggregation.level = aggregation.level,
            focus.variable = focus.variable,
            focus.levels = focus.levels,
            leave.focus.out = leave.focus.out
        )
        
        ### model fit ###
        
        if (rel.risk) {
            fit <- tryCatch({
                gam(focus_Daily_new ~ 1 + offset(log(Expected)) + s(Rel_Date),
                    family = nb(),
                    data = data.focus %>% mutate(
                        Expected = if_else(Expected == 0, 1 / 100, Expected)))
            }, error = function(cond) {
                gam(Daily_new ~ 1 + offset(log(Expected)) + Rel.Date,
                    family = nb(),
                    data = data.focus %>% mutate(
                        Expected = if_else(Expected == 0, 1 / 100, Expected)))
            })
            
            spred <- predict(fit,
                             newdata = data.focus %>% mutate(
                                 Expected = if_else(Expected == 0, 1 / 100, Expected)
                             ),
                             type = "response")
            
            p <- predict(fit,
                         newdata = data.focus %>% mutate(
                             Expected = if_else(Expected == 0, 1 / 100, Expected)
                         ),
                         type = "link",
                         se.fit = TRUE)
            upper <- fit$family$linkinv(p$fit + qnorm(0.975) * p$se.fit)
            lower <- fit$family$linkinv(p$fit + qnorm(0.025) * p$se.fit)
        } else { # raw counts
            fit <- tryCatch({
                gam(focus_Daily_new ~ 1 + s(Rel_Date),
                    family = nb(),
                    data = data.focus)
            }, error = function(cond) {
                gam(Daily_new ~ 1 + Rel.Date,
                    family = nb(),
                    data = data.focus)
            })
            
            spred <- predict(fit, type = "response")
            
            p <- predict(fit, type = "link", se.fit = TRUE)
            upper <- fit$family$linkinv(p$fit + qnorm(0.975) * p$se.fit)
            lower <- fit$family$linkinv(p$fit + qnorm(0.025) * p$se.fit)
        }
        
        ### plotting ###
        
        if (is.null(title)) {
            plot.title <- paste0(
                "",
                focus.variable,
                " = ",
                paste(focus.levels, collapse = ", "),
                ifelse(rel.risk,
                       paste0(
                           "\nversus ",
                           ifelse(leave.focus.out, "everything else in ", ""),
                           aggregation.variable,
                           " = ",
                           aggregation.level
                       ),
                       ""
                )
            )
        } else {
            plot.title = title
        }
        
        
        # plotting options
        if (rel.risk) {
            
            plot.data <- data.focus %>% select(Date, focus_Daily_new, Expected, Crude_rr) %>%
                mutate(
                    expected.filled = if_else(Expected == 0, 1 / 100, Expected)
                )
            
            plot.data$predicted <- spred
            plot.data$upper <- upper
            plot.data$lower <- lower
            
            transform <- ifelse(log.scale, log2, identity)
            
            p <- ggplot(plot.data, aes(x = Date, y = transform(Crude_rr))) +
                geom_point() +
                geom_line(aes(y = transform(predicted / expected.filled)), color = "dodgerblue4", size = 1.2) +
                xlab("Date") +
                ylab(ifelse(log.scale, "Log2 relative risk", "Relative risk")) +
                ggtitle(plot.title) +
                theme_minimal() + 
                geom_hline(yintercept = transform(1), linetype = "dashed")
            
            if (confint) {
                p <- p + geom_ribbon(aes(ymax = transform(upper / expected.filled),
                                         ymin = transform(lower / expected.filled)),
                                     alpha = 0.3,
                                     fill = "dodgerblue3")
            }
            
            p
            
        } else { # plotting raw counts
            
            plot.data <- data.focus %>% select(Date, focus_Daily_new, Expected, Crude_rr) %>%
                mutate(
                    expected.filled = if_else(Expected == 0, 1 / 100, Expected)
                )
            
            plot.data$predicted <- spred
            plot.data$upper <- upper
            plot.data$lower <- lower
            
            p <- ggplot(plot.data, aes(x = Date, y = focus_Daily_new)) +
                geom_point() +
                geom_line(aes(y = predicted),color = "dodgerblue4", size = 1.2) +
                scale_y_continuous(trans = if_else(log.scale, expr(log10()), expr(identity()))) +
                xlab("Date") +
                ylab(ifelse(log.scale, "Daily new count (log scale)", "Daily new count")) +
                theme_minimal() + 
                ggtitle(plot.title)
            
            if (confint) {
                p <- p + geom_ribbon(aes(ymax = upper,
                                         ymin = lower),
                                     alpha = 0.3,
                                     fill = "dodgerblue3")
            }
            
            p
        }
    }

screen <- function(
    data,
    aggregation.variable = "State",
    aggregation.level = "OH",
    focus.variable = "County",
    leave.focus.out = TRUE
) {
    focus.levels <- data %>%
        filter(get(aggregation.variable) == aggregation.level) %>%
        pull(get(focus.variable)) %>%
        unique()
    
    summary <- array(NA, dim = c(length(focus.levels), 2, 4))
    dimnames(summary) <- list(focus.variable = focus.levels,
                              "Summary" = c("Log2 rel risk",
                                            "Deriv log2 rel risk"),
                              "Quantity" = c("Estimate",
                                             "2.5%", "97.5%",
                                             "Post prob pos"))
    
    
    
    
    for (focus.level in focus.levels) {
        test <- test.rr.positive(data,
                                 aggregation.variable = aggregation.variable,
                                 aggregation.level = aggregation.level,
                                 focus.variable = focus.variable,
                                 focus.levels = focus.level,
                                 leave.focus.out = TRUE)
        
        summary[focus.level, , ] <- test
    }
    
    summary
}




# label format
mylabelFormat <- function(prefix = "",
                          suffix = "",
                          between = " &ndash; ",
                          digits = 3,
                          big.mark = ",",
                          transform = identity) {
    formatNum <- function(x) {
        format(
            round(transform(x), digits),
            trim = TRUE, scientific = FALSE,
            big.mark = big.mark
        )
    }

    function(type, ...) {
        switch(
            type,
            numeric = (function(cuts) {
                paste0(prefix, formatNum(cuts), suffix)
            })(...), # nolint
            bin = (function(cuts) {
                n <- length(cuts)
                prefix
            })(...), # nolint
            quantile = (function(cuts, p) {
                n <- length(cuts)
                p <- paste0(round(p * 100), "%")
                cuts <- paste0(formatNum(cuts[-n]), between, formatNum(cuts[-1]))
                # mouse over the legend labels to see the values (quantiles)
                paste0(
                    "<span title=\"", cuts, "\">", prefix, p[-n], between, p[-1], suffix,
                    "</span>"
                )
            })(...), # nolint
            factor = (function(cuts) {
                paste0(prefix, as.character(transform(cuts)), suffix)
            })(...) # nolint
        )
    }
}


#' Get Quantile Labels
get_quantile_labels <- function(x,
                                n = 6,
                                labels = c("Very High", "High", "Slightly High", "Slightly Low", "Low", "Very Low")) {
    qtile_values <- quantile(x, probs = seq(0, 1, 1 / n))

    cut_values <- cut(x, qtile_values, labels = labels, include.lowest = TRUE, right = FALSE)

    return(cut_values)
}