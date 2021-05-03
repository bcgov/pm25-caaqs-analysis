# Copyright 2021 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

get_pm25_data <- function(keep_csv = FALSE, ...) {
  pm25_link <- "ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/AnnualSummary/2009-LatestVerified/PM25.csv"
  filepath <- pm25_link
  
  if (keep_csv) {
    filepath <- file.path("data/pm25", basename(pm25_link))
    if (!file.exists(filepath)) {
      dir.create(dirname(filepath), showWarnings = FALSE )
      download.file(pm25_link, destfile = filepath, quiet = TRUE, method = "curl")
    }
  }
  
  
  readr::read_csv(filepath, 
           col_types = readr::cols_only(DATE_PST = readr::col_datetime(), 
                                 EMS_ID = readr::col_character(), 
                                 STATION_NAME = readr::col_character(),
                                 PARAMETER = readr::col_character(),
                                 INSTRUMENT = readr::col_character(),
                                 UNIT = readr::col_character(),
                                 RAW_VALUE = readr::col_double()), ...)
  
  # arrow::open_dataset(dirname(stored_path), format = "csv", schema = arrow::schema(
  #   DATE_PST = arrow::timestamp(),
  #   STATION_NAME = arrow::string(),
  #   EMS_ID = arrow::string(),
  #   PARAMETER = arrow::string(),
  #   INSTRUMENT = arrow::string(),
  #   RAW_VALUE = arrow::float64(),
  #   UNIT = arrow::string(),
  #   ROUNDED_VALUE = arrow::float64()
  # ), ...)
}

get_aq_stations <- function() {
  aq_stations_link <- "ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/Air_Monitoring_Stations/bc_air_monitoring_stations.csv"
  
  stored_path <- file.path("data", basename(aq_stations_link))
  
  if (!file.exists(stored_path)) {
    dir.create(dirname(stored_path), showWarnings = FALSE )
    download.file(aq_stations_link, destfile = stored_path, quiet = TRUE, method = "curl")
  }
  
  readr::read_csv(stored_path, na = c("", "N/A"))
}

get_station_names <- function(file) {
  read_csv(file) %>% 
    mutate(ems_id = str_pad(ems_id, 7, "left", "0")) %>% 
    rename(orig_stn_name = station_name)
}

clean_stations <- function(aq_stations, stn_names, airzones) {
  excluded_stations <- stations_exclude(aq_stations)

  # Combine two squamish stations (both FEM, one in 2015 and the other in 2016-17)
  # for a complete record
  squamish_ems_ids <- c("0310172", "E304570")
  combo_squamish_id <- paste(squamish_ems_ids, collapse = "-")
  
  ## Clean station data - lowercase column names, remove pseudo-duplicates, subset to those 
  ## stations analysed
  ## OLD == closed stns; 
  ## _60 == meteorological stns;
  ## Met == meteorological stns using Campbell loggers; 
  ## BAM == Beta Attenuation Monitoring for PM measurement.
  select_pattern <- "_60$|Met$|OLD$|BAM$|Squamish Gov't Bldg"
  
  aq_stations %>% 
    rename_all(tolower) %>% 
    mutate(ems_id = case_when(ems_id %in% squamish_ems_ids ~ combo_squamish_id, 
                              TRUE ~ gsub("_.+$", "", ems_id))) %>% 
    group_by(ems_id) %>%
    filter(n() == 1 | 
             !grepl(select_pattern, station_name) | 
             all(grepl(select_pattern, station_name))) %>% 
    filter(!is.na(latitude), !is.na(longitude)) %>% 
    mutate(station_name = gsub(select_pattern, "", station_name), 
           station_name = gsub("(Squamish).+", "\\1", station_name)) %>% 
    # semi_join(pm25_clean, by = "ems_id") %>% 
    top_n(1, station_name) %>% 
    ungroup() %>% 
    left_join(select(stn_names, -airzone), by = "ems_id") %>% 
    mutate(station_name = case_when(is.na(reporting_name) ~ station_name,
                                    TRUE ~ reporting_name)) %>% 
    assign_airzone(airzones = airzones, 
                   station_id = "ems_id", 
                   coords = c("longitude", "latitude"))
}

stations_exclude <- function(aq_stations) {
  ## Set stations to exclude from analysis (those in industrial settings):
  excluded_stations <- aq_stations$EMS_ID[grepl("industr", aq_stations$STATION_ENVIRONMENT, ignore.case = TRUE)]
  
  # remove bc_hydro stations (fort st James) as not reported 
  bchydro_stations <- unique(aq_stations$EMS_ID[grepl("INDUSTRY-BCH", aq_stations$STATION_OWNER, ignore.case = TRUE)])
  unique(c(excluded_stations, bchydro_stations))
}

pre_clean_pm25 <- function(pm25_df, max_year, excluded_stations = NULL) {
  pm25_df %>% 
    filter(!EMS_ID %in% excluded_stations) %>% 
    mutate(date_time = format_caaqs_dt(DATE_PST), 
           year = year(date_time)) %>% 
    filter(year <= max_year) %>% 
    select(-DATE_PST) %>% 
    rename_all(tolower) %>% 
    rename(value = raw_value) %>% 
    mutate(ems_id = gsub("_1$", "", ems_id), # remove _1 from ems_id (Smithers St Josephs)
           value = clean_neg(value, type = "pm25"),) %>% 
    group_by(ems_id, instrument) %>% 
    do(., date_fill(., date_col = "date_time",
                    fill_cols = c("ems_id", "instrument", "parameter"),
                    interval = "1 hour")) %>% 
    mutate(instrument_type = 
             case_when(grepl("TEOM", instrument) ~ "TEOM",
                       grepl("SHARP|BAM", instrument) ~ "FEM", 
                       is.na(instrument) ~ NA_character_,
                       TRUE ~ "Unknown"), 
           year = year(date_time)) %>% 
    ungroup() %>% 
    # Filter NAs out of Kamloops Fed building from two overlapping monitors
    filter(!(ems_id == "0605008" & 
               instrument == "BAM1020" & 
               date_time > as.POSIXct("2017/06/19 14:59:59")) & 
             !(ems_id == "0605008" & 
                 instrument == "PM25 SHARP5030" & 
                 date_time <= as.POSIXct("2017/06/19 14:59:59"))
    ) %>% 
    # Remove TEOM from Grand forks in 2017, as there was also FEM running at the 
    # same time, and combine TEOM and FEM for that station
    filter(!(ems_id == "E263701" & 
               instrument_type == "TEOM" & 
               date_time >= as.POSIXct("2016-12-31 23:59:59")
    )) %>% 
    mutate(
      instrument_type = ifelse(ems_id == "E263701", "TEOM (2017 FEM)", instrument_type)
    ) %>% 
    distinct()
}