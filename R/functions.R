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