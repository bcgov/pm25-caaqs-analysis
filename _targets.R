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

## Load your packages, e.g. library(targets).
source("./packages.R")

conflict_prefer("filter", "dplyr")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)


# Constants, input files and options -----------------------------

options("rcaaqs.timezone" = "Etc/GMT+8")

constants <- list(
  tar_target(max_year, 2019L), 
  tar_target(station_names_csv, "data/stn_names_reporting.csv", format = "file")
)

# Load pm25 and stations data -----------------------

data_load <- list(
  tar_target(airzones, bcmaps::airzones()),
  tar_target(pm25, get_pm25_data()),
  tar_target(aq_stations, get_aq_stations()),
  tar_target(stn_names, get_station_names(station_names_csv))
)


# Clean data ----------------------------------------

data_clean <- list(
  tar_target(aq_stations_clean, clean_stations(aq_stations, stn_names, airzones))
)


# Run -----------------------------------------------

list(
  constants,
  data_load,
  data_clean
  #...
)




##### TODO
## tar_render is from {tarchetypes}
# mon_year <- format(Sys.Date(), "%B%Y")
# outfile <- paste0("print_ver/envreportbc_pm25_", mon_year, ".pdf")
# tar_render(doc, "print_ver/pm25.Rmd", output_file = outfile)
