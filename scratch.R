
source("./packages.R")

conflict_prefer("filter", "dplyr")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)


# Constants, input files and options -----------------------------

options("rcaaqs.timezone" = "Etc/GMT+8")

lapply(tar_objects(), tar_load_raw, envir = .GlobalEnv)

pm25_new <- pm25_preclean

# temp : check stations with NA's for instrument type
na_stations <- pm25_new %>%
  filter(is.na(instrument_type)) %>%
  group_by(station_name, year)%>%
  summarise(count = n())
na_stations

plot_station_instruments(pm25_new)
plot_station_instruments(pm25_new, instrument = "instrument_type")

plot_station_instruments(filter(pm25_new, grepl("[Dd]uncan", station_name)))
plot_station_instruments(filter(pm25_new, grepl("[Kk]elowna", station_name)))
plot_station_instruments(filter(pm25_new, grepl("[Qq]uesnel", station_name)))
plot_station_instruments(filter(pm25_new, grepl("[Ss]mithers", station_name)))

stn_combos <- read_csv("data/combo-stations.csv")

stn_combos <- stn_combos %>% 
  group_by(combo) %>% 
  mutate(combo_ems_id = paste(ems_id, collapse = "-"),
  combo_stn_name = paste(station_name, collapse = "/")) %>% 
  ungroup()

system.time({
  ems_id_combos <- setNames(stn_combos$combo_ems_id, stn_combos$ems_id)
  # newid <- unname(ems_id_combos[aq_stations$EMS_ID])
  
  stn_name_combos <- setNames(stn_combos$combo_stn_name, stn_combos$ems_id)
  # newname <- unname(stn_name_combos[aq_stations$EMS_ID])
  
  # aq_stations$EMS_ID[!is.na(newid)] <- newid[!is.na(newid)]
  # aq_stations$STATION_NAME[!is.na(newname)] <- newname[!is.na(newname)]
  
  new_pm_ems_ids <- ems_id_combos[pm25_new$ems_id]
  pm25_new$ems_id[!is.na(new_pm_ems_ids)] <- new_pm_ems_ids[!is.na(new_pm_ems_ids)]
})

system.time(
pm25_2 <- pm25_new %>% 
  left_join(stn_combos, c("ems_id" = "ems_id")) %>% 
  mutate(ems_id = ifelse(is.na(combo_ems_id), ems_id, combo_ems_id))
)
