#### Load packages ####
library(tidyverse)
library(lubridate)

#### Read data ####
deploy = read_csv("data/raw/deploy.csv")

# Quality check
# Coordinates per station name
# Dates make sense?
# Battery dates

#### Quality filter ####
deploy = deploy %>% 
  filter(date(deploy_date_time) != date(recover_date_time)) %>% 
  filter(deploy_latitude >= 50 & deploy_latitude <= 52) %>% 
  filter(!(station_name == "bpns-Buitenratel" & date(deploy_date_time) == date("2020-09-07")))

# If battery estimated end dates are before deploy date, we assume someone forgot to fill them out correctly
deploy = deploy %>% 
  mutate(battery_estimated_end_date = ifelse(battery_estimated_end_date < deploy_date_time,
                                             as.character(deploy_date_time + months(15)),
                                             as.character(battery_estimated_end_date))) %>% 
  mutate(battery_estimated_end_date = parse_date_time(battery_estimated_end_date, orders = "ymd HMS"))

# Filter for deployments that were recovered and that resulted in a download
deploy = deploy %>% filter(!is.na(recover_date_time) & !is.na(download_file_name))

#### Add column end date ####
# Needs to be changed: database issues with battery end dates
deploy = deploy %>%
  mutate(timedif = difftime(recover_date_time, battery_estimated_end_date, units = "days")) %>%
  mutate(end_date = case_when(
    is.na(timedif) & difftime(recover_date_time, deploy_date_time, units = "weeks") < 78 ~ as.character(recover_date_time),
    is.na(timedif) & difftime(recover_date_time, deploy_date_time, units = "weeks") >= 78 ~ as.character(deploy_date_time + months(15)),
    as.numeric(timedif) < 90 ~ as.character(recover_date_time),
    as.numeric(timedif) >= 90 ~ as.character(battery_estimated_end_date)
  ))

#### Filter & select####
# Filter for time
deploy = deploy %>% 
  filter(year(recover_date_time) > 2017)

# Select columns
deploy = deploy %>% 
  select(receiver_id, acoustic_project_code, station_name, mooring_type,
         deploy_date_time, recover_date_time, battery_estimated_end_date, end_date,
         deploy_latitude, deploy_longitude)


#### Make new data frame ####
list_recloc = lapply(1:nrow(deploy), function(i){
  deploysub = deploy[i,]
  
  recloc = data.frame(
    station_name = deploysub$station_name,
    Date = seq.Date(from = date(deploysub$deploy_date_time), 
                    to = date(deploysub$end_date), "days")
  )
  
  recloc = deploysub %>% 
    merge(recloc)
  
  return(recloc)
})

recloc = plyr::ldply(list_recloc) %>% as_tibble()


#### Filter out days where receiver was placed or removed, except if it was put back the same day ####
recloc = recloc %>% 
  group_by(station_name, deploy_latitude, deploy_longitude, Date) %>% 
  summarise(n_days = n()) %>% 
  merge(recloc)


recloc =  recloc %>% filter(!((Date == date(deploy_date_time) | 
                                 Date == date(recover_date_time)) & 
                                n_days == 1))


#### Final select & filter ####
recloc = recloc %>% 
  dplyr::select(station_name, deploy_latitude, deploy_longitude, Date, receiver_id, 
                acoustic_project_code, mooring_type)

recloc = recloc %>% filter(year(Date) > 2017)

# recloc_short = recloc %>% 
#   select(station_name, deploy_latitude, deploy_longitude, Date)


#### Write csv file ####
write_csv(recloc, "data/interim/reclocations.csv")

