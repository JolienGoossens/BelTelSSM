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
         deploy_date_time, recover_date_time, battery_estimated_end_date, end_date, timedif,
         deploy_latitude, deploy_longitude)



check = deploy %>% filter(battery_estimated_end_date < recover_date_time) %>% 
  mutate(timedif = difftime(recover_date_time, battery_estimated_end_date))



#### Make new data frame ####

list_deploydf = lapply(1:nrow(deploy), function(i){
  deploysub = deploy[i,]
  
  deploydf = data.frame(
    station_name = deploysub$station_name,
    date_time_day = seq.Date(from = date(deploysub$deploy_date_time), 
                             to = date(deploysub$recover_date_time), "days")
  )
  
  deploydf = deploysub %>% 
    merge(deploydf)
  
  return(deploydf)
})

deploydf = plyr::ldply(list_deploydf) %>% as_tibble()


#### Filter out days where receiver was placed or removed, except if it was put back the same day ####
deploydf = deploydf %>% group_by(station_name, deploy_latitude, deploy_longitude, date_time_day) %>% 
  summarise(n_days = n()) %>% 
  merge(deploydf)


deploydf =  deploydf %>% filter(!((date_time_day == date(deploy_date_time) | 
                                     date_time_day == date(recover_date_time)) & 
                      n_days == 1))


#### Final select & filter ####
deploydf = deploydf %>% 
  dplyr::select(station_name, deploy_latitude, deploy_longitude, date_time_day, receiver_id, 
         acoustic_project_code, mooring_type)

deploydf = deploydf %>% filter(year(date_time_day) > 2017)

deploydf_short = deploydf %>% 
  select(station_name, deploy_latitude, deploy_longitude, date_time_day)


#### Write csv file ####
write_csv(deploydf, "data/interim/reclocations.csv")

