
# Explore data

#### Load packages ####
library(tidyverse)

#### Get data ####
df = read_csv("data/raw/df.csv")
an = read_csv("data/raw/an.csv")
tags = read_csv("data/raw/tags.csv")
deploy = read_csv("data/raw/deploy.csv")

bpns_raster = raster::raster("data/external/hackraster.grd")

#### Basic check: detection data ####
str(df)
# What we will need: 
# Tag or animal identifier: tag_serial_number / acoustic_tag_id / animal_id
df %>% distinct(tag_serial_number, animal_id, acoustic_tag_id) # 2 acoustic_tag_id ~ 2 sensors (depth + temperature)
# 3 animals in this data set: we will start working on animal_id 3510 / tag_serial_number 1292646 

# Date of detection: date_time
df %>% 
  mutate(date = lubridate::date(date_time)) %>% 
  group_by(tag_serial_number) %>% summarise(n_days = length(unique(date)))

# Location: station_name / receiver_id / deploy_latitude + deploy_longitude
df %>% 
  group_by(tag_serial_number) %>% 
  summarise(
    n_stat = length(unique(station_name)))

df %>% 
  group_by(tag_serial_number, acoustic_project_code) %>% 
  summarise(
    n_stat = length(unique(station_name)))

# Sensor information not relevant at this point
# Depth info could be of interest for looking into positions at a higher resolution

#### Basic check: animal data ####
str(an)

# What we will need: 
# Tag or animal identifier: tag_serial_number / acoustic_tag_id / animal_id
an %>% distinct(tag_serial_number, animal_id, acoustic_tag_id) # 2 acoustic_tag_id ~ 2 sensors (depth + temperature)

# Release date: release_date_time
an %>% select(tag_serial_number, release_date_time)

# Release location: release_location / release_latitude + release_longitude
an %>% select(tag_serial_number, release_location, release_latitude, release_longitude)

#### Basic check: tag data ####
str(tags)

# What we will need: 
# Tag or animal identifier: tag_serial_number / acoustic_tag_id 
tags %>% select(tag_serial_number, acoustic_tag_id, sensor_type) # 2 acoustic_tag_id ~ 2 sensors (depth + temperature)

# Battery end date: battery_estimated_life / battery_estimated_end_date
tags %>% 
  group_by(tag_serial_number) %>% 
  summarise(battery_estimated_end_date = unique(battery_estimated_end_date),
            battery_estimated_life = unique(battery_estimated_life))

# Transmitter power settings
tags %>% 
  group_by(tag_serial_number) %>% 
  summarise_at(vars(step1_power, step1_duration, 
                    step2_power, step2_duration, 
                    step3_power, step3_duration,
                    step4_power, step4_duration), 
               unique)
# In a future step, we will also include the transmitter intervals

#### Basic check: deploy data ####
str(deploy)

# What we will need: 
# Location: station_name / receiver_id / deploy_latitude + deploy_longitude
deploy %>% distinct(station_name)
deploy %>% filter(is.na(station_name))


# Time deployment was active: deploy_date_time, recover_date_time 
min(deploy$deploy_date_time)
max(deploy$recover_date_time, na.rm =T)
# We will only need the deployments active (and resulting in data)
check = deploy %>% filter(is.na(recover_date_time) & !is.na(download_file_name)) # issue with these files: data management issue
check = deploy %>% filter(!is.na(recover_date_time) & is.na(download_file_name)) # deployments were closed, but don't have data

# For a later stage: can add a quality check, based on comments / battery_estimated_end_date / ...




#### Visual exploration raster ####
raster::plot(bpns_raster)

bpns_df = as_tibble(raster::rasterToPoints(bpns_raster))
bpns_df %>% 
  ggplot(aes(x,y, colour = depth)) +
  geom_point(size = 0.1)
