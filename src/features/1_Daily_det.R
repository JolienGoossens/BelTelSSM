
#### Load packages ####
library(tidyverse)
library(lubridate)

#### Get data ####
df = read_csv("data/raw/df.csv")
an = read_csv("data/raw/an.csv")
tags = read_csv("data/raw/tags.csv")

#### Get period ####
list_df_day = lapply(unique(an$tag_serial_number), function(serial_id){
  # Subset animal data
  an_sub = an %>% filter(tag_serial_number == serial_id)
  
  # Get battery lifetime
  bat_life_time = as.data.frame(tags) %>% 
    filter(tag_serial_number == serial_id) %>% 
    distinct(battery_estimated_life) %>% pull()
  
  # Make sequence of date from release
  date_seq = seq.Date(from = date(an_sub$release_date_time), 
                      to = date(an_sub$release_date_time) + days(bat_life_time),
                      by = "days")
  
  # Summarise detection data per date
  df_day = df %>% 
    filter(tag_serial_number == serial_id) %>%
    mutate(Date = date(date_time)) %>% 
    group_by(Date,
             station_name, deploy_latitude, deploy_longitude) %>% 
    summarise(det_count = n())
  
  # Add release location and date
  df_day = rbind(
    tibble(
      Date = date(an_sub$release_date_time),
      station_name = an_sub$release_location,
      deploy_latitude = an_sub$release_latitude, 
      deploy_longitude = an_sub$release_longitude,
      det_count = 1
    ),
    df_day)
  
  # Add with date sequence
  df_day = full_join(df_day, tibble(Date = date_seq)) %>% 
    arrange(Date) %>% 
    mutate(tag_serial_number = serial_id,
           animal_id = unique(an_sub$animal_id))
  
  return(df_day)
})

df_day = as_tibble(plyr::ldply(list_df_day))

#### Save ####
write_csv(df_day, "data/interim/df_day.csv")



