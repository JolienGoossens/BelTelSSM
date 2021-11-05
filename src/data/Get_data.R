
# Get data from ETN #

#### Load packages ####
#devtools::install_github("inbo/etn")
library(etn)
library(tidyverse)

#### Settings for subset hackathon ####
# Serial numbers of acoustic tags
tag_serial_number_sub = c(1292646, 1400201, 1293271)
# Project codes of acoustic telemetry networks active in the area at the time of the seabass research
acoustic_project_code_potential = c("bpns", "cpodnetwork", "ws1", "ws2", "ws3", "JJ_Belwind", "pc4c", "OP-Test", "Apelafico")

#### Get data ####
# Set connection to username and password for access to etn database 
# See etn vignette: https://github.com/inbo/etn/blob/main/vignettes/acoustic_telemetry.Rmd
# See Rstudio info on securing credentials: https://db.rstudio.com/best-practices/managing-credentials/#use-environment-variables
my_con <- connect_to_etn(Sys.getenv("username"),
                         Sys.getenv("password"))

# Get acoustic detections
df = get_acoustic_detections(connection = my_con,  animal_project = "PhD_Goossens")
df = df %>% filter(tag_serial_number %in% tag_serial_number_sub) # Subset for hackathon

# Get animal metadata
an = get_animals(connection = my_con, tag_serial_number = tag_serial_number_sub)

# Get tag metadata
tags = get_tags(connection = my_con, tag_serial_number = tag_serial_number_sub)

# Get acoustic receiver deployment metadata
deploy = get_acoustic_deployments(connection = my_con, acoustic_project_code = acoustic_project_code_potential)

#### Save data ####
write_csv(df, "data/raw/df.csv")
write_csv(an, "data/raw/an.csv")
write_csv(tags , "data/raw/tags.csv")
write_csv(deploy, "data/raw/deploy.csv")

