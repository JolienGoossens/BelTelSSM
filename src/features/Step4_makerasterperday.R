#### Load packages ####
library(tidyverse)
library(terra)

#### Load data ####
fish_count_per_receiver_and_date # Observation_model_step1and2a
recloc = read_csv("data/interim/reclocations.csv")

# Read in terra
bpns_rast = rast("data/external/hackraster.grd")

# Set projection
newproj = "+proj=utm +zone=31 +datum=WGS84 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m "


#### DAta formatting ####
recloc = recloc %>% mutate(Date = date_time_day) %>% dplyr::select(-date_time_day)

colnames(fish_count_per_receiver_and_date)
colnames(recloc)

#### Raster formatting ####
terra::crs(bpns_rast)= "EPSG:4326"
bpns_rast = bpns_rast *0
names(bpns_rast) = "likelihood"

# Reproject
bpns_rast_m = terra::project(bpns_rast, newproj)

plot(bpns_rast_m)
#### work out for one day
# set fish id
date_id = fish_count_per_receiver_and_date$Date[2]

list_raster = lapply(unique(fish_count_per_receiver_and_date$Date), function(date_id){
  fish_date = fish_count_per_receiver_and_date %>% 
    filter(Date == date_id)
  
  if (any(fish_date$count_sum > 0)) {
    # make a raster around fish date coordinates
    fish_date = st_as_sf(fish_date, 
                         coords = c("deploy_longitude", "deploy_latitude"),
                         crs = 4326)
    fish_date_m = st_transform(fish_date, crs = newproj)
    fish_date_m_sp = as(fish_date_m, "Spatial")
    
    #buff_close_m = buffer(fish_date_m_sp, 2000)
    buff_far_m = buffer(fish_date_m_sp, 1000)
    
    #buff_close_m_sf <- st_as_sf(buff_close_m)
    buff_far_m_sf <- st_as_sf(buff_far_m)
    
    #buff_close_m_sf$likelihood <- 0.5
    buff_far_m_sf$likelihood <- 0.9
    
    # buff_m_sf <- rbind(buff_close_m_sf,buff_far_m_sf)
    #buff_m_sf = st_combine(buff_close_m_sf,buff_far_m_sf)
    buff_m_sf = buff_far_m_sf
    buff_m_ras = terra::rasterize(vect(buff_m_sf),bpns_rast_m, field="likelihood", background = 0, update=T)
    buff_m_ras_mask = mask(buff_m_ras, bpns_rast_m)
    
    plot(buff_m_ras_mask)
    title(date_id)
    
  } else {
    # make a raster around receiver locations
    recloc_date = st_as_sf(recloc %>% dplyr::filter(Date == date_id), 
                           coords = c("deploy_longitude", "deploy_latitude"),
                           crs = 4326)
    recloc_date_m = st_transform(recloc_date, crs = newproj)
    recloc_date_m_sp = as(recloc_date_m, "Spatial")
    
    buff_far_m = buffer(recloc_date_m_sp, 1000)
    
    #buff_close_m_sf <- st_as_sf(buff_close_m)
    buff_far_m_sf <- st_as_sf(buff_far_m)
    
    #buff_close_m_sf$likelihood <- 0.5
    buff_far_m_sf$likelihood <- 0.9
    
    # buff_m_sf <- rbind(buff_close_m_sf,buff_far_m_sf)
    #buff_m_sf = st_combine(buff_close_m_sf,buff_far_m_sf)
    buff_m_sf = buff_far_m_sf
    buff_m_ras = terra::rasterize(vect(buff_m_sf),bpns_rast_m, field="likelihood", background = 0, update=T)
    
    
    
    buff_m_ras$likelihood = buff_m_ras$likelihood / sum(as.matrix(buff_m_ras$likelihood), na.rm =T)

    
    buff_m_ras_mask = mask(buff_m_ras, bpns_rast_m)
    
    
    buff_m_ras$likelihood = (buff_m_ras$likelihood - max(buff_m_ras$likelihood, na.rm = T))*-1
    plot(buff_m_ras_mask)
    title(date_id)
    
    
  }
})
