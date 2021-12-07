#### Load packages ####
library(tidyverse)
library(lubridate)
library(terra)
library(sf)

#### Load data ####
recloc = read_csv("data/interim/reclocations.csv")
df_day = read_csv("data/interim/df_day.csv")

#### Set count ####
df_day = df_day %>% 
  mutate(det_count = ifelse(is.na(det_count), 0, det_count))
#### Load raster ####
bpns_rast = rast("data/external/hackraster.grd")

#### Set projection ####
newproj = "+proj=utm +zone=31 +datum=WGS84 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m "

#### Raster formatting ####
# Set CRS
terra::crs(bpns_rast)= "epsg:4326"

# Reproject
bpns_rast_m = terra::project(bpns_rast, newproj)

# save mask
save_mask = bpns_rast_m

# Set values to 0
bpns_rast_m = bpns_rast_m * 0
# bpns_rast_m = setValues(bpns_rast_m, 0)
names(bpns_rast_m) = "likelihood"


serial_id = unique(df_day$tag_serial_number)[1]
df_day_sub = df_day %>% filter(tag_serial_number == serial_id)

date_id = unique(df_day_sub$Date)[1]
df_day_sub = df_day_sub %>% filter(Date == date_id)

list_rast = lapply(unique(df_day$tag_serial_number), function(serial_id){
  df_day_sub = df_day %>% filter(tag_serial_number == serial_id)
  list_rast_serial = lapply(unique(df_day_sub$Date), function(date_id){
    df_day_sub = df_day_sub %>% filter(Date == date_id)
    if (any(df_day_sub$det_count > 0)) {
      # make a raster around fish date coordinates
      df_day_sub = st_as_sf(df_day_sub, 
                            coords = c("deploy_longitude", "deploy_latitude"),
                            crs = 4326)
      df_day_sub_m = st_transform(df_day_sub, crs = newproj)
      df_day_sub_m_sp = as(df_day_sub_m, "Spatial")
      
      #buff_close_m = buffer(fish_date_m_sp, 2000)
      buff_far_m_sp = buffer(df_day_sub_m_sp, 2000)
      
      #buff_close_m_sf <- st_as_sf(buff_close_m)
      buff_far_m <- st_as_sf(buff_far_m_sp)
      
      #buff_close_m_sf$likelihood <- 0.5
      buff_far_m$likelihood <- 0.9
      
      # buff_m_sf <- rbind(buff_close_m_sf,buff_far_m_sf)
      #buff_m_sf = st_combine(buff_close_m_sf,buff_far_m_sf)
      buff_m = buff_far_m
      buff_m_ras = terra::rasterize(vect(buff_m), bpns_rast_m,  field="likelihood", background = 0, update=T)
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
      
      buff_far_m_sp = buffer(recloc_date_m_sp, 2000)
      
      #buff_close_m_sf <- st_as_sf(buff_close_m)
      buff_far_m <- st_as_sf(buff_far_m_sp)
      
      #buff_close_m_sf$likelihood <- 0.5
      buff_far_m$likelihood <- 0.01
      
      # buff_m_sf <- rbind(buff_close_m_sf,buff_far_m_sf)
      #buff_m_sf = st_combine(buff_close_m_sf,buff_far_m_sf)
      buff_m = buff_far_m
      
      bpns_rast_m1 = bpns_rast_m + 1
      buff_m_ras = terra::rasterize(vect(buff_m), bpns_rast_m1, field="likelihood", background = 1, update=T)
      
      
      
      buff_m_ras$likelihood = buff_m_ras$likelihood / sum(as.matrix(buff_m_ras$likelihood), na.rm =T)
      
      buff_m_ras_mask = mask(buff_m_ras, bpns_rast_m)
      
      plot(buff_m_ras_mask)
      title(date_id)
    }
    
  })
})


if (any(df_day_sub$det_count > 0)) {
  # make a raster around fish date coordinates
  df_day_sub = st_as_sf(df_day_sub, 
                       coords = c("deploy_longitude", "deploy_latitude"),
                       crs = 4326)
  df_day_sub_m = st_transform(df_day_sub, crs = newproj)
  df_day_sub_m_sp = as(df_day_sub_m, "Spatial")
  
  #buff_close_m = buffer(fish_date_m_sp, 2000)
  buff_far_m_sp = buffer(df_day_sub_m_sp, 2000)
  
  #buff_close_m_sf <- st_as_sf(buff_close_m)
  buff_far_m <- st_as_sf(buff_far_m_sp)
  
  #buff_close_m_sf$likelihood <- 0.5
  buff_far_m$likelihood <- 0.9
  
  # buff_m_sf <- rbind(buff_close_m_sf,buff_far_m_sf)
  #buff_m_sf = st_combine(buff_close_m_sf,buff_far_m_sf)
  buff_m = buff_far_m
  buff_m_ras = terra::rasterize(vect(buff_m), bpns_rast_m,  field="likelihood", background = 0, update=T)
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
  
  buff_far_m_sp = buffer(recloc_date_m_sp, 2000)
  
  #buff_close_m_sf <- st_as_sf(buff_close_m)
  buff_far_m <- st_as_sf(buff_far_m_sp)
  
  #buff_close_m_sf$likelihood <- 0.5
  buff_far_m$likelihood <- 0.01
  
  # buff_m_sf <- rbind(buff_close_m_sf,buff_far_m_sf)
  #buff_m_sf = st_combine(buff_close_m_sf,buff_far_m_sf)
  buff_m = buff_far_m
  
  bpns_rast_m1 = bpns_rast_m + 1
  buff_m_ras = terra::rasterize(vect(buff_m), bpns_rast_m1, field="likelihood", background = 1, update=T)
  
  
  
  buff_m_ras$likelihood = buff_m_ras$likelihood / sum(as.matrix(buff_m_ras$likelihood), na.rm =T)
  
  buff_m_ras_mask = mask(buff_m_ras, bpns_rast_m)
  
  plot(buff_m_ras_mask)
  title(date_id)
}
