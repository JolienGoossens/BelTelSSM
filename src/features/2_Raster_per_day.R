#### Load packages ####
library(tidyverse)
library(lubridate)
library(terra)
library(sf)

#### Load data ####
recloc = read_csv("data/interim/reclocations.csv")
df_day = read_csv("data/interim/df_day.csv")

#### Load raster ####
bpns_rast = rast("data/external/hackraster.grd")

#### Settings ####
# Set projection
newproj = "+proj=utm +zone=31 +datum=WGS84 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m "

# Set distances in m
dist_close = 500
dist_far = 2000

# Set likelihoods (relative, will be recalculated later)
lik_nodet_close = 0.01
lik_nodet_far = 0.1
lik_nodet_out = 1
lik_det_close = 1
lik_det_far = 0.1
lik_det_out = 0.01

#### Set count ####
df_day = df_day %>% 
  mutate(det_count = ifelse(is.na(det_count), 0, det_count))

#### Raster formatting ####
# Set CRS
terra::crs(bpns_rast)= "epsg:4326"

# Reproject
bpns_rast_m = terra::project(bpns_rast, newproj)

# save mask
save_mask = bpns_rast_m

# Set values to 0
bpns_rast_m0 = bpns_rast_m * 0
names(bpns_rast_m0) = "likelihood"


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
      
      buff_close_m_sp = buffer(df_day_sub_m_sp, dist_close)
      buff_far_m_sp = buffer(df_day_sub_m_sp, dist_far)
      
      buff_close_m_sf <- st_as_sf(buff_close_m_sp)
      buff_far_m_sf <- st_as_sf(buff_far_m_sp)
      
      buff_close_m_sf$likelihood <- lik_det_close
      buff_far_m_sf$likelihood <- lik_det_far
      
      buff_close_m_v = vect(buff_close_m_sf)
      buff_far_m_v = vect(buff_far_m_sf)
      
      buff_m_v = terra::cover(buff_far_m_v, buff_close_m_v)
      
      bpns_rast_m = bpns_rast_m0 + lik_det_out
      buff_m_ras = terra::rasterize(buff_m_v, bpns_rast_m,  field="likelihood", background = lik_det_out, update=T)
      buff_m_ras$likelihood = buff_m_ras$likelihood / sum(as.matrix(buff_m_ras$likelihood), na.rm =T)
      
      buff_m_ras_mask = mask(buff_m_ras, bpns_rast_m0)
      
      plot(buff_m_ras_mask)
      title(paste0("Tag ", serial_id, " - date ", date_id))
      
    } else {
      # make a raster around receiver locations
      recloc_date = st_as_sf(recloc %>% dplyr::filter(Date == date_id), 
                             coords = c("deploy_longitude", "deploy_latitude"),
                             crs = 4326)
      recloc_date_m = st_transform(recloc_date, crs = newproj)
      recloc_date_m_sp = as(recloc_date_m, "Spatial")
      
      buff_close_m_sp = buffer(recloc_date_m_sp, dist_close)
      buff_far_m_sp = buffer(recloc_date_m_sp, dist_far)
      
      buff_close_m_sf <- st_as_sf(buff_close_m_sp)
      buff_far_m_sf <- st_as_sf(buff_far_m_sp)
      
      buff_close_m_sf$likelihood <- lik_nodet_close
      buff_far_m_sf$likelihood <- lik_nodet_far
      
      buff_close_m_v = vect(buff_close_m_sf)
      buff_far_m_v = vect(buff_far_m_sf)
      
      buff_m_v = terra::cover(buff_far_m_v, buff_close_m_v)
      
      bpns_rast_m = bpns_rast_m0 + lik_nodet_out
      buff_m_ras = terra::rasterize(buff_m_v, bpns_rast_m, field="likelihood", background = lik_nodet_out, update=T)
      buff_m_ras$likelihood = buff_m_ras$likelihood / sum(as.matrix(buff_m_ras$likelihood), na.rm =T)
      
      buff_m_ras_mask = mask(buff_m_ras, bpns_rast_m0)
      
      plot(buff_m_ras_mask)
      title(date_id)
    }
    
  })
})


serial_id = unique(df_day$tag_serial_number)[1]
df_day_sub = df_day %>% filter(tag_serial_number == serial_id)

date_id = unique(df_day_sub$Date)[5]
df_day_sub = df_day_sub %>% filter(Date == date_id)






if (any(df_day_sub$det_count > 0)) {
  # make a raster around fish date coordinates
  df_day_sub = st_as_sf(df_day_sub, 
                       coords = c("deploy_longitude", "deploy_latitude"),
                       crs = 4326)
  df_day_sub_m = st_transform(df_day_sub, crs = newproj)
  df_day_sub_m_sp = as(df_day_sub_m, "Spatial")
  
  buff_close_m_sp = buffer(df_day_sub_m_sp, dist_close)
  buff_far_m_sp = buffer(df_day_sub_m_sp, dist_far)
  
  buff_close_m_sf <- st_as_sf(buff_close_m_sp)
  buff_far_m_sf <- st_as_sf(buff_far_m_sp)
  
  buff_close_m_sf$likelihood <- lik_det_close
  buff_far_m_sf$likelihood <- lik_det_far
  
  buff_close_m_v = vect(buff_close_m_sf)
  buff_far_m_v = vect(buff_far_m_sf)
  
  buff_m_v = terra::cover(buff_far_m_v, buff_close_m_v)
  
  bpns_rast_m = bpns_rast_m0 + lik_det_out
  buff_m_ras = terra::rasterize(buff_m_v, bpns_rast_m,  field="likelihood", background = lik_det_out, update=T)
  buff_m_ras$likelihood = buff_m_ras$likelihood / sum(as.matrix(buff_m_ras$likelihood), na.rm =T)
  
  buff_m_ras_mask = mask(buff_m_ras, bpns_rast_m0)
  
  plot(buff_m_ras_mask)
  title(paste0("Tag ", serial_id, " - date ", date_id))
  
} else {
  # make a raster around receiver locations
  recloc_date = st_as_sf(recloc %>% dplyr::filter(Date == date_id), 
                         coords = c("deploy_longitude", "deploy_latitude"),
                         crs = 4326)
  recloc_date_m = recloc_date %>% st_transform(crs = newproj)
  
  buff_close_m = recloc_date_m %>% st_buffer(dist = dist_close) %>% mutate(likelihood = lik_nodet_close)
  buff_far_m = recloc_date_m %>% st_buffer(dist = dist_far) %>% mutate(likelihood = lik_nodet_far)

  buff_close_m_v = buff_close_m %>% st_union() %>% vect(layer = "likelihood")
  buff_far_m_v = buff_far_m %>% st_union() %>% vect()
  
  buff_m_v = terra::cover(buff_far_m_v, buff_close_m_v)
  plot(buff_m_v)
  
  recloc_date_m_sp = as(recloc_date_m, "Spatial")
  
  buff_close_m_sp = raster::buffer(recloc_date_m_sp, dist_close)
  buff_far_m_sp = raster::buffer(recloc_date_m_sp, dist_far)
  
  buff_close_m_sf <- st_as_sf(buff_close_m_sp)
  buff_far_m_sf <- st_as_sf(buff_far_m_sp)
  
  buff_close_m_sf$likelihood <- lik_nodet_close
  buff_far_m_sf$likelihood <- lik_nodet_far
  
  
  buff_close_m_v = vect(buff_close_m_sf)
  buff_far_m_v = vect(buff_far_m_sf)
  
  plot(buff_close_m_v)
  buff_m_v = terra::cover(buff_far_m_v, buff_close_m_v)
  buff_m_v = union(buff_far_m_v, buff_close_m_v)
  plot(buff_m_v)
  
  
  
  bpns_rast_m = bpns_rast_m0 + lik_nodet_out
  buff_m_ras = terra::rasterize(buff_m_v, bpns_rast_m, fun = "min", field="likelihood", touches = T, background = lik_nodet_out, update=T)
  
  
  
  plot(buff_m_ras)
  buff_m_ras = terra::rasterize(buff_close_m_v, bpns_rast_m, touches = T)
  buff_m_ras = buff_m_ras > 0
  plot(buff_close_m_v)
  
  
  
  
  
  buff_m_ras$likelihood = buff_m_ras$likelihood / sum(as.matrix(buff_m_ras$likelihood), na.rm =T)
  plot(buff_m_ras)
  buff_m_ras_mask = mask(buff_m_ras, bpns_rast_m0)
  
  plot(buff_m_ras_mask)
  title(date_id)
}
