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

#### Make raster for every day ####
list_rast = lapply(unique(df_day$tag_serial_number), function(serial_id){
  print(paste0("Started with serial ID ", serial_id))
  df_day_sub = df_day %>% filter(tag_serial_number == serial_id)
  list_rast_serial = lapply(unique(df_day_sub$Date), function(date_id){
    df_day_sub = df_day_sub %>% filter(Date == date_id)
    if (any(df_day_sub$det_count > 0)) {  # make a raster around fish date coordinates
      # Transform data to spatial object
      df_day_sub = st_as_sf(df_day_sub, 
                            coords = c("deploy_longitude", "deploy_latitude"),
                            crs = 4326)
      # Reproject
      df_day_sub_m = st_transform(df_day_sub, crs = newproj)
      
      # Buffer around detection location(s)
      buff_close_m = df_day_sub_m %>% st_buffer(dist = dist_close) %>% mutate(likelihood = 2)
      buff_far_m = df_day_sub_m %>% st_buffer(dist = dist_far) %>% mutate(likelihood = 2)
      
      # Vectorize buffer
      buff_close_m_v = buff_close_m %>% vect()
      buff_far_m_v = buff_far_m %>% vect()
      
      # Set likelihood of base layer
      bpns_rast_m = bpns_rast_m0 + lik_det_out
      
      # Rasterize buffered vectors
      buff_m_ras_close = terra::rasterize(buff_close_m_v, bpns_rast_m, 
                                          field="likelihood", 
                                          touches = T, background = lik_nodet_out, update=T)
      buff_m_ras_far = terra::rasterize(buff_far_m_v, bpns_rast_m, 
                                        field="likelihood", 
                                        touches = T, background = lik_nodet_out, update=T)
      
      # Combine close and far rasters
      buff_m_try = buff_m_ras_far * buff_m_ras_close
      buff_m_try[buff_m_try == 4] = lik_det_close
      buff_m_try[buff_m_try == 2] = lik_det_far
      
      # Add mask
      buff_m_ras_mask = mask(buff_m_try, bpns_rast_m0)
      # Set total likelihood to 1
      buff_m_ras_mask$likelihood = buff_m_ras_mask$likelihood / sum(as.matrix(buff_m_ras_mask$likelihood), na.rm =T)
      
      # Plot
      # plot(buff_m_ras_mask)
      # title(date_id)
      
    } else { # make a raster around receiver locations
      # Transform data to spatial object
      recloc_date = st_as_sf(recloc %>% dplyr::filter(Date == date_id), 
                             coords = c("deploy_longitude", "deploy_latitude"),
                             crs = 4326)
      # Reproject
      recloc_date_m = st_transform(recloc_date, crs = newproj)
      
      # Buffer around receiver locations
      buff_close_m = recloc_date_m %>% st_buffer(dist = dist_close) %>% mutate(likelihood = 2)
      buff_far_m = recloc_date_m %>% st_buffer(dist = dist_far) %>% mutate(likelihood = 2)
      
      # Vectorize buffer
      buff_close_m_v = buff_close_m %>% vect()
      buff_far_m_v = buff_far_m %>% vect()
      
      # Set likelihood of base layer
      bpns_rast_m = bpns_rast_m0 + lik_nodet_out
      
      # Rasterize buffered vectors
      buff_m_ras_close = terra::rasterize(buff_close_m_v, bpns_rast_m, 
                                          field="likelihood", 
                                          touches = T, background = lik_nodet_out, update=T)
      buff_m_ras_far = terra::rasterize(buff_far_m_v, bpns_rast_m, 
                                        field="likelihood", 
                                        touches = T, background = lik_nodet_out, update=T)
      # Combine close and far rasters
      buff_m_try = buff_m_ras_far * buff_m_ras_close
      buff_m_try[buff_m_try == 4] = lik_nodet_close
      buff_m_try[buff_m_try == 2] = lik_nodet_far
      
      # Add mask
      buff_m_ras_mask = mask(buff_m_try, bpns_rast_m0)
      # Set total likelihood to 1
      buff_m_ras_mask$likelihood = buff_m_ras_mask$likelihood / sum(as.matrix(buff_m_ras_mask$likelihood), na.rm =T)
      
      # Plot
      # plot(buff_m_ras_mask)
      # title(date_id)
    }
    return(buff_m_ras_mask)
  })
  
  # Stack rasters of all days 
  rast_serial = terra::rast(list_rast_serial)
  names(rast_serial)  = unique(df_day_sub$Date)
  
  # Save stacked raster
  output_name = paste0("data/processed/obsmodel/obs_", serial_id, ".tif")
  writeRaster(rast_serial, output_name, overwrite = T)
})
