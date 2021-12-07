#### Load package ####
library(terra)
library(raster)
library(tidyverse)
library(stars)

#### Read raster file ####
# Read in terra
bpns_rast = rast("data/external/hackraster.grd")

# Read in raster
bpns_raster = raster::raster("data/external/hackraster.grd")

# Read in sf
bpns_sf = st_read("data/external/hackraster.grd")

# Read eez
eez = st_read("data/eez.shp")

#### Read daily position data ####
recloc = read_csv("data/interim/reclocations.csv")
recloc = recloc %>% mutate(likelihood = 1)

#### Make likelihood layer ####
bpns_raster = bpns_raster %>% setValues(0)
names(bpns_raster) = "likelihood"
raster::crs(bpns_raster) = "EPSG:4326"
raster::crs(bpns_raster) = 4326


terra::crs(bpns_rast)= "EPSG:4326"
bpns_rast = bpns_rast *0
names(bpns_rast) = "likelihood"

#### Get rec positions as a point ####
# One receiver
rec1 = st_as_sf(recloc[1,], 
                coords = c("deploy_longitude", "deploy_latitude"),
                crs = 4326)

# All receivers of one day
recday1 = st_as_sf(recloc %>% filter(date_time_day == unique(recloc$date_time_day)[1]), 
                   coords = c("deploy_longitude", "deploy_latitude"),
                   crs = 4326)

#### Change projection to one in meters ####
newproj = "+proj=utm +zone=31 +datum=WGS84 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m "
# bpns_raster_m = projectRaster(bpns_raster, crs=st_crs(rec1_m)$wkt)

bpns_rast_m = terra::project(bpns_rast, newproj)
rec1_m = st_transform(rec1, crs = newproj)
recday1_m = st_transform(recday1, crs = newproj)
eez_m = st_transform(eez, crs = newproj)


buff_sf <- st_as_sf(buff_min_second_m)
buff_sf_m = st_transform(buff_sf, crs = newproj)
buff_sf_m$likelihood <- 0.5

buff_sf_fir <- st_as_sf(buff_min_first_m)
buff_sf_fir_m = st_transform(buff_sf_fir, crs = newproj)
buff_sf_fir_m$likelihood <- 0.9
merged <- rbind(buff_sf_m,buff_sf_fir_m)

#### Put points on a raster ####
# terra
check = terra::rasterize(vect(rec1_m),bpns_rast_m, background = 0)
check = terra::rasterize(vect(recday1_m),bpns_rast_m, background = 0)
check = terra::rasterize(vect(eez_m),bpns_rast_m, background = 0, update=T)

check = terra::rasterize(vect(merged),bpns_rast_m, field="likelihood", background = 0, update=T)

check_mask = mask(check, bpns_rast_m)
plot(check)
plot(check_mask)

# stars
bpns_rast_m_stars = st_as_stars(bpns_rast_m)
check = st_rasterize(rec1_m, bpns_rast_m_stars)



# Draw circle
# Rescale likelihoods to sum = 1




# make a spatial object in the format of the package sf
# check for difference point / kernel
# put this on raster
# change likelihood value of this coordinate



