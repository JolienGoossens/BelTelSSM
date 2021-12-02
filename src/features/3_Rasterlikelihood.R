#### Load package ####
library(terra)
library(raster)

#### Read raster file ####
# Read in terra
bpns_rast = rast("data/external/hackraster.grd")

# Read in raster
bpns_raster = raster::raster("data/external/hackraster.grd")

#### Make likelihood layer ####
bpns_raster = bpns_raster %>% setValues(0)
names(bpns_raster) = "likelihood"
raster::crs(bpns_raster) = "EPSG:4326"

bpns_raster


              
# Change projection to one in meters
#projectRaster(bpns_raster, crs=newproj)

# Draw circle
# Rescale likelihoods to sum = 1

