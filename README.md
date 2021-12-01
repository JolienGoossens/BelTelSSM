# BelTelSSM

On 2 - 3 December 2021 we endeavour to develop a state space model to analyze presence of animals tagged with acoustic transmitters in the Belgian Part of the North Sea (BPNS). During these days, we will work on data of European seabass (*Dicentrarchus labrax*), tagged since 2018 in the framework of the PhD of Jolien Goossens. The tagged seabass were detected on the LifeWatch Permanent Belgian Acoustic Receiver Network (PBARN; see [paper](https://animalbiotelemetry.biomedcentral.com/articles/10.1186/s40317-019-0164-8) and [website](https://lifewatch.be/en/fish-acoustic-receiver-network)) and temporary receiver arrays in the area. 

## Preparation
In the **src/data** folder, you will find two scripts.

#### Step 1: Make folders
Run the script *Set_up_folders.R* in the *src/data* folder to make directories (if they are not yet there).

#### Step 2: Get data
Data were taken from the European Tracking Network ([ETN](https://lifewatch.be/etn/)) database through the [ETN package](https://github.com/inbo/etn). Code for the retrieval of this data is available in *Get_data.R* in the *src/data* folder.

However, as these data are under moratorium, we will share these data as a zip folder **data.zip**. Save and unzip this file as the folder **data** in BelTelSSM main directory.

The available data are: 

Folder **data/raw**:

* **df.csv**: detection data
* **an.csv**: animal metadata
* **tags.csv**: tag metadata

* **deploy.csv**: acoustic receiver deployment metadata

Folder **data/external**:

* **hackraster.grd**: raster of BPNS (0.5nm x 0.5 nm resolution). This was obtained from the Delft 3D DCSM-FM model.

#### Step 3: Explore the available data
The *Exploration.R* script in the *src/features* folder contains some basic exploration of the data.

## Observation model
For every day in the lifetime of a tag (or until the capture or death of a tagged animal), we will develop a raster where we assign a likelihood to each pixel of that raster. Therefore, the likelihood (0 - 1) in a pixel represents the likelihood the tagged fish was present in that pixel during that day.

Assigning this likelihood will be defined differently if the animal was:

* **Detected**: The likelihood is defined as a kernel around the position of the receiver where the fish was detected.
* **Not detected**: The likelihood is defined as kernels around the position of the receivers where the fish was not detected.

#### Step 1: Get relevant time period of tag deployment
Define the start date (= date the animal was tagged) and end date (= end of tag battery life time OR date of animal death).

#### Step 2.A: Animal detections
Get locations of animal detections for every day of the tag lifetime

#### Step 2.B: Deployment data
Get locations of acoustic receiver deployments (with successfull data offloading) for every day of the tag lifetime

#### Step 3.A: Rasterize
Create a function to put locations on a raster

#### Step 3.B: Account for detection efficiency
Create a function to add a detection range kernel (or raster equivalent) around a location on a raster

#### Step 4: Create a likelihood raster per day
Create a raster for every day of the tag lifetime based on deployment data and / or detection data

#### Issues to consider and discuss:

* 2.A: How to deal with multiple detection locations in one day (finer temporal resolution, etc.)? 
* 2.B: Quality assessment of deployment data?
* 3.A: Does the 0.5 x 0.5 nm resolution suffice?
* 3.B: Simple function or account for tag
* 4: Do we account for deployment data if a tag was detected? What likelihood do we address to different pixels (~ how sure are we a tag was or wasn't detected)


## Movement model

The likelihood rasters of the observation model are used as input for the movement model. The conditional nature of movement (i.e. where a fish is at moment t1 will limit the area where the same fish can be at moment t2 because of the limited maximum swimming speed) introduces temporal and spatial autocorrelation between detections. This [autocorrelation](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2894959/) is used in the movement model and allows to use the information stored between detections rather than within detections alone. 

### Which movement model to use?

* Continuous or discrete time formulation?
* Account for a centralizing tendency (i.e. home range behavior?)
* ...

Useful R packages for movement models, click [here](https://gist.github.com/mdsumner/0a3cb0e58bf9d37b782943ac269e1eff).

## Relevant literature

* [Estimating individual animal movement from observation networks](https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.12086)
* [Navigating through the r packages for movement](https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/1365-2656.13116)
