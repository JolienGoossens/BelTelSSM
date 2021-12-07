#Observation_model_step1_and2a

source("./src/features/Exploration.R")

library(leaflet)
library(sp)
library(dplyr)
library(MASS)

fish<-df[which(df$tag_serial_number==1292646),]

#Dataframe with coordinates (make sure first long and then lat)
coords <- cbind(long = as.numeric(as.character((fish$deploy_longitude))),lat = as.numeric(as.character((fish$deploy_latitude))))

#################################"Plot detecting receivers

#Use coordinates dataframe to make SPDF
coords_SP <- SpatialPointsDataFrame(coords,data = data.frame(fish$station_name,fish$receiver_id), proj4string = CRS("+init=epsg:4326")) 
names(coords_SP)[1]<-"station_name"
names(coords_SP)[2]<-"receiver_id"
plot(coords_SP)

#Text that is depicted when clicked on location
Samples_text<-paste0("<b>station_name</b>: ",coords_SP@data$station_name,
                     "<br><b>receiver_id</b>: ",coords_SP@data$receiver_id)

#Use leaflet to make the map (internet required)
RangePlot<-leaflet()%>%
  addTiles()%>%
  addCircleMarkers(data=coords_SP,radius=8,fillColor = 'blue',fillOpacity=0.8,weight=1,color='black',popup=Samples_text,label=coords_SP@data$station_name,labelOptions = labelOptions(noHide = T))
RangePlot

fish$date_time<-as.POSIXct(fish$date_time,format="%y-%m-%d %H:%M:S",tz="UCT")
fish$Date<-as.Date(fish$date_time)

fish$count<-1

fish_count_per_receiver_and_date <- fish%>%
  dplyr::group_by(Date,station_name,deploy_longitude,deploy_latitude)%>%
  dplyr::summarise(count_sum=sum(count))
days_of_study_period<-as.data.frame(seq(min(fish_count_per_receiver_and_date$Date), max(fish_count_per_receiver_and_date$Date), by="days"))
colnames(days_of_study_period)="Date"
fish_count_per_receiver_and_date_full<-left_join(days_of_study_period,fish_count_per_receiver_and_date,by="Date")
fish_count_per_receiver_and_date_full$count_sum[which(is.na(fish_count_per_receiver_and_date$count_sum)==TRUE)]=0

fish_number_of_receivers_per_day <- as.data.frame(table(fish_count_per_receiver_and_date$Date))
fish_number_of_receivers_per_day$Var1<-as.Date(fish_number_of_receivers_per_day$Var1,format="%Y-%m-%d",tz="utc")

days_of_study_period<-as.data.frame(seq(min(fish_count_per_receiver_and_date$Date), max(fish_count_per_receiver_and_date$Date), by="days"))
colnames(days_of_study_period)="Var1"

fish_number_of_receivers_per_day_with_zeros<-left_join(days_of_study_period,fish_number_of_receivers_per_day,by="Var1")
fish_number_of_receivers_per_day_with_zeros$Freq[which(is.na(fish_number_of_receivers_per_day_with_zeros$Freq)==TRUE)]=0


# Plot kernels

Kernel <- kde2d(fish_count_per_receiver_and_date$deploy_longitude, fish_count_per_receiver_and_date$deploy_latitude, 
                n = 100, lims = c(2, 4, 51, 52),
            h = c(0.1, 0.2) )
image(f2, zlim = c(0, 0.05))

