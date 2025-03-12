## Silvia Pineda-Munoz, PhD
## 18 February 2018
## cleaning up US background data



library(sp)
library(maps)
library(maptools)

# The single argument to this function, pointsDF, is a data.frame in which:
#   - column 1 contains the longitude in degrees (negative in the US)
#   - column 2 contains the latitude in degrees

latlong2state <- function(pointsDF) {
  
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per state (plus DC, minus HI & AK)
  states <- map("state", fill=T, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, states_sp)
  
  # Return the state names of the Polygons object containing each point
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  stateNames[indices]
}


backDataF<-read.csv("~/Google Drive/Post-doc/Niche Spaces/2019_03_31_GIS_Time_corrected analysis/datasets/FossilClimate/us_coordinates_climate.csv")
backDataM<-read.csv("~/Google Drive/Post-doc/Niche Spaces/2019_03_31_GIS_Time_corrected analysis/datasets/ModernClimate/modernBack_Climate.csv")
backDataM2<-read.csv("~/Google Drive/Post-doc/Niche Spaces/2019_03_31_GIS_Time_corrected analysis/datasets/ModernClimate/modernBack2_Climate.csv")


# Converting Modern background data
testPoints <- data.frame(backDataM$LONG,backDataM$LAT)
wherePoints<-latlong2state(testPoints)

backDataM<-backDataM[is.na(wherePoints)==F,]
plot(backDataM$LONG,backDataM$LAT)
write.csv(backDataM,"~/Google Drive/Post-doc/Niche Spaces/2019_03_31_GIS_Time_corrected analysis/datasets/Data_Analysis_files/Contiguous_US/background_modern_contiguous_US.csv")

# Converting Modern2 background data
testPoints <- data.frame(backDataM2$LONG,backDataM2$LAT)
wherePoints<-latlong2state(testPoints)

backDataM2<-backDataM2[is.na(wherePoints)==F,]
plot(backDataM2$LONG,backDataM2$LAT)
write.csv(backDataM2,"~/Google Drive/Post-doc/Niche Spaces/2019_03_31_GIS_Time_corrected analysis/datasets/Data_Analysis_files/Contiguous_US/background_modern2_contiguous_US.csv")



# Converting Fossil background data
testPoints <- data.frame(backDataF$LONG,backDataF$LAT)
wherePoints<-latlong2state(testPoints)

backDataF<-backDataF[is.na(wherePoints)==F,]
plot(backDataF$LONG,backDataF$LAT)
write.csv(backDataF,"~/Google Drive/Post-doc/Niche Spaces/2019_03_31_GIS_Time_corrected analysis/datasets/Data_Analysis_files/Contiguous_US/background_fossil_contiguous_US.csv")

# Converting Fossil data
fossilData<-read.csv("~/Google Drive/Post-doc/Niche Spaces/2019_03_31_GIS_Time_corrected analysis/datasets/Data_Analysis_files/fossil_data_with_climate.csv")

testPoints <- data.frame(fossilData$LONG,fossilData$LAT)
wherePoints<-latlong2state(testPoints)

fossilData<-fossilData[is.na(wherePoints)==F,]
plot(fossilData$LONG,fossilData$LAT)
write.csv(fossilData,"~/Google Drive/Post-doc/Niche Spaces/2019_03_31_GIS_Time_corrected analysis/datasets/Data_Analysis_files/Contiguous_US/fossil_climate_contiguous_US.csv")

# Converting Modern data
modernData<-read.csv("~/Google Drive/Post-doc/Niche Spaces/2019_03_31_GIS_Time_corrected analysis/datasets/Data_Analysis_files/modern_data_with_climate.csv")

testPoints <- data.frame(modernData$LONG,modernData$LAT)
wherePoints<-latlong2state(testPoints)

modernData<-modernData[is.na(wherePoints)==F,]
plot(modernData$LONG,modernData$LAT)
write.csv(modernData,"~/Google Drive/Post-doc/Niche Spaces/2019_03_31_GIS_Time_corrected analysis/datasets/Data_Analysis_files/Contiguous_US/modern_climate_contiguous_US.csv")


