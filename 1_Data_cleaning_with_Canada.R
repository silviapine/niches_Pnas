## Silvia Pineda-Munoz, PhD
## 31 March 2019
## Preaparing data for niche analysis
# A) Faunmap data and sensitivity data
# B) Background data 
# C) Gbif Data
# D) Land use data

library("rjson")
library(sf)
library(maptools)
data("wrld_simpl")
library("dplyr")
library(sp)
library(maps)



Wmap<-st_as_sf(wrld_simpl)
USmap<-filter(Wmap,NAME %in% c("United States"))
plot(USmap$geometry)
baseMap<-st_transform(USmap,crs = "+proj=longlat +datum=WGS84")

###
###A) Faunmap Data
###
setwd("~/Google Drive/Post-doc/Niche Spaces/2019_03_31_GIS_Time_corrected analysis/datasets")
combined<-read.csv("Calibrated_ages.csv")
length(unique(combined$Site))


combined<-droplevels(combined[combined$Class=="Mammalia",])#selects only mammal occurrences
combined<-droplevels(combined[is.na(combined$LATDD)==F,])#remove NAs
combined<-droplevels(combined[is.na(combined$LONGDD)==F,])
length(unique(combined$Site))



combined$Average.age<-(combined$MaxAgeCal+combined$MinAgeCal)/2 #Correct average dates
combined$A_age_yrs<-combined$Average.age*1000000

dataAnal<-data.frame(siteid=combined$Site,Class=combined$Class,Order=combined$Order,
                     Family=combined$Family,
                     Genus=combined$Genus,Species=combined$Species,LAT=combined$LAT,
                     LONG=combined$LONG, MinAge=combined$MinAgeCal,
                     MaxAge=combined$MaxAgeCal,AvAge=combined$Average.age)
head(dataAnal)
length(unique(dataAnal$siteid))

dataAnal<-dataAnal[dataAnal$AvAge<=20000,]

length(unique(dataAnal$siteid))

write.csv(dataAnal,"faunmapFossilDataCalibratedPleisCan.csv")##this contains all the data
forClimateFaunmap<-na.omit(unique(data.frame(siteid=dataAnal$siteid, Age=dataAnal$AvAge,
                                             LAT=dataAnal$LAT,
                                             LONG=dataAnal$LONG)))

write.csv(forClimateFaunmap,"faunmapLocalitiesPleisCan.csv")

##Sensitivity analysis
dataAnal<-read.csv("faunmapFossilDataCalibratedPleisCan.csv")
SenLocalities<-na.omit(unique(data.frame(siteid=dataAnal$siteid, MaxAge=dataAnal$MaxAge,
                       MinAge=dataAnal$MinAge,LAT=dataAnal$LAT,
                       LONG=dataAnal$LONG)))

subLoc<-SenLocalities[1,]
numTimes<-seq(subLoc$MinAge,subLoc$MaxAge,by=50)
Ssiteid<-rep(subLoc[1,1],times=length(numTimes))
subSiteid<-paste0(Ssiteid,"_",numTimes,"yrs")
Lats1<-rep(subLoc$LAT,times=length(numTimes))
Longs1<-rep(subLoc$LONG,times=length(numTimes))
Age<-numTimes
siteIds<-subSiteid
oldSiteid<-Ssiteid
LAT<-Lats1
LONG<-Longs1
for(i in 1:nrow(SenLocalities)){
  subLoc<-SenLocalities[i,]
  numTimes<-seq(min(subLoc$MinAge,subLoc$MaxAge),
                max(subLoc$MinAge,subLoc$MaxAge),by=50)
  Ssiteid<-rep(subLoc[1,1],times=length(numTimes))
  subSiteid<-paste0(Ssiteid,"_",numTimes,"yrs")
  Lats1<-rep(subLoc$LAT,times=length(numTimes))
  Longs1<-rep(subLoc$LONG,times=length(numTimes))
  Age<-c(Age,numTimes)
  siteIds<-c(siteIds,subSiteid)
  oldSiteid<-c(as.character(oldSiteid),as.character(Ssiteid))
  LAT<-c(LAT,Lats1)
  LONG<-c(LONG,Longs1)
  
}

sensData<-data.frame(oldSiteId=oldSiteid,sieid=siteIds,
           age=Age,LAT=LAT,LONG=LONG)
write.csv(sensData,"sesitivityAnalDataCan.csv")



####
####
####
####B) Background Data
###
####
####

#Obtaining lat and long Data for the US


## Create a SpatialPoints object

set.seed(0)
lat <- seq(10,70,by=1)
lon <- seq(-45,-180,by=-1)
points <- expand.grid(lon, lat)
points <- data.frame(lon=points$Var1,lat=points$Var2)

Wmap<-st_as_sf(wrld_simpl)
USmap<-filter(Wmap,NAME %in% c("United States"))
baseMap<-st_transform(USmap,crs = 4326)


pointsMap <- st_as_sf(points,
                      coords = c('lon', 
                                 'lat'),
                      crs = 4326,dim="XY")

interPointsMap<-st_intersection(pointsMap,baseMap)

plot(interPointsMap$geometry,pch=19,cex=0.5,col="red")

coordMap<-data.frame(st_coordinates(interPointsMap))

landcoords<-data.frame(Lat=coordMap$Y,Long=coordMap$X)


timeBins<-seq(0,20000, by=500)

latLongs<-data.frame(paste0(timeBins[1],"yrs",1:nrow(landcoords)),
                     landcoords,
                     rep(timeBins[1],nrow(landcoords)))
colnames(latLongs)<-c("siteid","LAT","LONG","Age")
for (i in 2:length(timeBins)){
  newLL<-data.frame(paste0(timeBins[i],"yrs",1:nrow(landcoords)),
                    landcoords,rep(timeBins[i],nrow(landcoords)))
  colnames(newLL)<-c("siteid","LAT","LONG","Age")
  latLongs<-rbind(latLongs,newLL)
}

head(latLongs)
write.csv(latLongs,"us_coordinates.csv")


###
###C) Gbif data
###
###
###

###Getting data from modern occurrences

listFiles<-list.files("~/Google Drive/Post-doc/Niche Spaces/Getting_modern_data/Species_data_modern/")
modernData<-read.csv(paste0("~/Google Drive/Post-doc/Niche Spaces/Getting_modern_data/Species_data_modern/",listFiles[1]))
modernData<-modernData[modernData$basisOfRecord!="FOSSIL_SPECIMEN",]
modernData<-modernData[modernData$basisOfRecord!="UNKNOWN",]
modernData<-droplevels(modernData)
modernData<-data.frame(name=modernData$name,LAT=modernData$decimalLatitude,
                       LONG=modernData$decimalLongitude)

for(i in 2:length(listFiles)){
  newD<-read.csv(paste0("~/Google Drive/Post-doc/Niche Spaces/Getting_modern_data/Species_data_modern/",listFiles[i]))
  newD<-newD[newD$basisOfRecord!="FOSSIL_SPECIMEN",]
  newD<-newD[newD$basisOfRecord!="UNKNOWN",]
  newD<-droplevels(newD)
  if(nrow(newD)>=100){
    newD<-data.frame(name=newD$name,LAT=newD$decimalLatitude,
                     LONG=newD$decimalLongitude)
    modernData<-rbind(modernData,newD)
  }
}

speciesPleis<-read.csv("~/Google Drive/Post-doc/Niche Spaces/2019_03_31_GIS_Time_corrected analysis/Pre-climate_data_to CCSM3/faunmapFossilDataCalibrated.csv")

speciesPleis$binomial<-paste(speciesPleis$Genus,speciesPleis$Species)

species<-unique(speciesPleis$binomial)
modern2<-modernData[modernData$name %in% species,]

modern2<-droplevels(modern2)

modern2<-data.frame(siteid=paste0(modern2$LAT,"_", modern2$LONG),
                       modern2,age=rep(0,times=nrow(modern2)) )
modern2<-unique(modern2)

#not turn it to a GIS file

pointsModern <- st_as_sf(modern2,
                      coords = c('LONG', 
                                 'LAT'),
                      crs = 4326,dim="XY")

modernPointsUS<-st_intersection(pointsModern,baseMap)

plot(modernPointsUS$geometry,pch=19,cex=0.5,col="red")

coordMap<-data.frame(st_coordinates(modernPointsUS))

landcoords<-data.frame(Lat=coordMap$Y,Long=coordMap$X)

modernOccus<-data.frame(as.data.frame(modernPointsUS)[,1:2],landcoords)


write.csv(modernOccus,"modernOccurrencesUS.csv")

modernLocalities<-unique(data.frame(siteid=paste0(modernOccus$Lat,"_", modernOccus$Long),
                                    LAT=modernOccus$Lat, LONG=modernOccus$Long,
                                    age=rep(0,times=nrow(modernOccus))))

write.csv(modernLocalities,"uniqueModernLocalitiesUS.csv")

