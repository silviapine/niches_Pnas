## Silvia Pineda-Munoz, PhD
## 11 November 2018
## GIS analysis

#http://zevross.com/blog/2015/03/30/map-and-analyze-raster-data-in-r/
library("maptools")
library("sp")
library("sf")
library("raster")
library("SDMTools")
library("rgdal")
library("foreign")
library("shapefiles")
library("RArcInfo")
library("bigmemory")

#setwd("~/Google Drive/Post-doc/Niche Spaces/GIS files/")
#readifile

landuse<-readOGR(dsn=path.expand("~/Google Drive/Post-doc/Niche Spaces/GIS files/na70_landuse/na70_lushp.shp"), 
        layer="na70_lushp")

#landuse<-readShapeSpatial("~/Google Drive/Post-doc/Niche Spaces/GIS files/na70_landuse/na70_lushp.shp")

plot(landuse)
landuse<-s

LatLongs<-data.frame(coordinates(spTransform(landuse, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))))
colnames(LatLongs)<-c("LONG", "LAT")

dataLanduse<-data.frame(siteid=c(1:nrow(LatLongs)),LAT=LatLongs$LAT,
                        LONG=LatLongs$LONG, age=rep(0,times=nrow(LatLongs)))
USdataLanduse<-data.frame(siteid=c(1:nrow(LatLongs)),LAT=LatLongs$LAT,
                        LONG=LatLongs$LONG, age=rep(0,times=nrow(LatLongs)),
                        landuse=landuse$LUC)

write.csv(dataLanduse,"landUseData_coordinates.csv")
write.csv(USdataLanduse,"USlandUseData.csv")
landuseData<-read.csv("/Users/smunoz8/Google Drive/Post-doc/Niche Spaces/Landuse_analysis/USlandUseData.csv")[-1,]
landuseData$age<-rep(1968,nrow(landuseData))
write.csv(landuseData,"/Users/smunoz8/Google Drive/Post-doc/Niche Spaces/Landuse_analysis/USlandUseData1968.csv")


###For Alaska Data

AK<-raster("~/Google Drive/Post-doc/Niche Spaces/GIS files/Alaska/ak_nlcd_2011_landcover_1_15_15.img")
AK<-raster("~/Google Drive/Post-doc/Niche Spaces/GIS files/raster/landcover.img")
#AK<-readShapeSpatial("~/Google Drive/Post-doc/Niche Spaces/GIS files/shapefiles/alaska.shp")
plot(AK)
agAK<-aggregate(AK, fact=3,fun=median)
agAK
plot(agAK)

agAK2<-aggregate(agAK, fact=3,fun=median)
agAK2
plot(agAK2)

agAK3<-aggregate(agAK2, fact=3,fun=median)
agAK3
plot(agAK3)

agAK4<-aggregate(agAK3, fact=3,fun=median)
agAK4
plot(agAK4)

# Convert raster to SpatialPointsDataFrame

AKpts <- rasterToPoints(AK, spatial=TRUE)

head(AKpts)

#AKpol <- rasterToPolygons(AK, fun=function(x){x!=1},digits=4)

#shapefile(AKpol, "polyAlaska.shp")

AKpol3 <- rasterToPolygons(agAK3, fun=function(x){x!=0})
setwd("~/Google Drive/Post-doc/Niche Spaces/GIS files/")
shapefile(AKpol3, "polyAlaska_88560.shp")

landuseMap<-spTransform(landuse, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
plot(landuseMap, col=topo.colors(17)[as.factor(landuse$LUC)],lwd=0.01,xlim=c(-180,-45))

AKpol2 <- rasterToPolygons(agAK2, fun=function(x){x!=0})
AKmaplayer<-spTransform(AKpol2, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
plot(AKmaplayer, col=topo.colors(14)[as.factor(AKpol2$landcover)],lwd=0.01,add=T)



##get and save coordinates
LatLongs<-data.frame(coordinates(spTransform(AKpol3, CRS("+proj=longlat +datum=WGS84"))))
colnames(LatLongs)<-c("LONG", "LAT")

AKdataLanduse<-data.frame(siteid=c(1:nrow(LatLongs)),LAT=LatLongs$LAT,
                        LONG=LatLongs$LONG, age=rep(0,times=nrow(LatLongs)))

AKdata<-data.frame(siteid=c(1:nrow(LatLongs)),LAT=LatLongs$LAT,
                          LONG=LatLongs$LONG, age=rep(0,times=nrow(LatLongs)),
                   landuse=AKpol3$landcover)

write.csv(AKdataLanduse,"AKlandUseData_coordinates.csv")
write.csv(AKdata,"AKlandUseData.csv")





coordAK<-coordinates(spTransform(AKpts, CRS("+proj=longlat +datum=WGS84")))

AKdata <- data.frame(easting=coordinates(coordAK)[,1],
                     northing=coordinates(coordAK)[,2],AKpts@data)    

AKdata<-droplevels(AKdata[AKdata$landcover!=0,])

plot_locations_HARV<-AKdata


plot_locations_sp_HARV<-st_as_sf(plot_locations_HARV, 
                                   coords = c("easting", "northing"), crs = "+proj=longlat +datum=WGS84")

st_crs(plot_locations_sp_HARV)

setwd("~/Google Drive/Post-doc/Niche Spaces/GIS files/")
st_write(plot_locations_sp_HARV,
         "Alaskas.shp", driver = "ESRI Shapefile")

alaskas<-readShapeSpatial("~/Google Drive/Post-doc/Niche Spaces/GIS files/Alaskas.shp")

alaskas<-readOGR(dsn=path.expand("~/Google Drive/Post-doc/Niche Spaces/GIS files/Alaskas.shp"), 
        layer="Alaskas")

plot(alaskas, col=topo.colors(17)[as.factor(alaskas$landcover)],lwd=0.01)



AKpts_data<-AKpts_data[AKpts_data$landcover!=0,]

head(AKpts_data[,2:3])

plot(AKpts_data[sample(nrow(AKpts_data),10000),2:3], pch=19,cex=0.5)

AKpts_data2<-data.frame(long=(AKpts_data$long)-180,lat=90-AKpts_data$lat)

plot(AKpts_data2[sample(nrow(AKpts_data2),5000),], pch=19,cex=0.5)

plot(AK, axes=F)

AKvalues<-values(AK)

head(data.frame(AK))
names(AK)
AK$files
attributes(AK)






localities<-read.csv("~/Google Drive/DaniSilvia/Localities_Final.csv",header=T,row.names=1)
lat_long<-data.frame(LATITUDE=localities$LATITUDE, LONGITUDE=localities$LONGITUDE)
lat_long<-na.omit(lat_long)
lat_long2<-na.omit(lat_long)
coordinates(lat_long2)<-c("LONGITUDE","LATITUDE")
WGS84<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ") #projection setup
proj4string(lat_long2) <- WGS84
lat_long2<-SpatialPointsDataFrame(lat_long2,lat_long,proj4string = WGS84)

# intersecting the two object
intersections<-intersect(lat_long2,biomes)
# for some reason it loses 6 points here. They might be points that don't fall in a biome.

results<-(intersections$BIOME)
econame<-intersections$ECO_NAME
#names(results)<-names(intersections$LATITUDE)
results2<-data.frame(locality=names(intersections$LATITUDE),biome_code=results,ECO_NAME=econame)
biomeCode<-read.csv("biome_code.csv")

results3<-merge(results2, biomeCode, by.x="biome_code", by.y= "MHT_CODE", all.x=T, all.y=F,sort=F)

write.csv(results3,"Info_biome_localities.csv")

