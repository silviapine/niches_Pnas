## Silvia Pineda-Munoz, PhD
## April 2 2019
## Preparing occurrence data (fossil + extanct)

library("sm")
setwd("~/Google Drive/Post-doc/Niche Spaces/2019_03_31_GIS_Time_corrected analysis/datasets")

#allData<-read.csv("~/Google Drive/Post-doc/Niche Spaces/occu_and_climate_data.csv")
#cols<-names(allData)
sensi<-read.csv("~/Google Drive/Post-doc/Niche Spaces/2019_03_31_GIS_Time_corrected analysis/datasets/Sensitivity_analysis/results_sensitivity_anal.csv")
length(unique(sensi$siteID))
#Fossil Data
fossilData<-read.csv("~/Google Drive/Post-doc/Niche Spaces/2019_03_31_GIS_Time_corrected analysis/datasets/Occurrences_data/faunmapFossilDataCalibratedPleis.csv")[,-1]
length(unique(fossilData$siteid))
fossilData<-na.omit(fossilData)
#fossilData<-fossilData[fossilData$Species!="   ",]
fossilData<-fossilData[-unique(grep("sp.", fossilData$Species, value=F,fixed=T)),] #I removed indetermined species
fossilData$Genus<-gsub(" ","",fossilData$Genus)
fossilData<-fossilData[fossilData$Species!="   ",] #I removed blanked species
fossilData<-fossilData[fossilData$AvAge<=22000,]
length(unique(fossilData$siteid))

fossilData$Species<-paste(fossilData$Genus,fossilData$Species)
fossilData<-subset(fossilData, select=-c(Genus,MinAge, MaxAge, AvAge))
climateFossil<-read.csv("~/Google Drive/Post-doc/Niche Spaces/2019_03_31_GIS_Time_corrected analysis/datasets/FossilClimate/faunmapLocalitiesPleis_climate.csv")[,-1]
climateFossil<-climateFossil[,c(1,2,5:10)]
length(unique(climateFossil$siteid))

fossilOcClim<-merge(fossilData,climateFossil,
                    by.x="siteid",by.y="siteid",
                    all.x=T,all.y=F)
length(unique(fossilOcClim$siteid))


subsetFossil<-fossilOcClim[fossilOcClim$siteid %in% sensi$siteID,]## just the ones that passed the sensitivity test
#subsetFossil<-subset(subsetFossil, select=-c(X))
length(unique(subsetFossil$siteid))

AgeQualF<-ifelse(subsetFossil$Age>=4200, "1_Up",
                 ifelse(subsetFossil$Age>450,"2_Low",
                        ifelse(subsetFossil$Age>=100,"3_Mod",
                               "4_Pres")))

subsetFossil<-droplevels(data.frame(subsetFossil,AgeQual=AgeQualF))

write.csv(subsetFossil, "~/Google Drive/Post-doc/Niche Spaces/2019_03_31_GIS_Time_corrected analysis/datasets/Data_Analysis_files/fossil_data_with_climate.csv")

##Modern Data
taxonomy<-data.frame(unique(subsetFossil[,2:5]))
modernData<-read.csv("~/Google Drive/Post-doc/Niche Spaces/2019_03_31_GIS_Time_corrected analysis/datasets/Occurrences_data/modernOccurrencesContiUS.csv")[,-1]

modernTaxa<-merge(modernData,taxonomy,
                  by.x="name",by.y="Species",
                  all.x=F,all.y=F)

modernTaxa<-data.frame(modernTaxa[,c(2,6,7,8)],Species=modernTaxa$name,
                       LAT=modernTaxa$Lat,LONG=modernTaxa$Long,
                       Age=modernTaxa$age)
climateModern<-read.csv("~/Google Drive/Post-doc/Niche Spaces/2019_03_31_GIS_Time_corrected analysis/datasets/ModernClimate/uniqueModernLocalitiesUS_Climate.csv")[,-1]
climateModern<-climateModern[,c(1,5:10)]

subsetModern<-merge(modernTaxa,climateModern,
                    by.x="siteid",by.y="siteid",
                    all.x=T,all.y=F)

subsetFossil<-read.csv("~/Google Drive/Post-doc/Niche Spaces/2019_03_31_GIS_Time_corrected analysis/datasets/Data_Analysis_files/fossil_data_with_climate.csv")[,-1]

subsetModern<-na.omit(subsetModern)

AgeQual<-ifelse(subsetModern$Age>=1950,
                "5_Now",
                ifelse(subsetModern$Age>=1850,
                       "4_Pres","3_Mod"))


subsetModern<-droplevels(data.frame(subsetModern,AgeQual))
names(subsetModern)<-names(subsetFossil)

write.csv(subsetModern, "~/Google Drive/Post-doc/Niche Spaces/2019_03_31_GIS_Time_corrected analysis/datasets/Data_Analysis_files/modern_data_with_climate.csv")



