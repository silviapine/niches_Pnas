## Silvia Pineda-Munoz, PhD
## 31 March 2019
## Getting corrected dates

# Code modified from Dr. Danielle Fraser

setwd("~/Google Drive/Post-doc/Niche Spaces/2019_03_31_GIS_Time_corrected analysis/datasets/Faunmap")

species<-read.csv("Species_occurrences.csv")
#species<-unique(species)
#species$Site<-paste0(as.character(species$AnalysisUnit),"_",as.character(species$Machine.Number))

localities<-read.csv("localities_LPtoHolo.csv")
#localities<-localities[localities$Epoch %in% c("HOLO","HOPL","LPLE"),]

#localities$Site<-paste0(as.character(localities$Analysis.unit),"_",as.character(localities$Machine.Number))

faunmapData<-merge(species,localities, by.x="Site", by.y="Site",#this merges occurrence data with locality data
                all.x=F,all.y=F)

faunmapData$MinimumAge<-faunmapData$MinimumAge*1000000
faunmapData$MaximumAge<-faunmapData$MaximumAge*1000000
faunmapData$minID<-paste0(as.character(faunmapData$Site),"_",as.character(faunmapData$MinimumAge))
faunmapData$maxID<-paste0(as.character(faunmapData$Site),"_",as.character(faunmapData$MaximumAge))

absolutef<-read.csv("absoluteF.csv")
absolutef$Associated.date<-absolutef$Associated.date*1000000
absolutef$Standard.deviation <- absolutef$Standard.deviation*1000000

absolutef$Site<-paste0(as.character(absolutef$Analysis.unit),"_",as.character(absolutef$Machine.Number))

absolutef$minID<-paste0(as.character(absolutef$Site),"_",as.character(absolutef$Associated.date))
absolutef$maxID<-paste0(as.character(absolutef$Site),"_",as.character(absolutef$Associated.date))

# Calibrate radiocarbon dates
# Calibrate all of the dates then figure out how to summarize them

dates <- absolutef$Associated.date # as a whole number, it doesn't like decimals
names(dates) <- absolutef$minID
dates<-dates[dates>71]
dates<-dates[dates<46401] # get rid of things that can't be calibrated

absolutef$Standard.deviation[is.na(absolutef$Standard.deviation)] <- 100 # for the NAs, add a 100 year error

stdv <- absolutef$Standard.deviation # stdev as whole number also 
names(stdv) <- absolutef$minID
stdv<-stdv[names(dates)]


calb <- vector(length = length(dates))
calb[1:length(calb)] <- c('intcal13') # Tell what calibration curve to use

library(Bchron)
ages <- BchronCalibrate (ages = dates, ageSds = stdv, calCurves = calb, ids = names(dates)) # calibrate the dates

newages<-summary(ages)

# Basically summarizing the distribution of calibrated dates for each C14 date
age_samples<-sampleAges(ages,n_samp=20000)
newages<-apply(age_samples, 2, quantile, prob=c(0.025,0.975))
meanages<-apply(age_samples, 2, median)
#meanages<-data.frame(meanages) # doesn't have Kate's modern data

agesMin<-unique(data.frame(nameUnit=names(dates),
                    calibMin=meanages))
agesMax<-unique(data.frame(nameUnit=names(dates),
                    calibMax=meanages))
  
#I now have the calibrated ages "meanages"
# and I have the basic dataset faunmapData


head(faunmapData)
nrow(faunmapData)
#head(corAges)

withMin<-merge(faunmapData,agesMin,by.x="minID",by.y="nameUnit",
               all.x=F,all.y=F)
head(withMin)
nrow(withMin)
withMax<-merge(withMin,agesMax,by.x="maxID",by.y="nameUnit",
               all.x=F,all.y=F)
head(withMax)
nrow(withMax)

allFossils<-withMax

allFossils$MinAgeCal<-allFossils$calibMin
allFossils$MinAgeCal[is.na(allFossils$MinAgeCal)]<-allFossils$MinimumAge[is.na(allFossils$MinAgeCal)]#if it's not calibrated keep the old value

allFossils$MaxAgeCal<-allFossils$calibMax
allFossils$MaxAgeCal[is.na(allFossils$MaxAgeCal)]<-allFossils$MaximumAge[is.na(allFossils$MaxAgeCal)]

write.csv(allFossils,"Calibrated_ages.csv")

nrow(read.csv("/Users/smunoz8/Google Drive/Post-doc/Niche Spaces/2019_03_31_GIS_Time_corrected analysis/datasets/Calibrated_ages.csv"))

combined<-read.csv("/Users/smunoz8/Google Drive/Post-doc/Niche Spaces/2019_03_31_GIS_Time_corrected analysis/datasets/Calibrated_ages.csv")
###