## Silvia Pineda-Munoz, PhD
## 2 April 2019
## Sensitivity analysis US localities

setwd("~/Google Drive/Post-doc/Niche Spaces/2019_03_31_GIS_Time_corrected analysis/datasets")

locs<-read.csv("FossilClimate/sesitivityAnalData_climate.csv")
head(locs)
length(unique(locs$oldSiteId))

locs<-na.omit(locs)
length(unique(locs$oldSiteId))

par(mfrow=c(1,1), mai = c(0.4, 0.4, 0.1, 0.04))
plot(locs$LONG,locs$LAT)

sites<-unique(locs$oldSiteId)
rangeTemp<-array(dim=length(sites))
rangePrec<-array(dim=length(sites))
rangeAge<-array(dim=length(sites))

for (i in 1:length(sites)){
  subs<-locs[locs$oldSiteId==sites[i],]
  rangeTemp[i]<-max(subs$tmean,na.rm=T)-min(subs$tmean,na.rm=T)
  rangePrec[i]<-max(subs$pmean,na.rm=T)-min(subs$pmean,na.rm=T)
  rangeAge[i]<-max(subs$age,na.rm=T)-min(subs$age,na.rm=T)
}

sensiAnal<-data.frame(siteID=sites, TempRange=rangeTemp,
                      PrecRange=rangePrec,AgeRange=rangeAge)

par(mfrow=c(2,1), mai = c(0.4, 0.4, 0.1, 0.04))
hist(rangeTemp, breaks=length(unique(round(rangeTemp))))
hist(rangePrec, breaks=length(unique(round(rangePrec))))

lowAgeRange<-sensiAnal[sensiAnal$AgeRange<100,]
mean(lowAgeRange$TempRange)

plot(rangeAge,rangeTemp)
abline(h=3,col="red")
abline(h=2,col="red")

OPrec<-rangePrec[order(rangePrec)]
OTemp<-rangeTemp[order(rangeTemp)]

OPrec[(length(OPrec)*80)/100]
length(OPrec[OPrec<=25])/length(OPrec)

OTemp[(length(OTemp)*80)/100]
length(OTemp[OTemp<=3])/length(OTemp)

goodSensi<-droplevels(sensiAnal[sensiAnal$TempRange<3,])
goodSensi<-droplevels(goodSensi[goodSensi$PrecRange<25,])

length(unique(goodSensi$siteID))

write.csv(goodSensi,"results_sensitivity_anal.csv")

aaa<-read.csv("~/Google Drive/Post-doc/Niche Spaces/2019_03_31_GIS_Time_corrected analysis/datasets/Sensitivity_analysis/results_sensitivity_anal.csv")
head(aaa)
length(unique(goodSensi$siteID))
