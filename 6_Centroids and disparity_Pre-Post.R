## Silvia Pineda-Munoz, PhD
## 11 May 2020
## Studying centroid and disparity Pre-Post-Industrial Revolution

#this fucntion helps create transparent colors for the plots
maT<-function(someColor, alpha=100)#makes transparent
{
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                              blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}

library("ecospat")
library("dispRity")
setwd("~/Google Drive/Post-doc/Niche Spaces/2019_03_31_GIS_Time_corrected analysis/datasets/Data_Analysis_files")

fossilData<-read.csv("~/Google Drive/Post-doc/Niche Spaces/2019_03_31_GIS_Time_corrected analysis/datasets/Data_Analysis_files/Contiguous_US/fossil_climate_contiguous_US.csv")

modernData<-read.csv("~/Google Drive/Post-doc/Niche Spaces/2019_03_31_GIS_Time_corrected analysis/datasets/Data_Analysis_files/Contiguous_US/modern_climate_contiguous_US.csv")

AllAnal<-rbind(fossilData,modernData)
AllAnal<-na.omit(AllAnal)
EM<-read.csv("~/Google Drive/Post-doc/Continuous_Diet/Diet_bodymass_data/MamFuncDat2_2.csv",
             header=T)
taxonomy<-read.csv("~/Google Drive/Post-doc/Niche Spaces/Taxonomy_data.csv")

#Body mass data
BMdata<-EM[,c(2,25)]
colnames(BMdata)<-c("Species","BodyMass_g")
AllAnal$AgeQual<-ifelse(AllAnal$AgeQual=="1_Up","1_Pre",
                        ifelse(AllAnal$AgeQual=="2_Low","1_Pre",
                               ifelse(AllAnal$AgeQual=="3_Pres","1_Pre","2_Post")))

#Background cliamte data
BackFossil<-read.csv("~/Google Drive/Post-doc/Niche Spaces/2019_03_31_GIS_Time_corrected analysis/datasets/Data_Analysis_files/Contiguous_US/background_fossil_contiguous_US.csv")
BackFossil<-droplevels(BackFossil[BackFossil$Age<=11650,])
AgeQualF<-ifelse(BackFossil$Age>=4200, "1_Pre",
                 ifelse(BackFossil$Age>450,"1_Pre",
                        ifelse(BackFossil$Age>=100,"1_Pre",
                               "2_Post")))
BackFossil<-data.frame(BackFossil, AgeQual=AgeQualF)

BackNow<-read.csv("~/Google Drive/Post-doc/Niche Spaces/2019_03_31_GIS_Time_corrected analysis/datasets/Data_Analysis_files/Contiguous_US/background_modern_contiguous_US.csv")

BackNow<-data.frame(BackNow,
                    AgeQual=rep("2_Post",times=nrow(BackNow)))

BackModern<-read.csv("~/Google Drive/Post-doc/Niche Spaces/2019_03_31_GIS_Time_corrected analysis/datasets/Data_Analysis_files/Contiguous_US/background_modern2_contiguous_US.csv")

BackModern<-data.frame(BackModern,
                       AgeQual=rep("1_Pre",times=nrow(BackModern)))

backData<-rbind(BackFossil,BackModern, BackNow)

BG<-unique(data.frame(tmean=backData$tmean,
                      pmean=backData$pmean,Age=backData$AgeQual))
BG<-na.omit(BG)
BG<-unique(data.frame(tmean=round(BG$tmean),pmean=round(BG$pmean),
                      Age=BG$Age))

bUp<-BG[BG$Age=="1_Pre",1:2]
bLow<-BG[BG$Age=="1_Pre",1:2]
bMod<-BG[BG$Age=="1_Pre",1:2]
bPres<-BG[BG$Age=="2_Post",1:2]
bNow<-BG[BG$Age=="2_Post",1:2]

###
###
#CREATING A NICHE SPACE FOR CLIMATE DIET
###
###

dataAnal<-data.frame(Species=AllAnal$Species,tmean=AllAnal$tmean,
                     pmean=AllAnal$pmean, Age=AllAnal$AgeQual)
dataAnal<-(unique(dataAnal))



##This does a final clean and leaves only species with min 20 specimens for Pleistocene, Holocene, and Present
subSpecies<-as.data.frame.matrix(table(data.frame(dataAnal$Species,dataAnal$Age)))
colnames(subSpecies)<-c("a","b")
table(subSpecies$Age)

speciesAnal<-read.csv("~/Google Drive/Post-doc/Niche Spaces/2019_03_31_GIS_Time_corrected analysis/Analysis_Pre_post_industrial/speciesAnalysis.csv")[,1]


dataAnal<-droplevels(dataAnal[dataAnal$Species %in% speciesAnal,])
as.data.frame.matrix(table(data.frame(dataAnal$Species,dataAnal$Age)))

affiliations<-merge(dataAnal, taxonomy, by.x="Species", 
                    by.y="Species", all.x=T, all.y=F)
taxaData<-droplevels(unique(affiliations[,c(1,6,7)]))
data.frame(table(taxaData$Order))


###
###
###CHANGE Low-Mod
###
###

setwd("~/Google Drive/Post-doc/Niche Spaces/2019_03_31_GIS_Time_corrected analysis/datasets/Results_Pre_Post/Disparities")

#data for the analysis
overallData<-(rbind(BG[,1:2],dataAnal[,2:3]))
subsetPAll<-droplevels(dataAnal[dataAnal$Age==as.character("1_Pre"),])
subsetMAll<-droplevels(dataAnal[dataAnal$Age==as.character("2_Post"),])
dataLow<-rbind(bLow[,1:2],subsetPAll[,2:3])
dataMod<-rbind(bNow[,1:2],subsetMAll[,2:3])

#Centroid and disparity
centDist<-array(dim=length(speciesAnal))
disp1<-array(dim=length(speciesAnal))
disp2<-array(dim=length(speciesAnal))
manovs<-array(dim=length(speciesAnal))
anovas<-array(dim=length(speciesAnal))
anovasSub<-array(dim=length(speciesAnal))
centP<-array(dim=length(speciesAnal))

for (i in 1:length(speciesAnal)){
  
  subsetPsp<-droplevels(subsetPAll[subsetPAll$Species==as.character(speciesAnal[i]),])
  subsetMsp<-droplevels(subsetMAll[subsetMAll$Species==as.character(speciesAnal[i]),])
  
  a1<-median(subsetPsp$tmean)
  a2<-median(subsetPsp$pmean)
  
  b1<-median(subsetMsp$tmean)
  b2<-median(subsetMsp$pmean)
  
  centDist[i]<-sqrt((abs(a1-b1))^2+(abs(a2-b2))^2)
  
  allMan<-rbind(data.frame(subsetPsp,Age=rep("2_Low",nrow(subsetPsp))),
                data.frame(subsetMsp,Age=rep("3_Mod",nrow(subsetMsp))))
  manovs[i]<-round(summary(manova(cbind(allMan[,2],allMan[,3])~allMan$Age))$stats[1,6],2)
  
  disp1[i]<-mean(data.frame(centroids(subsetPsp[,2:3]))[,1])
  disp2[i]<-mean(data.frame(centroids(subsetMsp[,2:3]))[,1])
  
  aa<-(data.frame(centroids(subsetPsp[,2:3]))[,1])
  bb<-(data.frame(centroids(subsetMsp[,2:3]))[,1])
  allAn<-rbind(data.frame(Dist=aa,Age=rep("3_Mod",length(aa))),
               data.frame(Dist=bb,Age=rep("4_Present",length(bb))))
  anovas[i]<-round(summary(aov(allAn[,1]~allAn$Age))[[1]][["Pr(>F)"]][1],4)
  
  subAnovas<-array(dim=100)
  simCent<-array(dim=100)
  
  for(j in 1:100){
    subsetPsp<-droplevels(subsetPAll[subsetPAll$Species==as.character(speciesAnal[i]),])
    subsetMsp<-droplevels(subsetMAll[subsetMAll$Species==as.character(speciesAnal[i]),])
    sizeSub<-min(nrow(subsetPsp),nrow(subsetMsp))
    
    subsetPsp<-subsetPsp[sample(nrow(subsetPsp),sizeSub),]
    subsetMsp<-subsetMsp[sample(nrow(subsetMsp),sizeSub),]
    
    subsetAll<-rbind(subsetPsp,subsetMsp)
    subsetAll<-data.frame(subsetAll[,1:3],Age=sample(subsetAll$Age))
    SsubsetPsp<-droplevels(subsetAll[subsetAll$Age=="2_Low",])
    SsubsetMsp<-droplevels(subsetAll[subsetAll$Age=="3_Mod",])
    a1<-median(SsubsetPsp$tmean)
    a2<-median(SsubsetPsp$pmean)
    b1<-median(SsubsetMsp$tmean)
    b2<-median(SsubsetMsp$pmean)
    simCent[j]<-sqrt((abs(a1-b1))^2+(abs(a2-b2))^2)
    
    aa<-(data.frame(centroids(subsetPsp[,2:3]))[,1])
    bb<-(data.frame(centroids(subsetMsp[,2:3]))[,1])
    allAn<-rbind(data.frame(Dist=aa,Age=rep("2_Low",length(aa))),
                 data.frame(Dist=bb,Age=rep("3_Mod",length(bb))))
    
    subAnovas[j]<-round(summary(aov(allAn[,1]~allAn$Age))[[1]][["Pr(>F)"]][1],4)
    
  }
  centP[i]<-length(simCent[simCent<=centDist[i]])/100
  anovasSub[i]<-mean(subAnovas)
}

CentDispPre_Post<-data.frame(speciesAnal,centDist,centP,manovs,
                            disp1,disp2,anovas, anovasSub)

write.csv(CentDispPre_Post,"CentDispPre_Post.csv")


