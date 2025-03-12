## Silvia Pineda-Munoz, PhD
## March 5 2019
## landuse analysis
#this fucntion helps create transparent colors for the plots
maT<-function(someColor, alpha=100)#makes transparent
{
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                              blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}

library("ecospat")
setwd("C:/Users/smunoz8/Google Drive/Post-doc/Niche Spaces/2019_03_31_GIS_Time_corrected analysis/datasets/Results_Analysis/Landuse_Results/")
##Continuous US data
USlanduse<-read.csv("C:/Users/smunoz8/Google Drive/Post-doc/Niche Spaces/Landuse_analysis/USlandUseData1968.csv")
USclimate<-read.csv("C:/Users/smunoz8/Google Drive/Post-doc/Niche Spaces/Landuse_analysis/USlandUseData1968_climate.csv")[,c(3,8:13)]
UScode<-read.csv("C:/Users/smunoz8/Google Drive/Post-doc/Niche Spaces/Landuse_analysis/LUC_code.csv")

USdata<-merge(USlanduse,USclimate,by.x="siteid",by.y="siteid")
USdata<-merge(USdata,UScode,by.x="landuse",
              by.y="LUC.ID",all.x=T,all.y=F)
USdataAnal<-data.frame(siteid=paste0("US_",USdata$siteid), LAT=USdata$LAT,
                       LON=USdata$LONG,age=USdata$age,
                       tmin=USdata$tmin,tmax=USdata$tmax,
                       tmean=USdata$tmean,pmin=USdata$pmin,pmax=USdata$pmax,
                       pmean=USdata$pmean,Landuse=USdata$LUC,
                       Landuse2=USdata$Mine)


AllDataLand<-USdataAnal


###Species Data
fossilData<-read.csv("C:/Users/smunoz8/Google Drive/Post-doc/Niche Spaces/2019_03_31_GIS_Time_corrected analysis/datasets/Data_Analysis_files/Contiguous_US/fossil_climate_contiguous_US.csv")

modernData<-read.csv("C:/Users/smunoz8/Google Drive/Post-doc/Niche Spaces/2019_03_31_GIS_Time_corrected analysis/datasets/Data_Analysis_files/Contiguous_US/modern_climate_contiguous_US.csv")

AllAnal<-rbind(fossilData,modernData)
AllAnal<-na.omit(AllAnal)
EM<-read.csv("C:/Users/smunoz8/Google Drive/Post-doc/Continuous_Diet/Diet_bodymass_data/MamFuncDat2_2.csv",
             header=T)
taxonomy<-read.csv("C:/Users/smunoz8/Google Drive/Post-doc/Niche Spaces/Taxonomy_data.csv")

#Body mass data
BMdata<-EM[,c(2,25)]
colnames(BMdata)<-c("Species","BodyMass_g")
AllAnal$AgeQual<-ifelse(AllAnal$AgeQual=="1_Up","1_Pre",
                        ifelse(AllAnal$AgeQual=="2_Low","1_Pre",
                               ifelse(AllAnal$AgeQual=="3_Pres","1_Pre","2_Post")))

#Background cliamte data
BackFossil<-read.csv("C:/Users/smunoz8/Google Drive/Post-doc/Niche Spaces/2019_03_31_GIS_Time_corrected analysis/datasets/Data_Analysis_files/Contiguous_US/background_fossil_contiguous_US.csv")
BackFossil<-droplevels(BackFossil[BackFossil$Age<=11650,])
AgeQualF<-ifelse(BackFossil$Age>=4200, "1_Pre",
                 ifelse(BackFossil$Age>450,"1_Pre",
                        ifelse(BackFossil$Age>=100,"1_Pre",
                               "2_Post")))
BackFossil<-data.frame(BackFossil, AgeQual=AgeQualF)

BackNow<-read.csv("C:/Users/smunoz8/Google Drive/Post-doc/Niche Spaces/2019_03_31_GIS_Time_corrected analysis/datasets/Data_Analysis_files/Contiguous_US/background_modern_contiguous_US.csv")

BackNow<-data.frame(BackNow,
                    AgeQual=rep("2_Post",times=nrow(BackNow)))

BackModern<-read.csv("C:/Users/smunoz8/Google Drive/Post-doc/Niche Spaces/2019_03_31_GIS_Time_corrected analysis/datasets/Data_Analysis_files/Contiguous_US/background_modern2_contiguous_US.csv")

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
table(subSpecies)

speciesAnal<-read.csv("C:/Users/smunoz8/Google Drive/Post-doc/Niche Spaces/2019_03_31_GIS_Time_corrected analysis/Analysis_Pre_post_industrial/speciesAnalysis.csv")[,1]


dataAnal<-droplevels(dataAnal[dataAnal$Species %in% speciesAnal,])
affiliations<-merge(dataAnal, taxonomy, by.x="Species", 
                    by.y="Species", all.x=T, all.y=F)
taxaData<-droplevels(unique(affiliations[,c(1,6,7)]))
data.frame(table(taxaData$Order))




###
### Analysing each landuse vs Time
###
head(AllDataLand)
AllDataLand<-na.omit(AllDataLand)
Landuses<-levels(AllDataLand$Landuse2)

Landuses<-Landuses[c(1,2,3,5,6,14)]

AllDataLand<-droplevels(AllDataLand[AllDataLand$Landuse2 %in%Landuses,])

AllDataLand<-droplevels(data.frame(Species=AllDataLand$Landuse2,tmean=AllDataLand$tmean,
                                   pmean=AllDataLand$pmean, Age=rep("Landuse",nrow(AllDataLand))))





###
###
###
##Post
###
###
###

dir.create(paste0("C:/Users/smunoz8/Google Drive/Post-doc/Niche Spaces/2019_03_31_GIS_Time_corrected analysis/datasets/Results_Pre_Post/Landuse/Post"))
setwd(paste0("C:/Users/smunoz8/Google Drive/Post-doc/Niche Spaces/2019_03_31_GIS_Time_corrected analysis/datasets/Results_Pre_Post/Landuse/Post"))


overallData<-rbind(bLow[,1:2],dataAnal[,2:3],AllDataLand[,2:3])
subsetPAll<-droplevels(dataAnal[dataAnal$Age==as.character("2_Post"),])

bNow2<-data.frame(Species=rep("Background",times=nrow(bNow)),
                   bNow,Age=rep("Background",times=nrow(bNow)))
subsetMAll<-rbind(AllDataLand)

dataLow<-rbind(bNow[,1:2],subsetPAll[,2:3])
dataLand<-rbind(bNow[,1:2],subsetMAll[,2:3])

##Arrays necessary
Overlap<-array(dim=length(speciesAnal))

for (k in 1:length(Landuses)){
  dir.create(paste0("C:/Users/smunoz8/Google Drive/Post-doc/Niche Spaces/2019_03_31_GIS_Time_corrected analysis/datasets/Results_Pre_Post/Landuse/Post/",Landuses[k]))
  setwd(paste0("C:/Users/smunoz8/Google Drive/Post-doc/Niche Spaces/2019_03_31_GIS_Time_corrected analysis/datasets/Results_Pre_Post/Landuse/Post/",Landuses[k]))
  for (i in 1:length(speciesAnal)){
    
    subsetPsp<-droplevels(subsetPAll[subsetPAll$Species==as.character(speciesAnal[i]),])
    subsetMsp<-droplevels(AllDataLand[AllDataLand$Species==Landuses[k],] )
    
    
    gridLow<-ecospat.grid.clim.dyn (glob=overallData, 
                                    glob1=dataLow,
                                    sp=subsetPsp[,2:3], R=50)
    
    gridLand<-ecospat.grid.clim.dyn (glob=overallData, 
                                     glob1=dataLand,
                                     sp=subsetMsp[,2:3], R=50)
    nulData<-data.frame(pmean=seq(0,5,0.2),tmean=seq(10,15,0.2))
    
    baseData<-ecospat.grid.clim.dyn (glob=overallData, 
                                     glob1=nulData,
                                     sp=nulData, R=50)
    
    Soverlap<-array(dim=100)
  
    for(j in 1:100){
      subsetPsp<-droplevels(subsetPAll[subsetPAll$Species==as.character(speciesAnal[i]),])
      subsetMsp<-droplevels(AllDataLand[AllDataLand$Species==Landuses[k],] )
      sizeSub<-min(nrow(subsetPsp),nrow(subsetMsp))
      subsetPsp<-subsetPsp[sample(nrow(subsetPsp),sizeSub),]
      subsetMsp<-subsetMsp[sample(nrow(subsetMsp),sizeSub),]
      
      Psp<-(rbind(dataLow,subsetPsp[,2:3]))
      Msp<-(rbind(dataLand,subsetMsp[,2:3]))
      
      SgridLow<-ecospat.grid.clim.dyn (glob=overallData, 
                                       glob1=Psp,
                                       sp=subsetPsp[,2:3], R=50)
      SgridLand<-ecospat.grid.clim.dyn (glob=overallData, 
                                        glob1=Msp,
                                        sp=subsetMsp[,2:3], R=50)
      
      
      Soverlap[j]<-ecospat.niche.overlap(SgridLow,SgridLand,cor=T)$D
      
    }
    
    Overlap[i]<-mean(Soverlap)
    
    
  }
  
  resultsAnalyLow<-data.frame(speciesAnal,
                              Overlap)
  
  
  write.csv(resultsAnalyLow,paste0("Similarity_Post",
                                   Landuses[k],".csv"))
}

