## Silvia Pineda-Munoz, PhD
## 3 Octover 2019
## Pre vs. post industrial

#this fucntion helps create transparent colors for the plots
maT<-function(someColor, alpha=100)#makes transparent
{
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                              blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}

library("ecospat")
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

setwd("~/Google Drive/Post-doc/Niche Spaces/2019_03_31_GIS_Time_corrected analysis/datasets/Results_Pre_Post/Overlap_results_D")

#data for the analysis
overallData<-(rbind(BG[,1:2],dataAnal[,2:3]))
subsetPAll<-droplevels(dataAnal[dataAnal$Age==as.character("1_Pre"),])
subsetMAll<-droplevels(dataAnal[dataAnal$Age==as.character("2_Post"),])
dataLow<-rbind(bLow[,1:2],subsetPAll[,2:3])
dataMod<-rbind(bNow[,1:2],subsetMAll[,2:3])

##Arrays necessary
Overlap<-array(dim=length(speciesAnal))
similSp<-array(dim=length(speciesAnal))

equivSpModer<-array(dim=length(speciesAnal))

persInSp<-array(dim=length(speciesAnal))

Overlap<-array(dim=length(speciesAnal))
Expansion<-array(dim=length(speciesAnal))
Stability<-array(dim=length(speciesAnal))
Unfilling<-array(dim=length(speciesAnal))
ExpansionSp<-array(dim=length(speciesAnal))
StabilitySp<-array(dim=length(speciesAnal))
UnfillingSp<-array(dim=length(speciesAnal))

for (i in 1:length(speciesAnal)){
  
  subsetPsp<-droplevels(subsetPAll[subsetPAll$Species==as.character(speciesAnal[i]),])
  subsetMsp<-droplevels(subsetMAll[subsetMAll$Species==as.character(speciesAnal[i]),])
  
  gridLow<-ecospat.grid.clim.dyn (glob=overallData, 
                                  glob1=dataLow,
                                  sp=subsetPsp[,2:3], R=50)
  
  gridMod<-ecospat.grid.clim.dyn (glob=overallData, 
                                  glob1=dataMod,
                                  sp=subsetMsp[,2:3], R=50)
  nulData<-data.frame(pmean=seq(0,5,0.2),tmean=seq(10,15,0.2))
  
  baseData<-ecospat.grid.clim.dyn (glob=overallData, 
                                   glob1=nulData,
                                   sp=nulData, R=50)
  
  Sim<-ecospat.niche.similarity.test(gridLow,gridMod,100)
  similSp[i]<-Sim$p.D
  
  EqL<-ecospat.niche.equivalency.test(gridLow,gridMod,100,"lower")
  equivSpModer[i]<-EqL$p.D
  
  Overlap[i]<-ecospat.niche.overlap(gridLow,gridMod,cor=T)$D
  hola<-ecospat.niche.dyn.index (gridLow, gridMod, intersection=NA)
  Expansion[i]<-hola$dynamic.index.w[1]
  Stability[i]<-hola$dynamic.index.w[2]
  Unfilling[i]<-hola$dynamic.index.w[3]
  
  
  SOverlap<-array(dim=100)
  SsOverlap<-array(dim=100)
  SExpansion<-array(dim=100)
  SStability<-array(dim=100)
  SUnfilling<-array(dim=100)
  
  for(j in 1:100){
    subsetPsp<-droplevels(subsetPAll[subsetPAll$Species==as.character(speciesAnal[i]),])
    subsetMsp<-subsetMAll[subsetMAll$Species==as.character(speciesAnal[i]),]
    
    #sizeSub<-min(nrow(subsetPsp),nrow(subsetMsp))
    #subsetPsp<-subsetPsp[sample(nrow(subsetPsp),sizeSub),]
    #subsetMsp<-subsetMsp[sample(nrow(subsetMsp),sizeSub),]
    
    
    subsetAll<-rbind(subsetPsp,subsetMsp)
    subsetAll<-data.frame(subsetAll[,1:3],Age=sample(subsetAll$Age))
    SsubsetPsp<-droplevels(subsetAll[subsetAll$Age=="1_Pre",])
    SsubsetMsp<-droplevels(subsetAll[subsetAll$Age=="2_Post",])
    
    Psp<-(rbind(dataLow,SsubsetPsp[,2:3]))
    Msp<-(rbind(dataMod,SsubsetMsp[,2:3]))
    
    SgridLow<-ecospat.grid.clim.dyn (glob=overallData, 
                                     glob1=Psp,
                                     sp=SsubsetPsp[,2:3], R=50)
    SgridMod<-ecospat.grid.clim.dyn (glob=overallData, 
                                     glob1=Msp,
                                     sp=SsubsetMsp[,2:3], R=50)
    
    
    SsOverlap[j]<-ecospat.niche.overlap(SgridLow,SgridMod,cor=T)$D
    hola<-ecospat.niche.dyn.index (SgridLow, SgridMod, intersection=NA)
    SExpansion[j]<-hola$dynamic.index.w[1]
    SStability[j]<-hola$dynamic.index.w[2]
    SUnfilling[j]<-hola$dynamic.index.w[3]
  }
  #Overlap[i]<-mean(SOverlap)
  persInSp[i]<-length(SsOverlap[SsOverlap<=Overlap[i]])/100
  
  ExpansionSp[i]<-length(SExpansion[SExpansion<=Expansion[i]])/100
  
  StabilitySp[i]<-length(SStability[SStability<=Stability[i]])/100
  
  UnfillingSp[i]<-length(SUnfilling[SUnfilling<=Unfilling[i]])/100
  
  ##Plotting
  subsetPsp<-droplevels(subsetPAll[subsetPAll$Species==as.character(speciesAnal[i]),])
  subsetMsp<-subsetMAll[subsetMAll$Species==as.character(speciesAnal[i]),]
  
  gridLow<-ecospat.grid.clim.dyn (glob=overallData, 
                                  glob1=dataLow,
                                  sp=subsetPsp[,2:3], R=50)
  
  gridMod<-ecospat.grid.clim.dyn (glob=overallData, 
                                  glob1=dataMod,
                                  sp=subsetMsp[,2:3], R=50)
  nulData<-data.frame(pmean=seq(0,5,0.2),tmean=seq(10,15,0.2))
  
  baseData<-ecospat.grid.clim.dyn (glob=overallData, 
                                   glob1=nulData,
                                   sp=nulData, R=50)
  
  
  
  mypath <- file.path(paste0("Overlap_landuses_",
                             as.character(speciesAnal[i]), ".pdf"))
  pdf(file=mypath,width = 5, height =5)
  par(mfrow=c(2,2), mai = c(0.65, 0.65, 0.3, 0.04))
  #3Mod
  ecospat.plot.niche.dyn(z1=gridLow,z2=baseData,quant=1,interest=1,
                         title="Low", name.axis1 = "",
                         name.axis2 = "",colz2 = maT(colors()[1],alpha=1),
                         colZ2 = maT(colors()[1],alpha=1),colz1 = maT(colors()[473],alpha=1),
                         colZ1 = maT(colors()[473]),colinter=maT(colors()[1],alpha=1))
  title(xlab="Temperature", ylab="Precipitation", 
        line=2.1, cex.lab=1)
  text(-10,225,speciesAnal[i],font=3)
  text(-10,150,paste0("N=",nrow(subsetPsp)),font=3)
  #4Present
  ecospat.plot.niche.dyn(z1=gridMod,z2=baseData,quant=1,interest=1,
                         title="Mod", name.axis1 = "",
                         name.axis2 = "",colz2 = maT(colors()[1],alpha=1),
                         colZ2 = maT(colors()[1],alpha=1),colz1 = maT(colors()[500],alpha=1),
                         colZ1 = maT(colors()[500]),colinter=maT(colors()[1],alpha=1))
  title(xlab="Temperature", ylab="Precipitation", 
        line=2.1, cex.lab=1)
  text(-10,225,speciesAnal[i],font=3)
  text(-10,150,paste0("N=",nrow(subsetMsp)),font=3)
  
  ecospat.plot.overlap.test (EqL, "D","Equivalency lower")
  ecospat.plot.overlap.test (Sim, "D", "Similarity")
  
  dev.off()
}

resultsAnalyLow_Mod<-data.frame(speciesAnal,
                                Overlap,persInSp,similSp,
                                equivSpModer,
                                Expansion,ExpansionSp,
                                Stability,StabilitySp,
                                Unfilling,UnfillingSp)


write.csv(resultsAnalyLow_Mod,"Similarity_Pre-Post.csv")




#### extra analysis for SI
overallData<-(rbind(BG[,1:2],dataAnal[,2:3]))
subsetPAll<-droplevels(dataAnal[dataAnal$Age==as.character("1_Pre"),])
subsetMAll<-droplevels(dataAnal[dataAnal$Age==as.character("2_Post"),])
dataLow<-rbind(bLow[,1:2],subsetPAll[,2:3])
dataMod<-rbind(bNow[,1:2],subsetMAll[,2:3])


Overlap<-array(dim=length(speciesAnal))
sdOverlap<-array(dim=length(speciesAnal))

setwd("~/Google Drive/Post-doc/Niche Spaces/2019_03_31_GIS_Time_corrected analysis/datasets/Results_Overlap_subsample/Overlaps with SD")

for (i in 1:length(speciesAnal)){
  
  SOverlap<-array(dim=100)
  
  for(j in 1:100){
    subsetPsp<-droplevels(subsetPAll[subsetPAll$Species==as.character(speciesAnal[i]),])
    subsetMsp<-droplevels(subsetMAll[subsetMAll$Species==as.character(speciesAnal[i]),])
    
    sizeSub<-min(nrow(subsetPsp),nrow(subsetMsp))
    subsetPsp<-subsetPsp[sample(nrow(subsetPsp),sizeSub),]
    subsetMsp<-subsetMsp[sample(nrow(subsetMsp),sizeSub),]
    
    
    Psp<-(rbind(dataLow,subsetPsp[,2:3]))
    Msp<-(rbind(dataMod,subsetMsp[,2:3]))
    
    SgridLow<-ecospat.grid.clim.dyn (glob=overallData, 
                                     glob1=Psp,
                                     sp=subsetPsp[,2:3], R=50)
    SgridMod<-ecospat.grid.clim.dyn (glob=overallData, 
                                     glob1=Msp,
                                     sp=subsetMsp[,2:3], R=50)
    
    
    SOverlap[j]<-ecospat.niche.overlap(SgridLow,SgridMod,cor=T)$D
  }
  
  Overlap[i]<-mean(SOverlap)
  sdOverlap[i]<-sd(SOverlap)
}

resultsAnalyPrePost<-data.frame(speciesAnal,
                                Overlap, sdOverlap)

write.csv(resultsAnalyPrePost,"Similarity_Pre-Post.csv")