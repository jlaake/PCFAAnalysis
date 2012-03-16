# This is how ER dataframe was created from pcfa.txt data file
# Read in data file anc create region fields
ER=read.delim("pcfa.txt",header=TRUE,na.strings="",sep="\t",colClasses=c("numeric","factor","numeric","character","factor","factor","numeric","numeric"))
# Create date type
ER$Date=as.Date(ER$Date,"%d-%b-%y")
ER$Month=as.POSIXlt(ER$Date)$mon+1
# Add various region fields to enable easy partitioning of the data
ER$Region=as.character(ER$Sreg)
Regions=c("NCA","SOR","OR","GH+","NWA","SJF","SVI","WVI","NBC","SEAK")
ER$NCA.SEAK=0
ER$NCA.SEAK[ER$Region%in%Regions]=1
Regions=c("NCA","SOR","OR","GH+","NWA","SJF","SVI","WVI","NBC")
ER$NCA.NBC=0
ER$NCA.NBC[ER$Region%in%Regions]=1
Regions=c("SOR","OR","GH+","NWA","SJF","SVI","WVI","NBC")
ER$OR.NBC=0
ER$OR.NBC[ER$Region%in%Regions]=1
Regions=c("SOR","OR","GH+","NWA","SJF","SVI")
ER$OR.SVI=0
ER$OR.SVI[ER$Region%in%Regions]=1
Regions=c("SOR","OR","GH+","NWA","SJF","SVI","WVI")
ER$OR.WVI=0
ER$OR.WVI[ER$Region%in%Regions]=1
ER$MUA=0
ER$MUA[ER$Region=="NWA"|ER$Region=="SJF"]=1
ER$MUA.SVI=0
ER$MUA.SVI[ER$MUA==1|ER$Region=="SVI"]=1
ER$AK=0
ER$AK[ER$Region%in%c("KAK","SEAK","NAK")]=1
ERx=ER[!is.na(ER$Month)&!ER$Region%in%c("SJI","PS-HC-BB-SJ","NPS","SEAK","CCA","SCA","NAK"),]
springwhales=unique(ERx$ID[ERx$Year>=1998&ERx$Month%in%c(3,4,5)])
ER$spring=0
ER$spring[ER$ID%in%springwhales]=1
# Add a field to indicate whales seen in spring in NWA
springwhales=unique(ERx$ID[ERx$Year>=1998&ERx$Month%in%c(3,4,5)&ERx$Region=="NWA"])
ER$springNWA=0
ER$springNWA[ER$ID%in%springwhales]=1
# Add a field to indicate whales seen in spring in SJF
springwhales=unique(ERx$ID[ERx$Year>=1998&ERx$Month%in%c(3,4,5)&ERx$Region=="SJF"])
ER$springSJF=0
ER$springSJF[ER$ID%in%springwhales]=1
names(ER)[names(ER)=="Region"]="region"
ER$Region=ER$region
ER$Region[ER$Region%in%c("NPS","PS-HC-BB-SJ","SJI")]="NPS"
ER$Region=factor(ER$Region,levels=c("SCA","CCA","NCA","SOR","OR","GH+","NWA","SJF","NPS","SVI","WVI","NBC","SEAK","KAK","NAK"))