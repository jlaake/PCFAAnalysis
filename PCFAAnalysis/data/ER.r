# This is how ER dataframe was created from pcfa.txt data file
# Read in data file anc create region fields
ER=utils::read.delim("pcfa.txt",header=TRUE,na.strings="",sep="\t",colClasses=c("numeric","factor","numeric","character","factor","factor","numeric","numeric"))
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
#springwhales=unique(ERx$ID[ERx$Year>=1996&ERx$Month%in%c(3,4,5,6)])
springwhales=unique(ERx$ID[ERx$Year>=1996&ERx$Month%in%c(3,4,5)])
ER$spring=0
ER$spring[ER$ID%in%springwhales]=1
# Add a field to indicate whales seen in spring in NWA
#springwhales=unique(ERx$ID[ERx$Year>=1996&ERx$Month%in%c(3,4,5,6)&ERx$Region=="NWA"])
springwhales=unique(ERx$ID[ERx$Year>=1996&ERx$Month%in%c(3,4,5)&ERx$Region=="NWA"])
ER$springNWA=0
ER$springNWA[ER$ID%in%springwhales]=1
# Add a field to indicate whales seen in spring in SJF
#springwhales=unique(ERx$ID[ERx$Year>=1996&ERx$Month%in%c(3,4,5,6)&ERx$Region=="SJF"])
springwhales=unique(ERx$ID[ERx$Year>=1996&ERx$Month%in%c(3,4,5)&ERx$Region=="SJF"])
ER$springSJF=0
ER$springSJF[ER$ID%in%springwhales]=1
names(ER)[names(ER)=="Region"]="region"
ER$Region=ER$region
ER$Region[ER$Region%in%c("NPS","PS-HC-BB-SJ","SJI")]="NPS"
ER$Region=factor(ER$Region,levels=c("MX","SCA","CCA","NCA","SOR","OR","GH+","NWA","SJF","NPS","SVI","WVI","NBC","SEAK","KAK","NAK"))
# add calf data; note that starting through 2017 this is the first date seen and last date seen regardless of mom presence as done previously
# also no mom id attached here.
calves=utils::read.delim("CalfMomData.txt",colClasses=c("numeric","factor","numeric","numeric",rep("character",3)))
calves$FirstDate=as.Date(calves$FirstDate,"%d-%b-%y")
calves$LastDate=as.Date(calves$LastDate,"%d-%b-%y")
calves$CalfAloneDate=as.Date(calves$CalfAloneDate,"%d-%b-%y")
names(calves)[names(calves)%in%"Year"]="Calf"
ER=merge(ER,calves,by.x="ID",by.y="CalfID",all.x=TRUE)
ER$Calf[is.na(ER$Calf)]=0


