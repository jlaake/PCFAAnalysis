\docType{data}
\name{ER}
\alias{ER}
\title{Photo-identification data for gray whales from California to Kodiak Alaska}
\format{A data frame with observations (a photo-identification) on the
following 28 variables.  \describe{
\item{list("ID")}{a numeric vector of CRC gray whale IDs}
\item{list("ResearchGroup")}{research group that contributed the photo}
\item{list("Year")}{a numeric vector of years photos were taken}
 \item{list("Date")}{vector of dates photos were taken}
\item{list("Sreg")}{a region factor with levels \code{CCA} \code{GH+}
\code{KAK} \code{NBC} \code{NCA} \code{NPS} \code{NWA} \code{OR}
\code{PS-HC-BB-SJ} \code{SCA} \code{SEAK} \code{SJF} \code{SOR} \code{SVI}
\code{WVI}}
\item{list("Reg")}{a numeric region factor with levels \code{1}
\code{10} \code{11} \code{12} \code{13} \code{14} \code{2} \code{3} \code{4}
\code{5} \code{6} \code{7} \code{8} \code{9}}
\item{list("Lat")}{a numeric vector of latitudes in decimal degrees}
\item{list("Lon")}{a numeric vector of longitudes in decimal degrees}
\item{list("Month")}{a numeric vector of months photos were taken}
\item{list("region")}{a character vector representing regions where photos were taken}
\item{list("NCA.SEAK")}{a numeric vector with values 1 if region contained in N CA to SEAK and 0 otherwise}
\item{list("NCA.NBC")}{a numeric vector with values 1 if region contained in N CA to NBC and 0 otherwise}
\item{list("OR.NBC")}{a numeric vector with values 1 if region contained in OR to NBC and 0 otherwise}
\item{list("OR.SVI")}{a numeric vector with values 1 if region contained in OR to SVI and 0 otherwise}
\item{list("OR.WVI")}{a numeric vector with values 1 if region contained in OR to WVI and 0 otherwise}
\item{list("MUA")}{a numeric vector with values 1 if region is MUA (NWA and SJF)}
\item{list("MUA.SVI")}{a numeric vector with values 1 if region contained in MUA to SVI and 0 otherwise}
\item{list("AK")}{a numeric vector with values 1 if region contained in Alaska and 0 otherwise}
\item{list("spring")}{a numeric vector with value 1 if whale was seen at some point in the spring (March to May) anywhere from N CA to NBC (not Puget Sound);0 otherwise}
\item{list("springNWA")}{a numeric vector value 1 if whale was seen at some point in the spring (March to May) anywhere in NWA;0 otherwise}
\item{list("springSJF")}{a numeric vector value 1 if whale was seen at some point in the spring (March to May) anywhere in SJF;0 otherwise}
\item{list("Region")}{a factor variable representing regions where photos
were taken with NWA/SJF collapsed to MUA and NPS/PS-HC-BB-SJF collapsed to
NPS}
\item{list("CalfStatus")}{either was a calf (C) or suspected to be a calf (C?) the
first year the whale was seen. It is NA otherwise.}
\item{list("MomID")}{ID of the mother}
\item{list("Calf")}{year whale was a calf and 0 if non-calf}
\item{list("FirstDate")}{First date seen with mom}
\item{list("LastDate")}{Last date seen with mom}
\item{list("CalfAloneDate")}{First date calf seen without mom}}}}
\description{
  Photo-identification data for gray whales from California
  to Kodiak Alaska
}
\examples{
# This is how ER dataframe was created from pcfa.txt data file
# Read in data file and create region fields
ER=read.delim("pcfa.txt",header=TRUE,na.strings="",sep="\\t",colClasses=c("numeric","factor","numeric","character","factor","factor","numeric","numeric"))
# Create date type
ER$Date=as.Date(ER$Date,"\%d-\%b-\%y")
ER$Month=as.POSIXlt(ER$Date)$mon+1
# Add various region fields to enable easy partitioning of the data
ER$Region=as.character(ER$Sreg)
Regions=c("NCA","SOR","OR","GH+","NWA","SJF","SVI","WVI","NBC","SEAK")
ER$NCA.SEAK=0
ER$NCA.SEAK[ER$Region\%in\%Regions]=1
Regions=c("NCA","SOR","OR","GH+","NWA","SJF","SVI","WVI","NBC")
ER$NCA.NBC=0
ER$NCA.NBC[ER$Region\%in\%Regions]=1
Regions=c("SOR","OR","GH+","NWA","SJF","SVI","WVI","NBC")
ER$OR.NBC=0
ER$OR.NBC[ER$Region\%in\%Regions]=1
Regions=c("SOR","OR","GH+","NWA","SJF","SVI")
ER$OR.SVI=0
ER$OR.SVI[ER$Region\%in\%Regions]=1
Regions=c("SOR","OR","GH+","NWA","SJF","SVI","WVI")
ER$OR.WVI=0
ER$OR.WVI[ER$Region\%in\%Regions]=1
ER$MUA=0
ER$MUA[ER$Region=="NWA"|ER$Region=="SJF"]=1
ER$MUA.SVI=0
ER$MUA.SVI[ER$MUA==1|ER$Region=="SVI"]=1
ER$AK=0
ER$AK[ER$Region\%in\%c("KAK","SEAK","NAK")]=1
ERx=ER[!is.na(ER$Month)&!ER$Region\%in\%c("SJI","PS-HC-BB-SJ","NPS","SEAK","CCA","SCA","NAK"),]
springwhales=unique(ERx$ID[ERx$Year>=1998&ERx$Month\%in\%c(3,4,5)])
ER$spring=0
ER$spring[ER$ID\%in\%springwhales]=1
# Add a field to indicate whales seen in spring in NWA
springwhales=unique(ERx$ID[ERx$Year>=1998&ERx$Month\%in\%c(3,4,5)&ERx$Region=="NWA"])
ER$springNWA=0
ER$springNWA[ER$ID\%in\%springwhales]=1
# Add a field to indicate whales seen in spring in SJF
springwhales=unique(ERx$ID[ERx$Year>=1998&ERx$Month\%in\%c(3,4,5)&ERx$Region=="SJF"])
ER$springSJF=0
ER$springSJF[ER$ID\%in\%springwhales]=1
names(ER)[names(ER)=="Region"]="region"
ER$Region=ER$region
ER$Region[ER$Region\%in\%c("NPS","PS-HC-BB-SJ","SJI")]="NPS"
ER$Region=factor(ER$Region,levels=c("MX","SCA","CCA","NCA","SOR","OR","GH+","NWA","SJF","NPS","SVI","WVI","NBC","SEAK","KAK","NAK"))
# add calf data
calves=read.delim("CalfMomData.txt",colClasses=c("numeric","factor","numeric","numeric",rep("character",3)))
calves$FirstDate=as.Date(calves$FirstDate,"\%d-\%b-\%y")
calves$LastDate=as.Date(calves$LastDate,"\%d-\%b-\%y")
calves$CalfAloneDate=as.Date(calves$CalfAlone,"\%d-\%b-\%y")
names(calves)[names(calves)\%in\%"Year"]="Calf"
ER=merge(ER,calves,by.x="ID",by.y="CalfID",all.x=TRUE)
}
\keyword{datasets}

