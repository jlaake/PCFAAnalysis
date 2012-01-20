

#' Photo-identification data for gray whales from California to Kodiak Alaska
#' 
#' 
#' 
#' @name ER
#' @docType data
#' @format A data frame with 13595 observations (a photo-identification) on the
#' following 23 variables.  \describe{ \item{list("ID")}{a numeric vector of
#' CRC gray whale IDs} \item{list("ResearchGroup")}{research group that
#' contributed the photo} \item{list("Year")}{a numeric vector of years photos
#' were taken} \item{list("Date")}{vector of dates photos were taken}
#' \item{list("Sreg")}{a region factor with levels \code{CCA} \code{GH+}
#' \code{KAK} \code{NBC} \code{NCA} \code{NPS} \code{NWA} \code{OR}
#' \code{PS-HC-BB-SJ} \code{SCA} \code{SEAK} \code{SJF} \code{SOR} \code{SVI}
#' \code{WVI}} \item{list("Reg")}{a numeric region factor with levels \code{1}
#' \code{10} \code{11} \code{12} \code{13} \code{14} \code{2} \code{3} \code{4}
#' \code{5} \code{6} \code{7} \code{8} \code{9}} \item{list("Lat")}{a numeric
#' vector of latitudes in decimal degrees} \item{list("Lon")}{a numeric vector
#' of longitudes in decimal degrees} \item{list("Calf")}{year whale was a calf
#' and 0 if non-calf} \item{list("Month")}{a numeric vector of months photos
#' were taken} \item{list("region")}{a character vector representing regions
#' where photos were taken} \item{list("NCA.SEAK")}{a numeric vector with
#' values 1 if region contained in N CA to SEAK and 0 otherwise}
#' \item{list("NCA.NBC")}{a numeric vector with values 1 if region contained in
#' N CA to NBC and 0 otherwise} \item{list("OR.NBC")}{a numeric vector with
#' values 1 if region contained in OR to NBC and 0 otherwise}
#' \item{list("OR.SVI")}{a numeric vector with values 1 if region contained in
#' OR to SVI and 0 otherwise} \item{list("OR.WVI")}{a numeric vector with
#' values 1 if region contained in OR to WVI and 0 otherwise}
#' \item{list("MUA")}{a numeric vector with values 1 if region is MUA (NWA and
#' SJF)} \item{list("MUA.SVI")}{a numeric vector with values 1 if region
#' contained in MUA to SVI and 0 otherwise} \item{list("AK")}{a numeric vector
#' with values 1 if region contained in Alaska and 0 otherwise}
#' \item{list("spring")}{a numeric vector with value 1 if whale was seen at
#' some point in the spring (March to May) anywhere from N CA to NBC (not Puget
#' Sound);0 otherwise} \item{list("springNWA")}{a numeric vector value 1 if
#' whale was seen at some point in the spring (March to May) anywhere in NWA;0
#' otherwise} \item{list("springSJF")}{a numeric vector value 1 if whale was
#' seen at some point in the spring (March to May) anywhere in SJF;0 otherwise}
#' \item{list("Region")}{a factor variable representing regions where photos
#' were taken with NWA/SJF collapsed to MUA and NPS/PS-HC-BB-SJF collapsed to
#' NPS} }
#' @keywords datasets
#' @examples
#' 
#' # This is how ER dataframe was created from pcfa.txt data file
#' # Read in data file anc create region fields
#' ER=read.delim("pcfa.txt",header=TRUE,na.strings="",sep="\t",colClasses=c("numeric","factor","numeric","character","factor","factor","numeric","numeric"))
#' # Create date type
#' ER$Date=as.Date(ER$Date,"%d-%b-%y")
#' ER$Month=as.POSIXlt(ER$Date)$mon+1
#' # Add various region fields to enable easy partitioning of the data
#' ER$Region=as.character(ER$Sreg)
#' Regions=c("NCA","SOR","OR","GH+","NWA","SJF","SVI","WVI","NBC","SEAK")
#' ER$NCA.SEAK=0
#' ER$NCA.SEAK[ER$Region%in%Regions]=1
#' Regions=c("NCA","SOR","OR","GH+","NWA","SJF","SVI","WVI","NBC")
#' ER$NCA.NBC=0
#' ER$NCA.NBC[ER$Region%in%Regions]=1
#' Regions=c("SOR","OR","GH+","NWA","SJF","SVI","WVI","NBC")
#' ER$OR.NBC=0
#' ER$OR.NBC[ER$Region%in%Regions]=1
#' Regions=c("SOR","OR","GH+","NWA","SJF","SVI")
#' ER$OR.SVI=0
#' ER$OR.SVI[ER$Region%in%Regions]=1
#' Regions=c("SOR","OR","GH+","NWA","SJF","SVI","WVI")
#' ER$OR.WVI=0
#' ER$OR.WVI[ER$Region%in%Regions]=1
#' ER$MUA=0
#' ER$MUA[ER$Region=="NWA"|ER$Region=="SJF"]=1
#' ER$MUA.SVI=0
#' ER$MUA.SVI[ER$MUA==1|ER$Region=="SVI"]=1
#' ER$AK=0
#' ER$AK[ER$Region%in%c("KAK","SEAK")]=1
#' ERx=ER[!is.na(ER$Month)&!ER$Region%in%c("PS-HC-BB-SJ","NPS","SEAK","CCA","SCA"),]
#' springwhales=unique(ERx$ID[ERx$Year>=1998&ERx$Month%in%c(3,4,5)])
#' ER$spring=0
#' ER$spring[ER$ID%in%springwhales]=1
#' # Add a field to indicate whales seen in spring in NWA
#' springwhales=unique(ERx$ID[ERx$Year>=1998&ERx$Month%in%c(3,4,5)&ERx$Region=="NWA"])
#' ER$springNWA=0
#' ER$springNWA[ER$ID%in%springwhales]=1
#' # Add a field to indicate whales seen in spring in SJF
#' springwhales=unique(ERx$ID[ERx$Year>=1998&ERx$Month%in%c(3,4,5)&ERx$Region=="SJF"])
#' ER$springSJF=0
#' ER$springSJF[ER$ID%in%springwhales]=1
#' names(ER)[names(ER)=="Region"]="region"
#' ER$Region=ER$region
#' ER$Region[ER$Region%in%c("NPS","PS-HC-BB-SJ")]="NPS"
#' ER$Region=factor(ER$Region,levels=c("SCA","CCA","NCA","SOR","OR","GH+","NWA","SJF","NPS","SVI","WVI","NBC","SEAK","KAK"))
#' 
NULL





#' Photo-identification data of gray whales in the PCFA which is defined as the
#' area from N CA to Kodiak Alaska from 1 June to 30 November
#' 
#' 
#' This is a subset of the data in \code{\link{ER}}.  See example for how it
#' was constructed. It includes all years of data.
#' 
#' @name PCFA
#' @docType data
#' @format A data frame with 12018 observations on the following 24 variables.
#' \describe{ \item{list("ID")}{a numeric vector of CRC gray whale IDs}
#' \item{list("ResearchGroup")}{research group that contributed the photo}
#' \item{list("Year")}{a numeric vector of years photos were taken}
#' \item{list("Date")}{vector of dates photos were taken} \item{list("Sreg")}{a
#' region factor with levels \code{CCA} \code{GH+} \code{KAK} \code{NBC}
#' \code{NCA} \code{NPS} \code{NWA} \code{OR} \code{PS-HC-BB-SJ} \code{SCA}
#' \code{SEAK} \code{SJF} \code{SOR} \code{SVI} \code{WVI}}
#' \item{list("Reg")}{a numeric region factor with levels \code{1} \code{10}
#' \code{11} \code{12} \code{13} \code{14} \code{2} \code{3} \code{4} \code{5}
#' \code{6} \code{7} \code{8} \code{9}} \item{list("Lat")}{a numeric vector of
#' latitudes in decimal degrees} \item{list("Lon")}{a numeric vector of
#' longitudes in decimal degrees} \item{list("Calf")}{year whale was a calf and
#' 0 if non-calf} \item{list("Month")}{a numeric vector of months photos were
#' taken} \item{list("region")}{a character vector representing regions where
#' photos were taken} \item{list("NCA.SEAK")}{a numeric vector with values 1 if
#' region contained in N CA to SEAK and 0 otherwise} \item{list("NCA.NBC")}{a
#' numeric vector with values 1 if region contained in N CA to NBC and 0
#' otherwise} \item{list("OR.NBC")}{a numeric vector with values 1 if region
#' contained in OR to NBC and 0 otherwise} \item{list("OR.SVI")}{a numeric
#' vector with values 1 if region contained in OR to SVI and 0 otherwise}
#' \item{list("OR.WVI")}{a numeric vector with values 1 if region contained in
#' OR to WVI and 0 otherwise} \item{list("MUA")}{a numeric vector with values 1
#' if region is MUA (NWA and SJF)} \item{list("MUA.SVI")}{a numeric vector with
#' values 1 if region contained in MUA to SVI and 0 otherwise}
#' \item{list("AK")}{a numeric vector with values 1 if region contained in
#' Alaska and 0 otherwise} \item{list("old")}{a numeric vector with values 1 if
#' whale was seen in PCFA prior to 1998 and 0 otherwise}
#' \item{list("spring")}{a numeric vector with value 1 if whale was seen at
#' some point in the spring (March to May) anywhere from N CA to NBC (not Puget
#' Sound);0 otherwise} \item{list("springNWA")}{a numeric vector value 1 if
#' whale was seen at some point in the spring (March to May) anywhere in NWA;0
#' otherwise} \item{list("springSJF")}{a numeric vector value 1 if whale was
#' seen at some point in the spring (March to May) anywhere in SJF;0 otherwise}
#' \item{list("Region")}{a factor variable representing regions where photos
#' were taken with NWA/SJF collapsed to MUA} }
#' @keywords datasets
#' @examples
#' 
#' data(ER)
#' #########################################################################################
#' # Define PCFA group which are whales seen from 1 June to 30 November except in Puget Sound, Hood Canal, etc
#' PCFA=ER[ER$Month>=6&!is.na(ER$Month)&ER$Month<=11&!ER$Region%in%c("NPS","SCA","CCA"),]
#' oldwhales=unique(PCFA$ID[PCFA$Year<1998])
#' # Add a field old to indicate the whale had been seen previous to 1998 in the PCFA
#' PCFA$old=0
#' PCFA$old[PCFA$ID%in%oldwhales]=1
#' # reset levels of Region and collapse MUA
#' PCFA$Region=PCFA$region
#' PCFA$Region[PCFA$Region%in%c("NWA","SJF")]="MUA"
#' PCFA$Region=factor(PCFA$Region,levels=c("NCA","SOR","OR","GH+","MUA","SVI","WVI","NBC","SEAK","KAK"))
#' 
NULL





#' Analysis of gray whale photo-identification data collected from California
#' to Kodiak Alaska
#' 
#' Contains various utility functions and scripts for analysis to support
#' development of EIS on Makah proposal to harvest gray whales.  Includes data
#' exploration and summary and code for abundance and survival estimation.
#' 
#' \tabular{ll}{ Package: \tab PCFAAnalysis\cr Type: \tab Package\cr Version:
#' \tab 1.0\cr Date: \tab 2009-12-30\cr License: \tab GPL-2\cr LazyLoad: \tab
#' yes\cr }
#' 
#' @name PCFAAnalysis-package
#' @aliases PCFAAnalysis-package PCFAAnalysis
#' @docType package
#' @author Jeff Laake Maintainer: <Jeff.Laake@@noaa.gov>
#' @keywords package
#' 
NULL



