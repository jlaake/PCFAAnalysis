\name{PCFA}
\alias{PCFA}
\docType{data}
\title{Photo-identification data of gray whales in the PCFA which is defined as the area from N CA to Kodiak Alaska
from 1 June to 30 November}
\usage{data(PCFA)}
\format{
  A data frame with 12018 observations on the following 24 variables.
  \describe{
    \item{\code{ID}}{a numeric vector of CRC gray whale IDs}
    \item{\code{ResearchGroup}}{research group that contributed the photo}
    \item{\code{Year}}{a numeric vector of years photos were taken}
    \item{\code{Date}}{vector of dates photos were taken}
    \item{\code{Sreg}}{a region factor with levels \code{CCA} \code{GH+} \code{KAK} \code{NBC} \code{NCA} \code{NPS} \code{NWA} \code{OR} \code{PS-HC-BB-SJ} \code{SCA} \code{SEAK} \code{SJF} \code{SOR} \code{SVI} \code{WVI}}
    \item{\code{Reg}}{a numeric region factor with levels \code{1} \code{10} \code{11} \code{12} \code{13} \code{14} \code{2} \code{3} \code{4} \code{5} \code{6} \code{7} \code{8} \code{9}}
    \item{\code{Lat}}{a numeric vector of latitudes in decimal degrees}
    \item{\code{Lon}}{a numeric vector of longitudes in decimal degrees}
    \item{\code{Calf}}{year whale was a calf and 0 if non-calf}
    \item{\code{Month}}{a numeric vector of months photos were taken}
    \item{\code{region}}{a character vector representing regions where photos were taken}
    \item{\code{NCA.SEAK}}{a numeric vector with values 1 if region contained in N CA to SEAK and 0 otherwise}
    \item{\code{NCA.NBC}}{a numeric vector with values 1 if region contained in N CA to NBC and 0 otherwise}
    \item{\code{OR.NBC}}{a numeric vector with values 1 if region contained in OR to NBC and 0 otherwise}
    \item{\code{OR.SVI}}{a numeric vector with values 1 if region contained in OR to SVI and 0 otherwise}
    \item{\code{OR.WVI}}{a numeric vector with values 1 if region contained in OR to WVI and 0 otherwise}
    \item{\code{MUA}}{a numeric vector with values 1 if region is MUA (NWA and SJF)}
    \item{\code{MUA.SVI}}{a numeric vector with values 1 if region contained in MUA to SVI and 0 otherwise}
    \item{\code{AK}}{a numeric vector with values 1 if region contained in Alaska and 0 otherwise}
    \item{\code{old}}{a numeric vector with values 1 if whale was seen in PCFA prior to 1998 and 0 otherwise}
    \item{\code{spring}}{a numeric vector with value 1 if whale was seen at some point in the spring (March to May) anywhere from N CA to NBC (not Puget Sound);0 otherwise}
    \item{\code{springNWA}}{a numeric vector  value 1 if whale was seen at some point in the spring (March to May) anywhere in NWA;0 otherwise}
    \item{\code{springSJF}}{a numeric vector  value 1 if whale was seen at some point in the spring (March to May) anywhere in SJF;0 otherwise}
    \item{\code{Region}}{a factor variable representing regions where photos were taken with NWA/SJF collapsed to MUA}
  }
}
\details{This is a subset of the data in \code{\link{ER}}.  See example for how it was constructed. It includes all years of data.}
\examples{
data(ER)
#########################################################################################
# Define PCFA group which are whales seen from 1 June to 30 November except in Puget Sound, Hood Canal, etc
PCFA=ER[ER$Month>=6&!is.na(ER$Month)&ER$Month<=11&!ER$Region\%in\%c("NPS","SCA","CCA"),]
oldwhales=unique(PCFA$ID[PCFA$Year<1998])
# Add a field old to indicate the whale had been seen previous to 1998 in the PCFA
PCFA$old=0
PCFA$old[PCFA$ID\%in\%oldwhales]=1
# reset levels of Region and collapse MUA
PCFA$Region=PCFA$region
PCFA$Region[PCFA$Region\%in\%c("NWA","SJF")]="MUA"
PCFA$Region=factor(PCFA$Region,levels=c("NCA","SOR","OR","GH+","MUA","SVI","WVI","NBC","SEAK","KAK"))
}
\keyword{datasets}
