\docType{data}
\name{PCFA}
\alias{PCFA}
\title{Photo-identification data of gray whales in the PCFA which is defined as the
area from N CA to Kodiak Alaska from 1 June to 30 November}
\format{A data frame with observations on the following 29 variables.
\describe{ \item{list("ID")}{a numeric vector of CRC gray whale IDs}
\item{list("ResearchGroup")}{research group that contributed the photo}
\item{list("Year")}{a numeric vector of years photos were taken}
\item{list("Date")}{vector of dates photos were taken} \item{list("Sreg")}{a
region factor with levels  \code{GH+} \code{KAK} \code{NBC}
\code{NCA} \code{NWA} \code{OR} \code{SEAK} \code{SJF} \code{SOR} \code{SVI} \code{WVI}}
\item{list("Reg")}{a numeric region factor with levels \code{1} \code{10}
\code{11} \code{12} \code{13} \code{14} \code{2} \code{3} \code{4} \code{5}
\code{6} \code{7} \code{8} \code{9}} \item{list("Lat")}{a numeric vector of
latitudes in decimal degrees} \item{list("Lon")}{a numeric vector of
longitudes in decimal degrees}  \item{list("Month")}{a numeric vector of months photos were
taken} \item{list("region")}{a character vector representing regions where
photos were taken} \item{list("NCA.SEAK")}{a numeric vector with values 1 if
region contained in N CA to SEAK and 0 otherwise} \item{list("NCA.NBC")}{a
numeric vector with values 1 if region contained in N CA to NBC and 0
otherwise} \item{list("OR.NBC")}{a numeric vector with values 1 if region
contained in OR to NBC and 0 otherwise} \item{list("OR.SVI")}{a numeric
vector with values 1 if region contained in OR to SVI and 0 otherwise}
\item{list("OR.WVI")}{a numeric vector with values 1 if region contained in
OR to WVI and 0 otherwise} \item{list("MUA")}{a numeric vector with values 1
if region is MUA (NWA and SJF)} \item{list("MUA.SVI")}{a numeric vector with
values 1 if region contained in MUA to SVI and 0 otherwise}
\item{list("AK")}{a numeric vector with values 1 if region contained in
Alaska and 0 otherwise} \item{list("old")}{a numeric vector with values 1 if
whale was seen in PCFA prior to 1996 and 0 otherwise}
\item{list("spring")}{a numeric vector with value 1 if whale was seen at
some point in the spring (March to May) anywhere from N CA to NBC (not Puget
Sound);0 otherwise} \item{list("springNWA")}{a numeric vector value 1 if
whale was seen at some point in the spring (March to May) anywhere in NWA;0
otherwise} \item{list("springSJF")}{a numeric vector value 1 if whale was
seen at some point in the spring (March to May) anywhere in SJF;0 otherwise}
\item{list("Region")}{a factor variable representing regions where photos
were taken with NWA/SJF collapsed to MUA}
\item{list("MomID")}{ID of the mother}
\item{list("Calf")}{Year they were seen as a calf or 0}
\item{list("FirstDate")}{First date seen with mom}
\item{list("LastDate")}{Last date seen with mom}
\item{list("CalfAloneDate")}{First date calf seen without mom}
\item{list("old")}{Seen prior to 1996}
}}
\description{
  This is a subset of the data in \code{\link{ER}}.  See
  example for how it was constructed. It includes all years
  of data.
}
\examples{
data(ER)
# Define PCFA group which are whales seen from 1 June to 30 November except in Puget Sound, Hood Canal, etc
PCFA=ER[ER$Month>=6&!is.na(ER$Month)&ER$Month<=11&!ER$Region\%in\%c("NPS","SCA","CCA","NAK","MX"),]
oldwhales=unique(PCFA$ID[PCFA$Year<1996])
# Add a field old to indicate the whale had been seen previous to 1996 in the PCFA
PCFA$old=0
PCFA$old[PCFA$ID\%in\%oldwhales]=1
# reset levels of Region and collapse MUA
PCFA$Region=PCFA$region
PCFA$Region[PCFA$Region\%in\%c("NWA","SJF")]="MUA"
PCFA$Region=factor(PCFA$Region,levels=c("NCA","SOR","OR","GH+","MUA","SVI","WVI","NBC","SEAK","KAK"))
PCFA$Sreg=factor(PCFA$Sreg)
}
\keyword{datasets}

