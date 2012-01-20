#' Create dataframe with capture history matrix and related data
#' 
#' 
#' 
#' @param ER dataframe containing all or subset of data in \code{\link{ER}}
#' @return A dataframe with the following fields: \item{ID}{CRC gray whale
#' photo ID number} \item{ch}{character string of 0/1 for the capture history}
#' \item{cohort}{first year sighted} \item{times.resighted}{number of years
#' resighted after initial sighting} \item{times.could.be.resighted}{number of
#' years it could be resighted after initial sighting}
#' \item{number.years.seen}{1+times.resighted} \item{minstay}{1+ number of days
#' between first and last day seen in the first year seen}
#' \item{min1998...min2007}{0 or the value of \code{minstay} if first year
#' seen} \item{pmin1998...min2008}{value of \code{minstay} in the prior year
#' regardless of number of years seen; pmin1998 always 0}
#' \item{td1998...td2008}{0 or 1 if seen in prior year; td2008 always 0}
#' \item{first1998...first2008}{0 or 1 if that was the first year seen}
#' \item{old}{0 or 1 if seen in a year prior to 1998 in the ER data}
#' \item{sightings}{number of sightings 0 or 1 if seen in a year prior to 1998
#' in the ER data}
#' @export
#' @author Jeff Laake
create.chmat <-
function(ER)
{
#
# Create capture history
#
yrlevels=min(ER$Year):max(ER$Year)
ER$Year=factor(ER$Year,levels=yrlevels)
chmat=table(list(ER$ID,ER$Year))
nocc=ncol(chmat)
chmat[chmat>0]=1
number.years.seen=apply(chmat,1,sum)
cohort=factor(apply(chmat,1,function(x) min(which(x!="0")))+min(yrlevels)-1,levels=yrlevels)
cohort.lookup=cbind(as.numeric(names(cohort)),as.numeric(cohort)+1997)
first.month=apply(cohort.lookup,1,function(x)min(ER$Month[ER$ID==x[1]&ER$Year==x[2]]))
first=matrix(0,nrow=length(cohort),ncol=nocc)
first[cbind(1:nrow(first),as.numeric(cohort))]=1
firstplus=t(apply(chmat,1,cumsum))
firstplus[firstplus>0]=1
times.resighted=apply(chmat,1,sum)-1
times.could.be.resighted=apply(firstplus,1,sum)-1

xx=ER[!duplicated(subset(ER,select=c("ID","Date"))),]
times.sighted.morethanonce=as.numeric(rowSums(table(xx$ID,xx$Year))>1)
years.sighted.morethanonce=as.numeric(rowSums(ifelse(table(ER$ID,ER$Year)>1,1,0))>1)
regions.sighted.morethanone=as.numeric(rowSums(apply(t(table(ER$ID,ER$Region)),1,function(x) as.numeric(x>0)))>1)

er=data.frame(ID=as.numeric(row.names(chmat)),ch=apply(chmat,1,paste,collapse=""),cohort=cohort,times.resighted=times.resighted,
   times.could.be.resighted=times.could.be.resighted,number.years.seen=number.years.seen,times.sighted.morethanonce=times.sighted.morethanonce,
   years.sighted.morethanonce=years.sighted.morethanonce,regions.sighted.morethanone=regions.sighted.morethanone,first.month=first.month)
er$ch=as.character(er$ch)
#
# Compute minimum stay for each whale for each year and create time-dependent covariates for p and Phi
#
minstay=tapply(ER$Date,list(ER$ID,ER$Year),max)-tapply(ER$Date,list(ER$ID,ER$Year),min)+1
minstay[is.na(minstay)]=0
Phiminstay=(minstay*first)[,-nocc]
pminstay=cbind(rep(0,nrow(minstay)),minstay[,-nocc])
colnames(pminstay)[1]=min(yrlevels)-1
colnames(pminstay)=paste("pmin",as.numeric(colnames(pminstay))+1,sep="")
colnames(Phiminstay)=paste("min",colnames(Phiminstay),sep="")
td=as.data.frame(cbind(rep(0,nrow(chmat)),chmat[,-ncol(chmat)]))
names(td)[1]=min(yrlevels)-1
names(td)=paste("td",as.numeric(names(td))+1,sep="")
first=as.data.frame(first)
names(first)=paste("first",yrlevels,sep="")
minstay= minstay[cbind(1:nrow(minstay),match(er$cohort,as.numeric(colnames(minstay))))]
er=cbind(er,minstay=minstay,Phiminstay,pminstay,td,first,old=as.numeric(table(ER$ID,ER$old)[,2]>0))
id.year.reg=with(ER[!duplicated(subset(ER,select=c("ID","Date"))),],table(list(ID,Year,Region)))
sightings.per.id=apply(id.year.reg,1,sum)
er$sightings=sightings.per.id
return(er)
}

