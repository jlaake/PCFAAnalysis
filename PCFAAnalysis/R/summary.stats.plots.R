#' Produces a set of summary statistics and plots for a subset of the PCFA data
#' 
#' 
#' 
#' @param x dataframe (or subset) of \code{\link{PCFA}}
#' @param ch capture history dataframe created by \code{\link{create.chmat}}
#' using same data
#' @export 
#' @return None
#' @author Jeff Laake
#' @seealso \code{\link{movement.table}}
#' 
summary.stats.plots=function(x,ch)
{
cat("\nTotal number of unique whale IDs\n")
print(length(table(x$ID)))
cat("\nTotal number of unique whale IDs by region and year\n")
print(with(x,apply(table(ID,Region,year),3,function(x)apply(x,2,function(x) sum(x>0)))))
cat("\nTotal number of unique whales by region across all years\n")
print(with(x,apply(table(ID,Region),2,function(x)sum(x>0))))
cat("\nExpress as a proportion of unique whales across all regions\n")
print(with(x,apply(table(ID,Region),2,function(x)sum(x>0)))/length(table(x$ID)),digits=3)
cat("\nShow numbers of unique whale IDs by year and region for whales seen once(in one year) and more than once\n")
print(with(x[x$times.resighted==0,],apply(table(ID,Region,year),3,function(x)apply(x,2,function(x) sum(x>0)))))
print(with(x[x$times.resighted>0,],apply(table(ID,Region,year),3,function(x)apply(x,2,function(x) sum(x>0)))))
cat("\nShow numbers of unique whale IDs by region for whales seen once(in one year) and more than once\n")
print(with(x[x$times.resighted==0,],apply(table(ID,Region),2,function(x)sum(x>0))))
print(with(x[x$times.resighted>0,],apply(table(ID,Region),2,function(x)sum(x>0))))
cat("\nExpress as a proportion of unique whales seen in all regions in each set\n")
print(with(x[x$times.resighted==0,],apply(table(ID,Region),2,function(x)sum(x>0)))/
    length(table(x$ID[x$times.resighted==0])),digits=3)
print(with(x[x$times.resighted>0,],apply(table(ID,Region),2,function(x)sum(x>0)))/
    length(table(x$ID[x$times.resighted>0])),digits=3)
cat("\nShow numbers of unique whale ID's by month and region for whales seen once(in one year)and more than once\n")
print(with(x[x$times.resighted==0,],apply(table(ID,Region,Month),3,function(x)apply(x,2,function(x) sum(x>0)))))
print(with(x[x$times.resighted>0,],apply(table(ID,Region,Month),3,function(x)apply(x,2,function(x) sum(x>0)))))
cat("\nShow numbers of unique whale ID's by month for whales seen once(in one year)and more than once\n")
print(with(x[x$times.resighted==0,],apply(table(ID,Month),2,function(x) sum(x>0))))
print(with(x[x$times.resighted>0,],apply(table(ID,Month),2,function(x) sum(x>0))))
cat("\nShow numbers of unique whale ID's by region for whales seen once(in one year)and more than once\n")
print(with(x[x$times.resighted==0,],apply(table(ID,Region),2,function(x) sum(x>0))))
print(with(x[x$times.resighted>0,],apply(table(ID,Region),2,function(x) sum(x>0))))
cat("\nExpress as a proportion not resighted within each set\n")
print(with(x[x$times.resighted==0,],apply(table(ID,Region),2,function(x) sum(x>0)))/
(with(x[x$times.resighted>0,],apply(table(ID,Region),2,function(x) sum(x>0))) +
with(x[x$times.resighted==0,],apply(table(ID,Region),2,function(x) sum(x>0)))),digits=3)
# Summaries of first seen and first seen and resighted
cat("\nTable of first seen and first seen and resighted\n")
year.range=min(x$Year):max(x$Year)
print(colSums(ch[,paste("first",year.range,sep="")]))
print(colSums(ch[,paste("first",year.range,sep="")][ch$times.resighted>0,]))
print(cumsum(colSums(ch[,paste("first",year.range,sep="")][ch$times.resighted>0,])))
win.graph()
plot(year.range,cumsum(colSums(ch[,paste("first",year.range,sep="")][ch$times.resighted>0,])),xlab="Year",ylab="Cummulative number of returning whales",type="b")
win.graph()
plot(year.range,cumsum(colSums(ch[,paste("first",year.range,sep="")][ch$times.resighted>0,])),xlab="Year",ylab="Cummulative number of returning whales",type="b",xlim=c(min(x$Year),max(x$Year)-3))
cat("\nProportion resighted excluding those seen in PCFA prior to 1998\n")
print(colSums(ch[,paste("first",year.range,sep="")][ch$times.resighted>0&ch$old==0,])/
 colSums(ch[ch$old==0,paste("first",year.range,sep="")]),digits=3)
cat("\nTable of number of years seen\n")
table(ch$number.years.seen[ch$cohort!=max(x$Year)])
win.graph()
hist(ch$times.resighted[ch$times.resighted>0],xlab="Number years resighted",main="Number of years resighted of whales resighted at least once")
propcounts=with(ch,table(times.resighted/times.could.be.resighted))/ sum(with(ch,table(times.resighted/times.could.be.resighted)))
cat("\nNumber seen in all 11 years\n")
print(nrow(ch[ch$times.resighted==10,]))
# Remainder proportion
cat("\nResight proportions in all regions\n")
prop=as.numeric(names(propcounts))
cat("\n=0%\n")
print(sum(propcounts[prop<0.001]),digits=3)
cat("\n<50% & >0\n")
print(sum(propcounts[prop>0&prop<.5]),digits=3)
cat("\n>=50% & <100%\n")
print(sum(propcounts[prop>=.5&prop<1]),digits=3)
cat("\n=100%\n")
print(sum(propcounts[prop>0.999]),digits=3)
# Create a table that identifies which regions for each whale

move=movement.table(x)
# Plot proportion seen in adjacent areas given seen in specified area (rows of move)
win.graph()
par(mfrow=c(3,ceiling(length(levels(x$Region))/3)),oma=c(2,1,2,0),mar=c(1,1,5,1))
nr=length(levels(x$Region))
for(i in 1:nr)
{
   barplot(move[i,],col=c(rep("grey25",i-1),"white",rep("grey25",nr-i)),border=NA,ylim=c(0,.8),main=levels(x$Region)[i])
}
mtext("Proportion seen in adjacent areas of those seen in specified area",outer=TRUE,line=0)
# Plot proportion seen in specified area given seen in adjacent areas (columns of move)
win.graph()
par(mfrow=c(3,ceiling(length(levels(x$Region))/3)),oma=c(2,1,2,0),mar=c(1,1,5,1))
for(i in 1:nr)
{
   barplot(move[,i],col=c(rep("grey25",i-1),"white",rep("grey25",nr-i)),border=NA,ylim=c(0,.8),main=levels(x$Region)[i])
}
mtext("Proportion seen in specified area from those seen in adjacent areas",outer=TRUE,line=0)
id.year.reg=table(list(x$ID,x$Year,x$Region))
sightings.per.id=apply(id.year.reg,1,sum)
dd=as.data.frame(id.year.reg)
dd=dd[dd$Freq>0,]
names(dd)=c("ID","Year","Region","Freq")
# Plot of average number of regions versus number of years seen
win.graph()
xx=tapply(apply(table(list(dd$ID,dd$Region)),1,function(x)length(x[x>0])),ch$number.years.seen,mean)
plot(1:length(xx),xx,xlab="Number of years seen",ylab="Average number of regions",type="b")
# Plot of average number of sightings per year versus number of years seen
win.graph()
xx=tapply(apply(tapply(dd$Freq,list(dd$ID,dd$Year),sum),1,mean,na.rm=TRUE), ch$number.years.seen,mean)
plot(1:length(xx),xx,xlab="Number of years seen",ylab="Average number of times seen per year seen",type="b")
# Plot of percent sightings by number of years seen
win.graph()
xx=tapply(sightings.per.id,ch$number.years.seen,sum)/sum(sightings.per.id)
plot(1:length(xx),xx,xlab="Number of years seen"
,ylab="Percent of sightings",type="b")
# avg minstay for those seen >1 year
win.graph()
par(mfrow=c(3,3))
minstay=tapply(x$Date,list(x$ID,x$Year),max)-tapply(x$Date,list(x$ID,x$Year),min)+1
minstay[is.na(minstay)]=0
for(i in 1:9)
 if(any( ch$number.years.seen>i))
 hist(apply(minstay,1,function(x) mean(x[x>0]))[ch$number.years.seen>i],
   freq=FALSE,ylim=c(0,0.025),xlim=c(0,100),xlab="Average minimum stay",main=paste("More than",i,"years"))
# Minimum tenure and resight probability  
cat("\nProduce plots of probability of resight versus minimum tenure in first year excluding last cohort\n")
par(mfrow=c(1,2))
exclude.cohort=max(x$Year)
xx=ch[!ch$cohort%in%exclude.cohort,]
xx$fac = factor(as.numeric(xx$cohort != min(x$Year)))
levels(xx$fac)=c("First seen 1998","First seen after 1998")
p = ggplot(xx, aes(cut(minstay, c(0, 1, 14, 28, 60, 100, 200)),times.resighted/times.could.be.resighted))
print(p+geom_boxplot()+facet_grid(.~fac)+ylab("Proportion of years resighted\n")+xlab("\nMinimum tenure in first year (days)"))
win.graph()
meanp=sapply(with(xx,split(times.resighted>0,cut(minstay, c(0, 1, 14, 28, 60, 100, 200)))),mean)
print(qplot(factor(names(meanp),levels=names(meanp)),meanp,geom="bar",stat="identity",xlab="\nMinimum tenure in first year (days)",ylab="Proportion of whales sighted in one or more following years\n",ylim=c(0,1)))
invisible()
}

