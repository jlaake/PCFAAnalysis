#' POPAN model fitting
#' 
#' 
#' @export open.estimate alt.open.estimate
#' @param er dataframe 
#' @param delta value of delta aicc to use for model averaging set. Default is Inf to use all.
#' @return List of results
#' @author Jeff Laake
open.estimate=function(er,delta=Inf,alternate=FALSE)
{
# minyyyy is the minimum tenure measure for year yyyy if it was the
# whales first year seen; else 0
# The following centers each of those variables which subtracts the median value
# of those whales with a non-zero value such that those with missing values (0)
# can be set to the median value.
#
if(!alternate)
{
   er$min1998[er$min1998>0]=er$min1998[er$min1998>0]-median(er$min1998[er$min1998>0])
   er$min1999[er$min1999>0]=er$min1999[er$min1999>0]-median(er$min1999[er$min1999>0])
   er$min2000[er$min2000>0]=er$min2000[er$min2000>0]-median(er$min2000[er$min2000>0])
   er$min2001[er$min2001>0]=er$min2001[er$min2001>0]-median(er$min2001[er$min2001>0])
   er$min2002[er$min2002>0]=er$min2002[er$min2002>0]-median(er$min2002[er$min2002>0])
   er$min2003[er$min2003>0]=er$min2003[er$min2003>0]-median(er$min2003[er$min2003>0])
   er$min2004[er$min2004>0]=er$min2004[er$min2004>0]-median(er$min2004[er$min2004>0])
   er$min2005[er$min2005>0]=er$min2005[er$min2005>0]-median(er$min2005[er$min2005>0])
   er$min2006[er$min2006>0]=er$min2006[er$min2006>0]-median(er$min2006[er$min2006>0])
   er$min2007[er$min2007>0]=er$min2007[er$min2007>0]-median(er$min2007[er$min2007>0])
   er$min2008[er$min2008>0]=er$min2008[er$min2008>0]-median(er$min2008[er$min2008>0])
   er$min2009[er$min2009>0]=er$min2009[er$min2009>0]-median(er$min2009[er$min2009>0])

   er$min1998[er$min1998>0]=er$min1998[er$min1998>0]/100
   er$min1999[er$min1999>0]=er$min1999[er$min1999>0]/100
   er$min2000[er$min2000>0]=er$min2000[er$min2000>0]/100
   er$min2001[er$min2001>0]=er$min2001[er$min2001>0]/100
   er$min2002[er$min2002>0]=er$min2002[er$min2002>0]/100
   er$min2003[er$min2003>0]=er$min2003[er$min2003>0]/100
   er$min2004[er$min2004>0]=er$min2004[er$min2004>0]/100
   er$min2005[er$min2005>0]=er$min2005[er$min2005>0]/100
   er$min2006[er$min2006>0]=er$min2006[er$min2006>0]/100
   er$min2007[er$min2007>0]=er$min2007[er$min2007>0]/100
   er$min2008[er$min2008>0]=er$min2008[er$min2008>0]/100
   er$min2009[er$min2009>0]=er$min2009[er$min2009>0]/100

# minyyyy is the minimum tenure measure for year yyyy-1 which is used to 
# model probability of detection of the whale in year yyyy. If it was not
# seen in year yyyy-1 then it is set to 0.  
# The following centers each of those variables which subtracts the median value
# of those whales with a non-zero value such that those with missing values (0)
# can be set to the median value.
#

   er$pmin1998[er$pmin1998>0]=er$pmin1998[er$pmin1998>0]-median(er$pmin1998[er$pmin1998>0])
   er$pmin1999[er$pmin1999>0]=er$pmin1999[er$pmin1999>0]-median(er$pmin1999[er$pmin1999>0])
   er$pmin2000[er$pmin2000>0]=er$pmin2000[er$pmin2000>0]-median(er$pmin2000[er$pmin2000>0])
   er$pmin2001[er$pmin2001>0]=er$pmin2001[er$pmin2001>0]-median(er$pmin2001[er$pmin2001>0])
   er$pmin2002[er$pmin2002>0]=er$pmin2002[er$pmin2002>0]-median(er$pmin2002[er$pmin2002>0])
   er$pmin2003[er$pmin2003>0]=er$pmin2003[er$pmin2003>0]-median(er$pmin2003[er$pmin2003>0]) 
   er$pmin2004[er$pmin2004>0]=er$pmin2004[er$pmin2004>0]-median(er$pmin2004[er$pmin2004>0])
   er$pmin2005[er$pmin2005>0]=er$pmin2005[er$pmin2005>0]-median(er$pmin2005[er$pmin2005>0])
   er$pmin2006[er$pmin2006>0]=er$pmin2006[er$pmin2006>0]-median(er$pmin2006[er$pmin2006>0])
   er$pmin2007[er$pmin2007>0]=er$pmin2007[er$pmin2007>0]-median(er$pmin2007[er$pmin2007>0])
   er$pmin2008[er$pmin2008>0]=er$pmin2008[er$pmin2008>0]-median(er$pmin2008[er$pmin2008>0]) 
   er$pmin2009[er$pmin2009>0]=er$pmin2009[er$pmin2009>0]-median(er$pmin2009[er$pmin2009>0])
   er$pmin2010[er$pmin2010>0]=er$pmin2010[er$pmin2010>0]-median(er$pmin2010[er$pmin2010>0])
}

#er$ID=NULL
er$NC=1-er$Calf
years=as.numeric(levels(er$cohort))
# Process data and set up design data for RMark
er.proc=process.data(er,model="POPAN",begin.time=1998,groups="cohort")
er.ddl=make.design.data(er.proc)
# create firstyr which is 1 if this is the whale's first year and 0 otherwise
er.ddl$Phi$firstyr=0
er.ddl$Phi$firstyr[as.character(er.ddl$Phi$group)==as.character(er.ddl$Phi$time)]=1
# create firstcohort which is 0 except for 1998 which has value 1 because it is the
# first cohort and these are a mix of whales new in 1998 and others that have been there
# in previous years.
er.ddl$Phi$firstcohort=0
er.ddl$Phi$firstcohort[er.ddl$Phi$cohort==1998]=1
# 1-firstyr = notfirstyr
er.ddl$p$notfirstyr=1
er.ddl$p$notfirstyr[as.character(er.ddl$p$group)==as.character(er.ddl$p$time)]=0
# setup indices for prob entry to have a fixed value of 1 such that cohort yyyy all
# enters at year yyyy.
fixed.pent.index=as.numeric(row.names(er.ddl$pent))
fixed.pent.values=rep(0,length(fixed.pent.index))
fixed.pent.values[as.character(er.ddl$pent$group)==as.character(er.ddl$pent$time)]=1

fixed.p.index=as.numeric(row.names(er.ddl$p))[as.character(er.ddl$p$group)==as.character(er.ddl$p$time)]
fixed.p.values=rep(0,length(fixed.p.index))
fixed.p.values=1

# create a timebin field that lumps 1998-1999 so N1998 can be estimated.
er.ddl$p$timebin=cut(er.ddl$p$Time,c(-1,1:(max(years)-min(years))))
levels(er.ddl$p$timebin)=c("1998-1999",as.character(2000:max(years)))
# create function to fit various model sets
do.popan=function()
{
if(alternate)
{
   p.1=list(formula=~-1+timebin,fixed=list(index=fixed.p.index,value=fixed.p.values))
   p.2=list(formula=~-1+timebin+pmin,fixed=list(index=fixed.p.index,value=fixed.p.values))
   p.3=list(formula=~pmin,fixed=list(index=fixed.p.index,value=fixed.p.values))
}
else
{
	p.1=list(formula=~-1+timebin)
	p.2=list(formula=~-1+timebin+pmin)
	p.3=list(formula=~pmin)
}
  Phi.1=list(formula=~firstyr)
  Phi.2=list(formula=~firstyr+firstyr:min)
  Phi.3=list(formula=~firstcohort:firstyr+firstyr)
  Phi.4=list(formula=~firstcohort:firstyr+firstyr+min:firstyr)
  Phi.5=list(formula=~firstcohort:firstyr+firstyr+firstyr:min+firstcohort:min)
  Phi.6=list(formula=~cohort:firstyr+firstyr:min)
  Phi.7=list(formula=~cohort:firstyr+Calf:firstyr+firstyr+firstyr:min)
  Phi.8=list(formula=~cohort:firstyr+Calf:firstyr+ Calf:min:firstyr +firstyr:min)
  Phi.9=list(formula=~firstcohort:firstyr+firstyr+firstyr:min+firstcohort:min + Calf:firstyr)
  Phi.10=list(formula=~firstcohort:firstyr+firstyr+firstyr:min+firstcohort:min + Calf:firstyr+Calf:min:firstyr )
 
  if(alternate)
	  N.1=list(formula=~-1+group,fixed=0)
  else
      N.1=list(formula=~-1+group)
  pent.1=list(formula=~1,fixed=list(index=fixed.pent.index,value=fixed.pent.values))
  cml=create.model.list("POPAN")
  results=mark.wrapper(cml,data=er.proc,ddl=er.ddl,output=FALSE)
}
# Run set of models and store in popan.results
popan.results=do.popan()
# Use top models with delta AICc <4 
model.nums=as.numeric(row.names(popan.results$model.table[popan.results$model.table$DeltaAICc<delta,]))
weight=popan.results$model.table$DeltaAICc[popan.results$model.table$DeltaAICc<delta]
weight=weight-min(weight)
weight=exp(-.5*weight)/sum(exp(-.5*weight))
nmodels=length(model.nums)
N.vcv.list=vector("list",nmodels)
Nest=matrix(NA,nrow=nmodels,ncol=nchar(er$ch[1]))
if(alternate)
{
	lnN.vcv.list=vector("list",nmodels)
	lnNest=matrix(NA,nrow=nmodels,ncol=nchar(er$ch[1]))
	for(i in 1:nmodels)
	{
		mod=popan.results[[model.nums[i]]]
		xx=abundance.p(er,mod) 
		Nest[i,]=xx$N
		N.vcv.list[[i]]=xx$N.vcv
		lnNest[i,]=xx$lnN
		lnN.vcv.list[[i]]=xx$lnN.vcv
	}
	Nbyocc=model.average(list(estimates=Nest,vcv=N.vcv.list,weight=weight))
# Compute LCL,UCL,Nmin calculation with std formula for log-normal conf interval
	Nbyocc$estimate=data.frame(N=Nbyocc$estimate,se=Nbyocc$se)
	C=exp(sqrt(log(1+(Nbyocc$estimate$se/Nbyocc$estimate$N)^2)))
	Nbyocc$estimate$LCL=Nbyocc$estimate$N/C
	Nbyocc$estimate$UCL=Nbyocc$estimate$N*C
	Nbyocc$estimate$Nmin=with(Nbyocc$estimate,N/exp(0.864*sqrt(log(1+(se/N)^2))))
	lnNbyocc=model.average(list(estimates=lnNest,vcv=lnN.vcv.list,weight=weight))
	lnNbyocc$estimate=data.frame(lnN=lnNbyocc$estimate,se=lnNbyocc$se)
	return(list(Nbyocc=Nbyocc,lnNbyocc=lnNbyocc,model.list=popan.results))
} else
{
	for(i in 1:nmodels)
	{
		mod=popan.results[[model.nums[i]]]
# Assign all covariate values to 0 which is the average value since all have been centered
		suppressWarnings(mod$design.matrix[is.na(as.numeric(mod$design.matrix))]<-"0")
# Call popan.derived to estimate abundance
		xx=popan.derived(er.proc,mod)
		Nest[i,]=xx$Nbyocc$N
		N.vcv.list[[i]]=xx$Nbyocc.vcv
	}
	Nbyocc=model.average(list(estimates=Nest,vcv=N.vcv.list,weight=weight))
# Compute LCL,UCL,Nmin calculation with std formula for log-normal conf interval
	Nbyocc$estimate=data.frame(N=Nbyocc$estimate,se=Nbyocc$se)
	C=exp(sqrt(log(1+(Nbyocc$estimate$se/Nbyocc$estimate$N)^2)))
	Nbyocc$estimate$LCL=Nbyocc$estimate$N/C
	Nbyocc$estimate$UCL=Nbyocc$estimate$N*C
	Nbyocc$estimate$Nmin=with(Nbyocc$estimate,N/exp(0.864*sqrt(log(1+(se/N)^2))))
	return(list(Nbyocc=Nbyocc,model.list=popan.results))
	
}
}

