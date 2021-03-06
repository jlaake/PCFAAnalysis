#' POPAN model fitting
#' 
#' 
#' @export open.estimate 
#' @param er dataframe 
#' @param delta value of delta aicc to use for model averaging set. Default is Inf to use all.
#' @param alternate If TRUE do not median center MT and instead use JS1 approach for abundance estimation; 
#'                  alternate=FALSE was used in SC/62/BRG32
#' @param chat Overdispersion value
#' @return List of results
#' @author Jeff Laake
open.estimate=function(er,delta=Inf,alternate=FALSE,chat=1)
{
# minyyyy is the minimum tenure measure for year yyyy if it was the
# whales first year seen; else 0
# The following centers each of those variables which subtracts the median value
# of those whales with a non-zero value such that those with missing values (0)
# can be set to the median value.
#
lastyear=max(as.numeric(unlist(strsplit(names(er)[grep("pmin",names(er))],"pmin"))),na.rm=TRUE)
if(!alternate)
{
   for(y in 1996:(lastyear-1))
   {
     cat("\n",y)
     selrows=er[,paste("min",y,sep="")]>0
     er[selrows,paste("min",y,sep="")]=(er[selrows,paste("min",y,sep="")]-median(er[selrows,paste("min",y,sep="")]))/100
   }

# minyyyy is the minimum tenure measure for year yyyy-1 which is used to 
# model probability of detection of the whale in year yyyy. If it was not
# seen in year yyyy-1 then it is set to 0.  
# The following centers each of those variables which subtracts the median value
# of those whales with a non-zero value such that those with missing values (0)
# can be set to the median value.
#
  for(y in 1996:lastyear)
  {
    selrows=er[,paste("pmin",y,sep="")]>0
    er[selrows,paste("pmin",y,sep="")]=er[selrows,paste("pmin",y,sep="")]-median(er[selrows,paste("pmin",y,sep="")])
  }
}
er$NC=1-er$Calf
years=as.numeric(levels(er$cohort))
minyear=min(years)
# Process data and set up design data for RMark
er.proc=process.data(er,model="POPAN",begin.time=minyear,groups="cohort")
er.ddl=make.design.data(er.proc)
# create firstyr which is 1 if this is the whale's first year and 0 otherwise
er.ddl$Phi$firstyr=0
er.ddl$Phi$firstyr[as.character(er.ddl$Phi$group)==as.character(er.ddl$Phi$time)]=1
er.ddl$Phi$notfirstyr=1-er.ddl$Phi$firstyr
# create firstcohort which is a factor variable 1992-1997,1998,1999+
#er.ddl$Phi$firstcohort=cut(as.numeric(er.ddl$Phi$cohort),c(0,6,7,maxyear-minyear+1))
er.ddl$Phi$firstcohort=cut(as.numeric(er.ddl$Phi$cohort),c(0,2,3,maxyear-minyear+1))
# 1-firstyr = notfirstyr
er.ddl$p$notfirstyr=1
er.ddl$p$notfirstyr[as.character(er.ddl$p$group)==as.character(er.ddl$p$time)]=0
# setup indices for prob entry to have a fixed value of 1 such that cohort yyyy all
# enters at year yyyy.
fixed.pent.index=as.numeric(row.names(er.ddl$pent))
fixed.pent.values=rep(0,length(fixed.pent.index))
fixed.pent.values[as.character(er.ddl$pent$group)==as.character(er.ddl$pent$time)]=1
# fix p to 1 for newly seen
fixed.p.index=as.numeric(row.names(er.ddl$p))[as.character(er.ddl$p$group)==as.character(er.ddl$p$time)]
fixed.p.values=rep(1,length(fixed.p.index))
fixed.p.index2=as.numeric(row.names(er.ddl$p))[as.numeric(er.ddl$p$group)>as.numeric(er.ddl$p$time)]
fixed.p.values=c(fixed.p.values,rep(0,length(fixed.p.index2)))
fixed.p.index=c(fixed.p.index,fixed.p.index2)

# create a timebin field that lumps 1998-1999 so N1998 can be estimated; not used for alternate where p set to 1
er.ddl$p$timebin=cut(er.ddl$p$Time,c(-1,1:(max(years)-min(years))))
levels(er.ddl$p$timebin)=c(paste(minyear,minyear+1,sep="-"),as.character((minyear+2):max(years)))
# create function to fit various model sets
do.popan=function(chat)
{
if(alternate)
{
	p.1=list(formula=~-1+time,fixed=list(index=fixed.p.index,value=fixed.p.values))
	p.2=list(formula=~-1+time+pmin,fixed=list(index=fixed.p.index,value=fixed.p.values))
	p.3=list(formula=~pmin,fixed=list(index=fixed.p.index,value=fixed.p.values))
}else
{
	p.1=list(formula=~-1+timebin)
	p.2=list(formula=~-1+timebin+pmin)
	p.3=list(formula=~pmin)
}
  Phi.1=list(formula=~firstyr+nt:notfirstyr)
  Phi.2=list(formula=~firstyr+firstyr:min+nt:notfirstyr)
  Phi.3=list(formula=~firstcohort:firstyr+nt:notfirstyr)
  Phi.4=list(formula=~firstcohort:firstyr+min:firstyr+nt:notfirstyr)
  Phi.5=list(formula=~firstcohort:firstyr+firstyr:firstcohort:min+nt:notfirstyr)
  Phi.6=list(formula=~cohort:firstyr+firstyr:min+nt:notfirstyr)
  Phi.7=list(formula=~cohort:firstyr+Calf:firstyr+firstyr+firstyr:min+nt:notfirstyr)
  Phi.8=list(formula=~cohort:firstyr+Calf:firstyr+ Calf:min:firstyr +firstyr:min+nt:notfirstyr)
  Phi.9=list(formula=~firstcohort:firstyr+firstyr:firstcohort:min + Calf:firstyr+nt:notfirstyr)
  Phi.10=list(formula=~firstcohort:firstyr+firstyr:firstcohort:min + Calf:firstyr+Calf:min:firstyr+nt:notfirstyr )
  if(alternate)
	  N.1=list(formula=~-1+group,fixed=0)
  else
      N.1=list(formula=~-1+group)
  pent.1=list(formula=~1,fixed=list(index=fixed.pent.index,value=fixed.pent.values))
  cml=create.model.list("POPAN")
  results=mark.wrapper(cml,data=er.proc,ddl=er.ddl,output=FALSE,chat=chat)
}
# Run set of models and store in popan.results
popan.results=do.popan(chat)
# Use top models with delta AICc <4 
if(chat==1)
{
	model.nums=as.numeric(row.names(popan.results$model.table[popan.results$model.table$DeltaAICc<delta,]))
	weight=popan.results$model.table$DeltaAICc[popan.results$model.table$DeltaAICc<delta]
}
else
{
	model.nums=as.numeric(row.names(popan.results$model.table[popan.results$model.table$DeltaQAICc<delta,]))
	weight=popan.results$model.table$DeltaQAICc[popan.results$model.table$DeltaQAICc<delta]
}
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
		xx=abundance.p(er,mod,minyear) 
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
	Nbyocc$estimate$Nmin=with(Nbyocc$estimate,N/exp(0.842*sqrt(log(1+(se/N)^2))))
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
	Nbyocc$estimate$Nmin=with(Nbyocc$estimate,N/exp(0.842*sqrt(log(1+(se/N)^2))))
	return(list(Nbyocc=Nbyocc,model.list=popan.results))
	
}
}

