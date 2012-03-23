#' Abundance estimation (JS1) without cavles
#' 
#' @export noCalf_Abundance
#' @param er dataframe with a field \code{ch} which is the capture history
#' string of 0s & 1s
#' @param popan.results list of fitted models
#' @param delta value of delta aicc to use for model averaging set. Default is Inf to use all.
#' @param alternate If TRUE do not median center MT and use JS1 approach for abundance estimation
#' @param chat Overdispersion value
#' @return List of abundance estimates
#' @author Jeff Laake
noCalf_Abundance=function(er,popan.results,chat,delta=Inf)
{
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
	lnN.vcv.list=vector("list",nmodels)
	lnNest=matrix(NA,nrow=nmodels,ncol=nchar(er$ch[1]))
	for(i in 1:nmodels)
	{
		mod=popan.results[[model.nums[i]]]
		xx=abundance.p(er,mod,minyear,calf=FALSE) 
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
}
