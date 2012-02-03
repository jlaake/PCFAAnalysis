#' Alternative abundance estimators for open population models
#' 
#' 
#' @export abundance.surv abundance.p
#' @param x dataframe with a field \code{ch} which is the capture history
#' string of 0s & 1s
#' @param model fitted model
#' @return List of results
#' @author Jeff Laake

abundance.surv=function(x,model)
{
	nocc=nchar(x$ch[1])
	last=1998+nocc-1
	x[x$cohort==last,paste("min",last-1,sep="")]=x$minstay[x$cohort==last]
	x=subset(x,select=c("ID","cohort",paste("min",1998:(last-1),sep="")))
	x$index=(as.numeric(x$cohort)-1)*(nocc-1)+as.numeric(x$cohort)
	x$index[x$cohort==last]=x$index[x$cohort==(last-1)][1]
	x$cohort=as.numeric(x$cohort)+1997
	phi=c(0,1,rep(0,nocc-1),2)
	names(phi)=c("ID","cohort",paste("min",1998:(last-1),sep=""),"index")
	x=rbind(x,phi)
	surv=covariate.predictions(model,x)
	estimates=cbind(estimate=surv$estimates$estimate,x)
	phi=estimates$estimate[nrow(estimates)]
	Nmat=matrix(0,nrow=nrow(estimates)-1,ncol=nocc)
	for(i in 1:(nrow(estimates)-1))
	{
		j=estimates$cohort[i]-1997
		Nmat[i,j]=estimates$estimate[i]
		if(j<nocc)Nmat[i,(j+1):nocc]=Nmat[i,j]*phi^(1:(nocc-j))
	}
	NbyYear=colSums(Nmat)
	deriv=matrix(0,nrow(estimates),ncol=nocc)
	for(j in 1:ncol(deriv))
	{
		deriv[estimates$cohort==1998+j-1,j]=1
		prev.cohort=estimates$cohort!=0&estimates$cohort<1998+j-1
		deriv[prev.cohort,j]=estimates$estimate[nrow(estimates)]^(j-(estimates$cohort[prev.cohort]-1997)-1)
		for(i in 1:(j-1))
			deriv[nrow(estimates),j]=deriv[nrow(estimates),j]+ sum(estimates$estimate[estimates$cohort==(1997+i)])*
					(j-i-1)*estimates$estimate[nrow(estimates)]^(j-i-2)
	}
	vcv=t(deriv)%*%surv$vcv%*%deriv
	return(list(N=NbyYear,N.vcv=vcv))
}

abundance.p=function(x,model)
{
	ch=x$ch
#   Start off by creating dataframe for Phi estimates for interval after initial sighting	
	nocc=nchar(ch[1])
	last=1998+nocc-1
	x[x$cohort==last,paste("min",last-1,sep="")]=x$minstay[x$cohort==last]
	x=subset(x,select=c("ID","cohort","Calf","old",paste("min",1998:(last-1),sep=""),paste("pmin",1999:last,sep="")))
	x$index=(as.numeric(x$cohort)-1)*(nocc-1)+as.numeric(x$cohort)
	x$index[x$cohort==last]=x$index[x$cohort==(last-1)][1]
	x$cohort=as.numeric(x$cohort)+1997
	x$year=x$cohort
#   Next add to dataframe the p estimates for each resigting event	
	occasions.seen=lapply(strsplit(ch,""),function(x) (as.numeric(x)*1:nocc)[as.numeric(x)>0])
	baseindex=(nocc-1)*nocc
	addx=NULL
	for(i in 1:nrow(x))
	{
		nresight=length(occasions.seen[[i]])-1
		if(nresight>0)
		{
			xtmp=x[rep(i,nresight),]
			xtmp$index=baseindex+nocc*(x$cohort[i]-1998)+occasions.seen[[i]][-1]
			xtmp$year=1997+occasions.seen[[i]][-1]
			addx=rbind(addx,xtmp)
		}
	}
	x=rbind(x,addx)
#   Get predictions
	surv=covariate.predictions(model,x)
	estimates=cbind(estimate=surv$estimates$estimate,x)
	n=length(ch)
	phi=estimates$estimate[1:n]
	Nmat=with(estimates[1:n,],tapply(estimate,list(ID,cohort),sum))
	Nmat[is.na(Nmat)]=0
	NbyYear=colSums(Nmat,na.rm=TRUE)
	Nmat=with(estimates[(n+1):nrow(estimates),],tapply(1/estimate,list(ID,year),sum))
	NbyYear.r=c(0,colSums(Nmat,na.rm=TRUE))
	Nmat=with(estimates[(n+1):nrow(estimates),],tapply(1/estimate^2,list(ID,year),sum))
	NbyYear.sq=c(0,colSums(Nmat,na.rm=TRUE))
	NbyYear=NbyYear+NbyYear.r	
	deriv=matrix(0,nrow(estimates),ncol=nocc)
	for(j in 1:ncol(deriv))
	{
		deriv[estimates$cohort==1998+j-1&row(estimates)[,1]<=n,j]=1
		deriv[estimates$year==1998+j-1&row(estimates)[,1]>n,j]=-1/estimates$estimate[estimates$year==1998+j-1&row(estimates)[,1]>n]^2
	}
	
	vcv=t(deriv)%*%surv$vcv%*%deriv
	diag(vcv)=diag(vcv)+NbyYear.sq-NbyYear.r 
	lnvcv=log(1+vcv/outer(NbyYear,NbyYear,"*"))
	NonPcfg=tapply(1-estimates$estimate[estimates$old==0&row(estimates)[,1]<=n],estimates$cohort[estimates$old==0&row(estimates)[,1]<=n],sum)
	Pcfg=tapply(estimates$estimate[estimates$old==0&row(estimates)[,1]<=n],estimates$cohort[estimates$old==0&row(estimates)[,1]<=n],sum)
	return(list(N=NbyYear,N.vcv=vcv,cor=vcv/outer(sqrt(diag(vcv)),sqrt(diag(vcv)),"*"),
					lnN=log(NbyYear),lnN.vcv=lnvcv,ln.cor=lnvcv/outer(sqrt(diag(lnvcv)),sqrt(diag(lnvcv)),"*"),Pcfg=Pcfg,NonPcfg=NonPcfg))
}
