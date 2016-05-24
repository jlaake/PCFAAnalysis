#' Computes sequence of closed population estimates for a specified number of
#' consecutive years in the capture history.
#' 
#' 
#' If the length of each capture history string is k, then it will produce an
#' estimate using occasions 1 to nocc, then 2 to nocc+1, ..., and finally
#' k-nocc to k.  The number of abundance estimates will be k-nocc+1. It uses
#' the package RMark to construct the estimates with program MARK using a
#' time-specific capture probability model.
#' 
#' \code{limited.LP} produces Lincoln-Petersen abundance estimates limited to
#' non-transient whales. For each pair of years in the data, it selects the
#' whales that were seen either before or after the pair of years and
#' constructs a Lincoln-Petersen estimate of abundance using the 01,10 and 11
#' capture histories in the pair of years for the selected data.  This excludes
#' temporary immmigrants in the calculation for the closed population
#' estimator.  The abundance estimate is of the non-transient whales that were
#' in the population during the two year period.  It is probably still biased
#' and some simulation and theory development should be used to explore this
#' ad-hoc approach for adjusting a closed population estimator in an open
#' population.
#' 
#' @aliases closed limited.LP closed.set 
#' @export closed closed.set limited.LP
#' @param x dataframe with a field \code{ch} which is the capture history
#' string of 0s & 1s
#' @param nocc number of consecutive occasions to include in each abundance
#' estimate
#' @param begin.time time (year) for first occasion
#' @param plot produces plot of estimates if TRUE
#' @param formula formula for p as string; default is "~-1+group:time which is the same
#' as doing each estimator separately with no sharing of p estimates across years
#' @return For \code{closed} \item{Nhat}{vector containing sequence of
#' abundance estimates} \item{Nse}{vector containing standard errors of
#' abundance estimates} \item{Nch}{list containing table of capture histories
#' for each of the estimates}
#' 
#' For \code{limited.LP} \item{Nhat}{vector containing sequence of abundance
#' estimates} \item{Nse}{vector containing standard errors of abundance
#' estimates}
#' 
#' For \code{closed.set}, each list contains a list with Nhat and Nse for \item{LP}{Lincoln-Petersen
#' sequence of abundance estimates} \item{limited.LP}{limited Lincoln-Petersen
#' sequence of abundance estimates} \item{Darroch3}{Darroch estimator with 3
#' occasions} \item{Darroch4}{Darroch estimator with 4 occasions}
#' \item{Darroch5}{Darroch estimator with 5 occasions}
#' @author Jeff Laake
closed <-function(x,nocc,begin.time=1998,formula="~-1+time:group")
{
# closed population estimators  -- nocc = number of occasions  
	nest=nchar(x$ch)[1]-nocc+1
	df=NULL
	Nch=vector("list",length=nest)
	for(i in 1:nest)
	{
		xx=x
		xx$ch=substr(xx$ch,i,i+nocc-1)
		xx=xx[xx$ch!=paste(rep("0",nocc),collapse=""),,drop=FALSE]
		xx=subset(xx,select=c("ch","cohort"))
		xx$cohort=i+begin.time-1
		df=rbind(df,xx)
		Nch[[i]]=table(xx$ch)
	}
	df$cohort=factor(df$cohort)
	df.proc=process.data(df,model="Closed",groups="cohort",begin.time=begin.time-1+1:nest)
	df.ddl=make.design.data(df.proc)
	df.ddl$p$time2=factor(rep(c(1:nocc),nest))
	df.ddl$c$time2=factor(rep(2:nocc,nest),levels=c(1:nocc))
	ss=mark(df.proc,df.ddl,model="Closed",model.parameters=list(p=list(formula=as.formula(formula),share=TRUE,link="sin"),f0=list(formula=~-1+group)),output=FALSE,delete=TRUE)$results$derived[[1]]
	return(list(Nhat=ss$estimate,Nse=ss$se,Nch=Nch))
}
# Create simple function for closed estimators
closed.set=function(x,plot=FALSE)
{
  LP=closed(x,2)
  Darroch3=closed(x,3)
  Darroch4=closed(x,4)
  Darroch5=closed(x,5)
  limited.lp=limited.LP(x)
  if(plot)
  {
    xmin=min(as.numeric(as.character(ch.pcfa$cohort)))
    xmax=max(as.numeric(as.character(ch.pcfa$cohort)))
    plot(xmin:(xmax-1)+.5,LP$Nhat,type="b",ylim=c(0,max(c(LP$Nhat,Darroch3$Nhat,Darroch4$Nhat,Darroch5$Nhat))))
    lines((xmin+1):(xmax-1),Darroch3$Nhat,type="b",pch=2)
    lines(xmin:(xmax-3)+1.5,Darroch4$Nhat,type="b",pch=3)
    lines((xmin+2):(xmax-2),Darroch5$Nhat,type="b",pch=4)
    lines(xmin:(xmax-1)+.5,limited.lp$Nhat,type="b",pch=5)
  }
  return(list(LP=LP,limited.LP=limited.lp,Darroch3=Darroch3,Darroch4=Darroch4,Darroch5=Darroch5))
}
limited.LP <-
function(x,begin.time=1998,formula="~-1+time:group")
{
nyears=nchar(x$ch[1])
xmat=t(sapply(strsplit(x$ch,""),as.numeric))
df=NULL
Nch=vector("list",length=(nyears-1))
for(i in 1:(nyears-1))
{
	ch=substr(x$ch[rowSums(xmat[,-(i:(i+1))])>0],i,i+1)
	ch=ch[ch!="00"]
	Nch[[i]]=table(ch)
	df=rbind(df,data.frame(ch=ch,cohort=rep(i+begin.time-1,length(ch)),stringsAsFactors=FALSE))
}
df$cohort=factor(df$cohort)
df.proc=process.data(df,model="Closed",groups="cohort",begin.time=begin.time-1+1:(nyears-1))
df.ddl=make.design.data(df.proc)
df.ddl$p$time2=factor(rep(c(1:2),nyears-1))
df.ddl$c$time2=factor(rep(2,nyears-1),levels=c(1:2))
ss=mark(df.proc,df.ddl,model="Closed",model.parameters=list(p=list(formula=as.formula(formula),link="sin",share=TRUE),f0=list(formula=~-1+group)),output=FALSE,delete=TRUE)$results$derived[[1]]
return(list(Nhat=ss$estimate,Nse=ss$se,Nch=Nch))
}
