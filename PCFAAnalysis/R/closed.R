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
#' @param plot produces plot of estimates if TRUE
#' @return For \code{closed} \item{Nhat}{vector containing sequence of
#' abundance estimates} \item{Nse}{vector containing standard errors of
#' abundance estimates} \item{Nch}{list containing table of capture histories
#' for each of the estimates}
#' 
#' For \code{limited.LP} \item{Nhat}{vector containing sequence of abundance
#' estimates} \item{Nse}{vector containing standard errors of abundance
#' estimates}
#' 
#' For \code{closed.set}, each list contains a list with Nhat and Nse for all
#' and Nch for all except for \code{limited.LP} \item{LP}{Lincoln-Petersen
#' sequence of abundance estimates} \item{limited.LP}{limited Lincoln-Petersen
#' sequence of abundance estimates} \item{Darroch3}{Darroch estimator with 3
#' occasions} \item{Darroch4}{Darroch estimator with 4 occasions}
#' \item{Darroch5}{Darroch estimator with 5 occasions}
#' @author Jeff Laake
closed <-function(x,nocc)
{
# closed population estimators  -- nocc = number of occasions  
  nest=nchar(x$ch)[1]-nocc+1
  Nhat=vector("numeric",length=nest)
  Nse=vector("numeric",length=nest)
  Nch=vector("list",length=nest)
  for(i in 1:nest)
  {
     xx=x
     xx$ch=substr(xx$ch,i,i+nocc-1)
     xx=xx[xx$ch!=paste(rep("0",nocc),collapse=""),,drop=FALSE]
     ss=summary(mark(xx,model="Closed",model.parameters=list(p=list(formula=~time,share=TRUE)),output=FALSE,delete=TRUE),se=TRUE)$real$N
     Nhat[i]=ss$estimate
     Nse[i]=ss$se
     Nch[[i]]=table(xx$ch)
  }
return(list(Nhat=Nhat,Nse=Nse,Nch=Nch))
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
function(x)
{
nyears=nchar(x$ch[1])
xmat=strsplit(x$ch,"")
xmat=do.call(rbind,xmat)
dimx=dim(xmat)
xmat=as.numeric(xmat)
dim(xmat)=dimx
x1=as.numeric(rowSums(xmat[,3:nyears])>0)
x2=as.numeric(rowSums(xmat[,c(1,4:nyears)])>0)
x3=as.numeric(rowSums(xmat[,c(1:2,5:nyears)])>0)
x4=as.numeric(rowSums(xmat[,c(1:3,6:nyears)])>0)
x5=as.numeric(rowSums(xmat[,c(1:4,7:nyears)])>0)
x6=as.numeric(rowSums(xmat[,c(1:5,8:nyears)])>0)
x7=as.numeric(rowSums(xmat[,c(1:6,9:nyears)])>0)
x8=as.numeric(rowSums(xmat[,c(1:7,10:nyears)])>0)
x9=as.numeric(rowSums(xmat[,c(1:8,11:nyears)])>0)
x10=as.numeric(rowSums(xmat[,c(1:9,12:nyears)])>0)
x11=as.numeric(rowSums(xmat[,c(1:10,13:nyears)])>0)
x12=as.numeric(rowSums(xmat[,c(1:11)])>0)
Nhat=vector("numeric",nyears-1)
Nse=vector("numeric",nyears-1)
Nch=vector("numeric",nyears-1)
xx=closed(x[x1==1,],2)
Nhat[1]=xx$Nhat[1]
Nse[1]=xx$Nse[1]
Nch[1]=xx$Nch[1]
xx=closed(x[x2==1,],2)
Nhat[2]=xx$Nhat[2]
Nse[2]=xx$Nse[2]
Nch[2]=xx$Nch[2]
xx=closed(x[x3==1,],2)
Nhat[3]=xx$Nhat[3]
Nse[3]=xx$Nse[3]
Nch[3]=xx$Nch[3]
xx=closed(x[x4==1,],2)
Nhat[4]=xx$Nhat[4]
Nse[4]=xx$Nse[4]
Nch[4]=xx$Nch[4]
xx=closed(x[x5==1,],2)
Nhat[5]=xx$Nhat[5]
Nse[5]=xx$Nse[5]
Nch[5]=xx$Nch[5]
xx=closed(x[x6==1,],2)
Nhat[6]=xx$Nhat[6]
Nse[6]=xx$Nse[6]
Nch[6]=xx$Nch[6]
xx=closed(x[x7==1,],2)
Nhat[7]=xx$Nhat[7]
Nse[7]=xx$Nse[7]
Nch[7]=xx$Nch[7]
xx=closed(x[x8==1,],2)
Nhat[8]=xx$Nhat[8]
Nse[8]=xx$Nse[8]
Nch[8]=xx$Nch[8]
xx=closed(x[x9==1,],2)
Nhat[9]=xx$Nhat[9]
Nse[9]=xx$Nse[9]
Nch[9]=xx$Nch[9]
xx=closed(x[x10==1,],2)
Nhat[10]=xx$Nhat[10]
Nse[10]=xx$Nse[10]
Nch[10]=xx$Nch[10]
xx=closed(x[x11==1,],2)
Nhat[11]=xx$Nhat[11]
Nse[11]=xx$Nse[11]
Nch[11]=xx$Nch[11]
xx=closed(x[x12==1,],2)
Nhat[12]=xx$Nhat[12]
Nse[12]=xx$Nse[12]
Nch[12]=xx$Nch[12]
return(list(Nhat=Nhat,Nse=Nse,Nch=Nch))
}


