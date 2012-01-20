#' Matrix of movements of whales between regions
#' 
#' 
#' 
#' @param ER dataframe \code{\link{ER}} or subset of that dataframe containing
#' fields \code{ID} and \code{Region}
#' @return A k by k matrix where k is the number of levels in the factor
#' variabele Region. The values in row i represent the proportion of whales
#' that were seen in region i that were also seen in each of the regions,
#' j=1,k.  Diagonal elements are obviously 1. Column i in the matrix is the
#' probability that a whale from each of the regions will be seen in the ith
#' region. Those values will be higher in regions with more effort.
#' @author Jeff Laake
#' @export
#' 
#' 
movement.table <-
function(ER)
{
ER$Region=factor(ER$Region)
xmat=table(list(ER$ID,ER$Region))
xmat[xmat>0]=1
# Create summary proportion table
move=matrix(0,nrow=length(levels(ER$Region)),ncol=length(levels(ER$Region)))
# colSums(xmat[xmat[,i]>0,,drop=FALSE]) conditions on having been seen in region i and
#   gets the total number of those that were seen in adjoining regions
#  nrow(xmat[xmat[,i]>0,,drop=FALSE]) is the number seen in region i
# so the rows of move are proportions seen in other regions that were seen in that region
# On the other hand the columns show the chances of seeing a whale from each of the other regions
#  
for(i in 1:length(levels(ER$Region)))
   move[i,]=colSums(xmat[xmat[,i]>0,,drop=FALSE])/nrow(xmat[xmat[,i]>0,,drop=FALSE])
colnames(move)=levels(ER$Region)
rownames(move)=levels(ER$Region)
return(move)
}

