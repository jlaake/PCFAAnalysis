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

