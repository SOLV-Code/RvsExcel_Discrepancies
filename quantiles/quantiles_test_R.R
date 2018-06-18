# Script to check alternative quantile calculations against excel

# the quantile() function actually has 9 alternative methods 
#  ?quantile

# the default method is type=7 and gave very different result from a excel-based
# cross-check for Estu Spn. So, here's a check of alternative types.
# The excel calcs are in Quantiles_ExcelCalc.xlsx


test.series <- read.csv("SampleData1.csv",stringsAsFactors=FALSE)
probs.use <- c(0.1,0.25,0.5,0.75,0.9)
var.use <- "spn_mill"


mat.out <- matrix(NA,nrow=9,ncol=length(probs.use),dimnames=list(paste("Type",1:9,sep=""),
			paste("p",probs.use*100,sep="")))

print(median(test.series[,var.use]))
for(t in 1:9){

	mat.out[t,] <- quantile(test.series[,var.use],type=t,probs=probs.use )

}

mat.out

write.csv(mat.out,"quantiles_R_Output.csv")


#  => quantile type=6 exactly matches the Excel results for this data set



# Plot the differences
ylim.plot <- c(0,max(test.series[,var.use],na.rm=TRUE))
xlim.plot <- c(min(test.series[,"yr"],na.rm=TRUE),30+ max(test.series[,"yr"],na.rm=TRUE))
plot(test.series[,"yr"],test.series[,var.use],axes=FALSE,bty="n",type="o",col="darkblue",
	xlim=xlim.plot,ylim=ylim.plot,pch=19,xlab="Year",ylab=var.use)
axis(2)
x.ticks <- pretty(test.series[,"yr"])[pretty(test.series[,"yr"]) %in% test.series[,"yr"]]
axis(1,at=x.ticks ,labels=x.ticks )

box.add <- function(X,at.x=1,box.width=1,fill.use="white",border.use="darkblue",whisker.lty=1,tips.add=FALSE){
                segments(at.x,X[1],at.x,X[5],col=border.use,lty=whisker.lty)
				if(tips.add){
							tip.length <- box.width/3
							segments(at.x-tip.length ,X[1],at.x+tip.length ,X[1],col=border.use)
							segments(at.x-tip.length ,X[5],at.x+tip.length ,X[5],col=border.use)
							}
                rect(at.x-box.width/2,X[2],at.x+box.width/2,X[4],col=fill.use,border=border.use)
                segments(at.x-(box.width/2)+0.1,X[3],at.x+(box.width/2)-0.1,X[3],col=border.use,lwd=3,lend=2)
                        }


bar.ticks <- max(test.series[,"yr"],na.rm=TRUE) + c(1:9)*(3)

for(t in 1:9){
box.add(mat.out[t,],at.x=bar.ticks[t]  ,box.width=1,tips.add=TRUE)
}

axis(1,at=bar.ticks,labels=1:9,cex.axis=0.8)






