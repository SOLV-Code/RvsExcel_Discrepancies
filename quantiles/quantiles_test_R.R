# Script to check alternative quantile calculations against excel

# the quantile() function actually has 9 alternative methods 
?quantile

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

# Plot the differences
#ylim.plot <- 
#plot(







#  => quantile type=6 exactly matches the Excel results for this data set









