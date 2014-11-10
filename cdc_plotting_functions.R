############ CDC model (EbolaResponse, Meltzer et al.)
## re-implementation by Carl A.B. Pearson
## Plotting Functions by Juliet R.C. Pulliam

invisible(library(ggplot2)); invisible(library(reshape2))

plotPeriodCDF <- function(distro, fillCol="lightsteelblue1") with(distro, {
  par(bty="L",lwd=3)
	par(mar=c(5,7,1,1)+1)
	plot(NA, NA, bty="L", ann=F, xaxt="n", yaxt="n", xlim=c(0,max(days)), ylim=c(0,1))
	axis(1, cex.axis=1.8, padj=.6, lwd=3)
	mtext("Days post infection", 1, cex=1.5, line=3.5)
	axis(2, cex.axis=1.8, padj=-.6, lwd=3)
	mtext("Cumulative probability", 2, cex=1.5, line=6)
	mtext("Normalized daily probability",2,cex=1.5,line=4)	
	barplot(pdf/max(pdf),add=T,border=F,col=fillCol,space=0,width=1,ann=F,xaxt="n",yaxt="n")
	# polygon(c(days,rev(days)),c(pdf/max(pdf),rep(0,length(days))),border=fillCol,col = fillCol)
	points(c(days, length(days)), cdf, pch=19, type="l", lwd=6)
	legend("right",
    legend=c(
      "Cumulative probability",
      "Normalized daily probability"
    ),
    bty="n", cex=1.3, lwd=c(6, NA), lty=c(1, NA), pch=c(NA, 22),
    col=c("black", fillCol), pt.bg=c(NA, fillCol), pt.cex=1.5
  )
})

plotCumInc <- function(run,plotMeltzerCorrection=T,mcf=2.5,plotMort=F,cfr=0.72,
											 beginDate=as.Date("2014-03-26"),endDate=as.Date("2014-09-26"),startCDC=as.Date("2014-02-03"),
											 lineCol="#2b8cbe",mortCol="red4"){
	dates <- startCDC:(startCDC+dim(run)[1]-1)
	par(bty="L",lwd=3)
	par(mar=c(5,7,1,1)+1)
	if(plotMeltzerCorrection){
		plot(dates, run[,2]/1000*mcf, type="l",ann=F,xaxt="n",yaxt="n",col=lineCol,xlim=c(beginDate,endDate),lty=3)
		lines(dates, run[,2]/1000,col=lineCol)
	}else{
		plot(dates, run[,2]/1000, type="l",ann=F,xaxt="n",yaxt="n",col=lineCol,xlim=c(beginDate,endDate))
	}
	mtext("Cumulative no. of cases",2,cex=1.7,line=6)
	mtext("(Thousands)",2,cex=1.7,line=4.3)
	labDates <- as.Date(seq(beginDate,endDate,30))
	axis(1,labDates,format(labDates,"%m/%e"),cex.axis=1.8,padj=.6,lwd=3)
	axis(2,cex.axis=1.8,padj=-.6,lwd=3)
	if(plotMort&plotMeltzerCorrection) lines(dates, run[,2]/1000*cfr*mcf,col=mortCol)
	if(plotMort) lines(dates, run[,2]/1000*cfr,col=mortCol)	
	if(plotMeltzerCorrection&!plotMort) legend("topleft",legend = c("No correction factor","Meltzer (CDC) correction factor"),bty="n",cex=1.3,
				 lwd=3,lty=c(1,3),col=lineCol)
	if(plotMeltzerCorrection&plotMort) {
		legend("topleft",legend = c("Cases - no correction factor","Cases - Meltzer (CDC) correction factor","Deaths - no correction factor","Deaths - Meltzer (CDC) correction factor"),
					 bty="n",cex=1.3,lwd=3,lty=c(1,3,1,3),col=c(lineCol,lineCol,mortCol,mortCol))	
	}
}

plotBedOcc <- function(run,plotMeltzerCorrection=T,mcf=2.5,
											 beginDate=as.Date("2014-03-26"),endDate=as.Date("2014-09-26"),startCDC=as.Date("2014-02-03"),
											 lineCol="#2b8cbe",mortCol="red4"){
	dates <- startCDC:(startCDC+dim(run)[1]-1)
	par(bty="L",lwd=3,mar=c(5,7,1,1)+1)
	if(plotMeltzerCorrection){
		plot(dates, run[,3]/100*mcf, type="l",ann=F,xaxt="n",yaxt="n",col=lineCol,xlim=c(beginDate,endDate),lty=3)
		lines(dates, run[,3]/100,col=lineCol)
	}else{
			plot(dates, run[,3]/100, type="l",ann=F,xaxt="n",yaxt="n",col=lineCol,xlim=c(beginDate,endDate))
		}
	mtext("Daily no. of beds in use",2,cex=1.7,line=6)
	mtext("(Hundreds)",2,cex=1.7,line=4.3)
	labDates <- as.Date(seq(beginDate,endDate,30))
	axis(1,labDates,format(labDates,"%m/%e"),cex.axis=1.8,padj=.6,lwd=3)
	axis(2,cex.axis=1.8,padj=-.6,lwd=3)
}

plotInterventionScenario <- function(intervention.ref, simulation_duration,
    names=c("Hospitalization","Isolating Home Care","Non-isolating Care"),
    col=c("green","blue","red")) {
  if (dim(intervention.ref)[1] == 1) {
    intervention.ref[2,] <- intervention.ref[1,]
    intervention.ref[2,"start_day"] <- simulation_duration
  } else {
    lastrow <- intervention.ref[dim(intervention.ref)[1],,drop=F]
    if (lastrow[,"start_day"] < simulation_duration) {
      lastrow[,"start_day"] <- simulation_duration
      intervention.ref[dim(intervention.ref)[1]+1,] <- lastrow
    } else if (lastrow[,"start_day"] > simulation_duration) {
      first <- which(intervention.ref$start_day > simulation_duration)[1]
      intervention.ref <- intervention.ref[1:first,]
      intervention.ref[first,] <- intervention.ref[first-1,]
      intervention.ref[first,"start_day"] <- simulation_duration
    }    
  }
  intervention.mlt <- melt(intervention.ref, id.var="start_day", variable.name="treatment", value.name="proportion")
  p <- ggplot(intervention.mlt, aes(start_day, proportion))+scale_x_continuous(breaks=intervention.ref$start_day) + xlab("day")
  p <- p + geom_area(aes(fill=treatment), position = 'stack') + scale_fill_manual(labels=names, values=col)
}

