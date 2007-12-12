`BlandAltman` <-
function(x,y,gui=TRUE,bandsOn=FALSE,biasOn=FALSE,regionOn=FALSE,sig=2){
BAenv <<- new.env()
bands = as.numeric(bandsOn)
bias = as.numeric(biasOn)
region = as.numeric(regionOn)
biasVal = sig
assign("bands",tclVar(bands), env = BAenv)
assign("bias",tclVar(bias), env = BAenv)
assign ("region",tclVar(region), env = BAenv)
assign("biasVal",tclVar(biasVal), env = BAenv)
refresh<-function(...)
{
if (length(x) != length(y))
	{
	cat("# Version 0.2	23.01.2007")
	cat(rep("\n",5))
	cat("The function \n\n\t\tBlandAltman (x, y, xlabel, ylabel, maintitle) \n\n* displays the classic Bland-Altman difference plot with limits ")
	cat("of agreement, \n* prints the mean difference and both agreement limits (along with 95% CIs) and \n* returns a list of relevant ")
	cat("parameters (the mean difference, its SD and SE, \n  upper and lower agreement limits, their SE, and the t value ")
	cat("used in the \n  calculation of CIs).\n\n")
	cat("The first two arguments (x and y) are required and should be vectors of equal \nlength, otherwise the execution is halted. ")
	cat("The other arguments are optional. \nWhen called with no arguments, this help is displayed.\n\n")
	cat("Based on the original Bland & Altman paper (Lancet 1986)\n(http://www-users.york.ac.uk/~mb55/meas/ba.htm)\n")
	cat("Original Author: Jaro Lajovic <jaro.lajovic@mf.uni-lj.si>\n\n")
	cat("Modified by: Mohamed Abdolell <mohamed.abdolell@dal.ca>\n")
	cat("Improve the visual display of point within and beyond UAL and LAL.\n")
	cat("Provides an additional argument that allows adjusting the default AL setting of 2*SD by a factor of alfactor.\n")
	cat("\n\n")
 	}
else
	{
        labx = "Mean"
        laby = "Difference"
        maintit="Bland-Altman difference plot"
        alfactor = 1
        alxlimfactor = 1
        rawBM = 1 - as.numeric(evalq(tclvalue(bands), env = BAenv))
        shaded = as.numeric(evalq(tclvalue(region), env = BAenv))
        shadeBias = as.numeric(evalq(tclvalue(bias), env = BAenv))
        if(gui & (shaded || shadeBias)){
          tkselect(bandsCheck)
        }
        mult = as.numeric(evalq(tclvalue(biasVal), env = BAenv))
        # NEW SD
        difference <- -1*(x-y)						
	average<-(x+y)/2					
	difference.mean<-mean(difference)		
	difference.sd<-sd(difference)
	al<-mult*difference.sd*alfactor
	upper.agreement.limit<-difference.mean+al	
	lower.agreement.limit<-difference.mean-al
	n<-length(difference)			
	
	difference.se<-difference.sd/sqrt(n)	
	al.se<-difference.sd*sqrt(3)/sqrt(n)	
	tvalue<-qt(0.975,n-1)			
	difference.mean.95ci<-difference.se*tvalue
	al.95ci<-al.se*tvalue
	upper.agreement.limit.95ci<-c(upper.agreement.limit-al.95ci,upper.agreement.limit+al.95ci)
	lower.agreement.limit.95ci<-c(lower.agreement.limit-al.95ci,lower.agreement.limit+al.95ci)
	

	# The x and the y limits of the plot (limx, limy)
	# if(min(average)<0){x.lower.boundary<-min(average)} else {x.lower.boundary<-0}
	# x.upper.boundary<-max(average)
	# limx<-c(x.lower.boundary,x.upper.boundary)
	# y.boundary<-max(abs(pretty(difference)))
	# limy<-c(-y.boundary,y.boundary)
	
        # TRYING a new definition of x and y limits: trying to make x-lim
        # more appropriate and make y compensate for inflated sigma
        x.lower.boundary <- min(average)
        x.upper.boundary <- max(average)
        limx <- c(x.lower.boundary,x.upper.boundary)
        y.boundary <- max(max(abs(pretty(difference))),(abs(difference.mean)+mult*1.1*difference.sd))
        limy <- c(-y.boundary,y.boundary)


	# Plot
	par(pty="m")
	if (rawBM & !shaded & !shadeBias)
		{ # PLOT RAW BLAND-ALTMAN
		plot.default(average,difference,pch=20,cex=0.5,xlim=limx,ylim=limy,xlab=labx,ylab=laby,main=maintit)
		# Zero line - blue dashed 
		abline(h=0,lty="dotted",col="blue")
		par(new=FALSE)
		}
	else
		{ 
                # PLOT BLAND-ALTMAN WITH UPPER AND LOWER AGREEMENT LIMITS
                color = (difference <= upper.agreement.limit) - (difference < lower.agreement.limit) +2
                plot.default(average,difference,pch=20,cex=0.5,xlim=limx,ylim=limy,xlab=labx,ylab=laby,main=maintit,col=color)
	        points(average,difference,cex = 0.5, col = color)	
		abline(h=0,lty="dotted",col="blue")

		# UAL, mean difference, LAL lines - red
		abline(h=upper.agreement.limit,lty="dotted",col="red")
		abline(h=difference.mean,col="red")
		abline(h=lower.agreement.limit,lty="dotted",col="red")
		# label UAL and LAL lines - red text
		text(min(range(limx))+abs(diff(range(limx))/3),(upper.agreement.limit+0.05*diff(range(difference))),pos=2,offset = 0,paste("UAL = +",mult),col=2)
                text(min(range(limx))+abs(diff(range(limx))/3),(upper.agreement.limit +0.05*diff(range(difference))),pos=4,offset = 0, expression(sigma),col=2)
                text(min(range(limx))+abs(diff(range(limx))/3),(upper.agreement.limit +0.05*diff(range(difference))),pos=4,offset = 1, paste(" = ",round(upper.agreement.limit,1)),col=2)
 
		text(min(range(limx))+abs(diff(range(limx))/3),(lower.agreement.limit+0.05*diff(range(difference))),pos=2,offset = 0,paste("UAL = -",mult),col=2)
                text(min(range(limx))+abs(diff(range(limx))/3),(lower.agreement.limit +0.05*diff(range(difference))),pos=4,offset = 0, expression(sigma),col=2)
                text(min(range(limx))+abs(diff(range(limx))/3),(lower.agreement.limit +0.05*diff(range(difference))),pos=4,offset = 1, paste(" = ",round(lower.agreement.limit,1)),col=2)
                
#		text(min(range(limx))+abs(diff(range(limx))/3),(lower.agreement.limit-0.05*diff(range(difference))),pos=4,paste("LAL =",round(lower.agreement.limit,1)),col=2)
		

		# SHADE BIAS REGION
		if (shadeBias==TRUE) 
		{
		polygon(c(min(range(limx))-10,min(range(limx))-10,max(range(limx))+10,max(range(limx))+10),c(0,difference.mean,difference.mean,0), xpd=FALSE, col="orange", lty=1, lwd=1)
		text(max(range(limx)),max(0,difference.mean)+abs(diff(range(limy)))/15,"BIAS",pos = 2,col="orange",cex=2)
		}

		# PLOT SHADED REGION WHICH WE EXPECT TO INCLUDE 95% OF THE OBSERVATTIONS
		if (shaded==TRUE)
		{ 
		polygon(c(min(range(limx))-10,min(range(limx))-10,max(range(limx))+10,max(range(limx))+10),c(lower.agreement.limit,upper.agreement.limit,upper.agreement.limit,lower.agreement.limit), xpd=FALSE, density=10, lty=2, lwd=1, border=NA, col="green")
		text(min(range(limx))+abs(diff(range(limx))/3),diff(range(limy))/2/3,"REGION OF",col="dark green",cex=2)
		text(min(range(limx))+abs(diff(range(limx))/3),-diff(range(limy))/2/3,"AGREEMENT",col="dark green",cex=2)
		par(new=FALSE)
		}
	}
	}
}
if(gui){
  m <- tktoplevel()
  tkwm.title(m, "Bland Altman Plot")
  bandsCheck <- tkcheckbutton(m,variable=BAenv$bands, command = refresh, text="add confidence bounds?")
  tkpack(bandsCheck)
  biasCheck <- tkcheckbutton(m,variable=BAenv$bias, command = refresh, text="add bias line?")
  tkpack(biasCheck)
  regionCheck <- tkcheckbutton(m,variable=BAenv$region, command = refresh, text="add region of agreement?")
  tkpack(regionCheck)
  frame <- tkframe(m)
  tkpack(frame,side="left")
  tkpack(tklabel(frame,text="sigma scaler"),side="left")
  bandsScale <- tkscale(frame,command=refresh,from=0,to=3,orient="horiz",resolution=.1,showvalue=FALSE,length = 125,variable=BAenv$biasVal)
  tkpack(bandsScale,side = "left")
}
else{
  refresh()
}
}

