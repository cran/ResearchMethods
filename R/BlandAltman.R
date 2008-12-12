BlandAltman <- function(x,y,gui=TRUE,bandsOn=FALSE,biasOn=FALSE,regionOn=FALSE,smooth=FALSE,sig=2){
  if(sum(is.na(x)+is.na(y))){
    n = length(x)
    out = (1:n)[(is.na(x)+is.na(y))!=0]
    x = x[-out]
    y = y[-out]
  }
  BAenv <- new.env()
  bands = as.numeric(bandsOn)
  bias = as.numeric(biasOn)
  region = as.numeric(regionOn)
  biasVal = sig
  assign("bands",tclVar(bands), env = BAenv)
  assign("bias",tclVar(bias), env = BAenv)
  assign ("region",tclVar(region), env = BAenv)
  assign("biasVal",tclVar(biasVal), env = BAenv)
  assign("smooth",tclVar(smooth), env = BAenv)

  refresh<-function(...)
  {
    labx = "Mean"
    laby = "Difference"
    maintit="Bland-Altman difference plot"
    alfactor = 1
    alxlimfactor = 1
    rawBM = 1 - as.numeric(evalq(tclvalue(bands), env = BAenv))
    shaded = as.numeric(evalq(tclvalue(region), env = BAenv))
    shadeBias = as.numeric(evalq(tclvalue(bias), env = BAenv))
    smooth = as.numeric(tclvalue(BAenv$smooth))

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
    x.lower.boundary<-min(average)
    x.upper.boundary<-max(average)
    limx<-c(x.lower.boundary,x.upper.boundary)
    y.boundary<-max(abs(pretty(difference)))
    limy<-c(-y.boundary,y.boundary)
    if(diff(limx)<diff(limy)){
      limx = mean(average,na.rm=TRUE) + c(-0.5,0.5)*diff(limy)
    }
    else {
      limy = mean(difference,na.rm=TRUE) + c(-0.5,0.5)*diff(limx)
    }
    
    # Plot
    par(pty="m")
    if (rawBM & !shaded & !shadeBias){
      # PLOT RAW BLAND-ALTMAN
      plot.default(average,difference,pch=20,cex=0.5,xlim=limx,ylim=limy,xlab=labx,ylab=laby,main=maintit)
      # Zero line - blue dashed 
      abline(h=0,lty="dotted",col="blue")
      par(new=FALSE)
    }
    else{ 
      # PLOT BLAND-ALTMAN WITH UPPER AND LOWER AGREEMENT LIMITS
      color = (difference <= upper.agreement.limit) - (difference < lower.agreement.limit) +2
      plot.default(jitter(average),jitter(difference),pch=20,cex=0.75,xlim=limx,ylim=limy,xlab=labx,ylab=laby,main=maintit,col=color)
      abline(h=0,lty="dotted",col="blue")
      # UAL, mean difference, LAL lines - red
      abline(h=upper.agreement.limit,lty="dashed",col="red",lwd=2)
      abline(h=difference.mean,col="red")
      abline(h=lower.agreement.limit,lty="dashed",col="red",lwd=2)
      # label UAL and LAL lines - red text
      text(min(range(limx))+abs(diff(range(limx))/3),(upper.agreement.limit+0.05*diff(range(difference))),pos=2,offset = 0,paste("UAL = +",mult),col=2)
      text(min(range(limx))+abs(diff(range(limx))/3),(upper.agreement.limit +0.05*diff(range(difference))),pos=4,offset = 0, expression(sigma),col=2)
      text(min(range(limx))+abs(diff(range(limx))/3),(upper.agreement.limit +0.05*diff(range(difference))),pos=4,offset = 1, paste(" = ",round(upper.agreement.limit,2)),col=2)
      text(min(range(limx))+abs(diff(range(limx))/3),(lower.agreement.limit+0.05*diff(range(difference))),pos=2,offset = 0,paste("LAL = -",mult),col=2)
      text(min(range(limx))+abs(diff(range(limx))/3),(lower.agreement.limit +0.05*diff(range(difference))),pos=4,offset = 0, expression(sigma),col=2)
      text(min(range(limx))+abs(diff(range(limx))/3),(lower.agreement.limit +0.05*diff(range(difference))),pos=4,offset = 1, paste(" = ",round(lower.agreement.limit,2)),col=2)
      # SHADE BIAS REGION
      if (shadeBias==TRUE){
        polygon(c(min(range(limx))-10,min(range(limx))-10,max(range(limx))+10,max(range(limx))+10),c(0,difference.mean,difference.mean,0), xpd=FALSE, col="orange", lty=1, lwd=1)
        text(max(range(limx)),max(0,difference.mean)+abs(diff(range(limy)))/15,"BIAS",pos = 2,col="orange",cex=2)
      }
      # PLOT SHADED REGION WHICH WE EXPECT TO INCLUDE 95% OF THE OBSERVATTIONS
      if (shaded==TRUE) { 
        polygon(c(min(range(limx))-10,min(range(limx))-10,max(range(limx))+10,max(range(limx))+10),c(lower.agreement.limit,upper.agreement.limit,upper.agreement.limit,lower.agreement.limit), xpd=FALSE, density=10, lty=2, lwd=1, border=NA, col="green")
        text(min(range(limx))+abs(diff(range(limx))/3),diff(range(limy))/2/3,"REGION OF",col="dark green",cex=2)
        text(min(range(limx))+abs(diff(range(limx))/3),-diff(range(limy))/2/3,"AGREEMENT",col="dark green",cex=2)
        par(new=FALSE)
      }
    }
    if (smooth){
      pts = lowess(average,difference)
      points(pts$x,pts$y,type='l',lty=2,col='blue',lwd=2)
      m = lm(difference~average)
      abline(m,lwd=2,lty=3,col='blue')
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
    smoothCheck <- tkcheckbutton(m,variable=BAenv$smooth, command = refresh, text="add a lines of best fit")
    tkpack(smoothCheck)
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
  
