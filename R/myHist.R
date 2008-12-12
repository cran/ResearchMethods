myHist <- function(xl,yb,xr,yt,hDat,breaks=NULL,sample=TRUE,CIs=FALSE,xlab=FALSE,label.bars=FALSE,right=TRUE){
  if(is.null(breaks)){
    breaks = 10
  }
  rect(xl,yb,xr,yt,density=-1,col='white',border=1)
  info = hist(hDat,breaks=breaks,plot=FALSE,right=right)
  bCount = length(info$breaks)
  colors = rep(grey(0.75),bCount)
  xPlot = c(xl+(xr-xl)*.1,xr-(xr-xl)*.1)
  yPlot = c(yb+(yt-yb)*.2,yt*.9)
  x = c(seq(xPlot[1],xPlot[2],length.out=bCount+1))    
  y = (info$counts/max(info$counts))*(yPlot[2]-yPlot[1])+yPlot[1]
  if(CIs){
    pts = as.numeric(quantile(hDat,probs=seq(0,1,by=0.025))[c(2,40)])
    xLCI = x[cumsum(info$breaks<pts[1])[bCount]]
    xRCI = x[cumsum(!(info$breaks>pts[2]))[bCount]+1]
    points(c(xLCI,xLCI),yPlot,col='red',type='l',lwd=2)
    points(c(xRCI,xRCI),yPlot,col='red',type='l',lwd=2)
    info$CI = pts
    colors[x<xLCI] = 2
    colors[x>=xRCI] = 2
  }
  for(i in 1:bCount){
    rect(x[i],yPlot[1],x[i+1],y[i],density=-1,col=colors[i])
    if(xlab){
      text(x[i],yPlot[1],pos=1,label=info$breaks[i])
    }
    if(label.bars){
      text((x[i]+x[i+1])/2,y[i],pos=3,label=info$counts[i])
    }
  }
  if(!sample){
    text(mean(xPlot),yb,pos=3,labels=substitute(paste(mu,"=",m),list(m=signif(mean(hDat),4))))
  }
  else{
    text(mean(xPlot),yb,pos=3,labels=substitute(paste(bar(x),"=",m),list(m=signif(mean(hDat),4))))
  }
  info$xPlot = x
  info$yPlot = y
  return(info)
}
