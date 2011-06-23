# a function to compare RR and OR

ratioCompare <- function(gui=TRUE,pointLabel=TRUE){
  RCenvir <<- new.env()
  RCenvir$p1 = RCenvir$p2 = seq(0,1,length.out=102)[2:101]
  RCenvir$RR = RCenvir$p1%*%t(1/RCenvir$p2)
  RCenvir$OR = (RCenvir$p1/(1-RCenvir$p1))%*%t((1-RCenvir$p2)/RCenvir$p2)
  RCenvir$diff = round(abs(log(RCenvir$OR)-log(RCenvir$RR)),4)
  RCenvir$colors = rainbow(100,start=3/6,end=0)
  RCenvir$breaks = quantile(RCenvir$diff,probs=seq(0,1,1/100))
  assign("p1Spot",tclVar(0.5),envir=RCenvir)
  assign("p2Spot",tclVar(0.5),envir=RCenvir)  
  refresh <- function(...){
    p1S = as.numeric(tclvalue(RCenvir$p1Spot))
    p2S = as.numeric(tclvalue(RCenvir$p2Spot))
    par(fig=c(0.8,1,0,1),mar=c(5,0,4,0))
    plot(0,xlim=c(0,1),ylim=c(0,100),type='n',xlab='',ylab='',main='Difference',axes=F)
    n = 15
    leg = seq(0,100,length.out=n)
    rect(rep(0,n),leg[1:(n-1)],rep(0.5,n),leg[2:n],density=-1,col=RCenvir$colors[leg])
    text(x=rep(0.75,n),y=leg[1:(n-1)],pos=3,labels=round(RCenvir$breaks[leg],2))
    par(new=T,fig=c(0,0.8,0,1),mar=c(5.1,4.1,4.1,1.1))
    image(RCenvir$p1,RCenvir$p2,RCenvir$diff,col=RCenvir$colors,breaks=RCenvir$breaks,xlab='p1',ylab='p2',main = 'log(OR) - log(RR)')
    if(pointLabel){
      points(p1S,p2S,pch=16,col='green')
      side = 0
      if(p1S > 0.7)
        side = 1
      height = 1
      if(p2S < 0.15)
        height = 0
      legend(p1S,p2S,xjust=side,yjust=height,legend=c(paste("OR:",round((p1S/(1-p1S))/(p2S/(1-p2S)),4)),paste("RR:",round(p1S/p2S,4))),bg='white')
    }
  }
  refresh()
  if(gui){
    m <- tktoplevel()
    tkwm.title(m,'OR compared to RR')
    leftFrame = tkframe(m)
    p1Scale <- tkscale(leftFrame,command=refresh,from=0.01,to=0.99,orient='horiz',label='p1',variable=RCenvir$p1Spot,resolution=0.01)
    p2Scale <- tkscale(leftFrame,command=refresh,from=0.01,to=0.99,orient='horiz',label='p2',variable=RCenvir$p2Spot,resolution=0.01)
    tkgrid(leftFrame)
    tkgrid(p1Scale)
    tkgrid(p2Scale)
  }
}
