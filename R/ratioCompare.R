# a function to compare RR and OR

ratioCompare <- function(){
  RCenv <<- new.env()
  RCenv$p1 = RCenv$p2 = seq(0,1,length.out=102)[2:101]
  RCenv$RR = RCenv$p1%*%t(1/RCenv$p2)
  RCenv$OR = (RCenv$p1/(1-RCenv$p1))%*%t((1-RCenv$p2)/RCenv$p2)
  RCenv$diff = round(abs(log(RCenv$OR)-log(RCenv$RR)),4)
  RCenv$colors = rainbow(100,start=3/6,end=0)
  RCenv$breaks = quantile(RCenv$diff,probs=seq(0,1,1/100))
  assign("p1Spot",tclVar(0.5),env=RCenv)
  assign("p2Spot",tclVar(0.5),env=RCenv)  
  refresh <- function(...){
    p1S = as.numeric(tclvalue(RCenv$p1Spot))
    p2S = as.numeric(tclvalue(RCenv$p2Spot))
    par(fig=c(0.8,1,0,1),mar=c(5,0,4,0))
    plot(0,xlim=c(0,1),ylim=c(0,100),type='n',xlab='',ylab='',main='Difference',axes=F)
    n = 15
    leg = seq(0,100,length.out=n)
    rect(rep(0,n),leg[1:(n-1)],rep(0.5,n),leg[2:n],density=-1,col=RCenv$colors[leg])
    text(x=rep(0.75,n),y=leg[1:(n-1)],pos=3,labels=round(RCenv$breaks[leg],2))
    par(new=T,fig=c(0,0.8,0,1),mar=c(5.1,4.1,4.1,1.1))
    image(RCenv$p1,RCenv$p2,RCenv$diff,col=RCenv$colors,breaks=RCenv$breaks,xlab='p1',ylab='p2',main = 'log(OR) - log(RR)')
    points(p1S,p2S,pch=16,col='green')
    side = 0
    if(p1S > 0.7)
      side = 1
    height = 1
    if(p2S < 0.15)
      height = 0
    legend(p1S,p2S,xjust=side,yjust=height,legend=c(paste("OR:",round((p1S/(1-p1S))/(p2S/(1-p2S)),4)),paste("RR:",round(p1S/p2S,4))),bg='white')
  }
  refresh()
  m <- tktoplevel()
  tkwm.title(m,'OR compared to RR')
  leftFrame = tkframe(m)
  p1Scale <- tkscale(leftFrame,command=refresh,from=0.01,to=0.99,orient='horiz',label='p1',variable=RCenv$p1Spot,resolution=0.01)
  p2Scale <- tkscale(leftFrame,command=refresh,from=0.01,to=0.99,orient='horiz',label='p2',variable=RCenv$p2Spot,resolution=0.01)
  tkgrid(leftFrame)
  tkgrid(p1Scale)
  tkgrid(p2Scale)
}
