# a third bootstrapping function, demonstrating the power of bootstrapping
# many many times
bootSequence <- function(dat){
  BSenv <- new.env()
  par(mar=c(1,0,1,0))
  plot(0,xlim=c(0,12),ylim=c(0,12),xlab='',ylab='',type='n',axes=FALSE)
  x = c(0,6,10,12)
  y = c(0,6,12)
  BSenv$means = vector()
  BSenv$n = 0
  myHist(x[1],x[2],x[2],y[3],dat,sample=FALSE)
  text(mean(x[1:2]),y[3],pos=3,labels='Population')
  text(mean(x[c(1,4)]),y[1],pos=1,labels='Means of Bootstrap Samples')
  text(mean(x[c(2,4)]),y[3],pos=3,labels='Newest Bootstrap Sample')
  refresh <- function(){
    samp = sample(dat,length(dat),replace=TRUE)
    BSenv$n=BSenv$n+1
    myHist(x[2],y[2],x[4],y[3],samp)
    BSenv$means = c(BSenv$means,mean(samp))
    info = myHist(x[1],y[1],x[3],y[2],BSenv$means,CIs=TRUE)
    yPts = seq(y[1],y[2],length.out=5)[2:4]
    rect(x[3],y[1],x[4],y[2],density=-1,col='white')
    text(x[3],yPts[3],pos=4,labels=paste("n =",BSenv$n),cex=1.25)
    text(x[3],yPts[2],pos=4,labels=paste("UCL:",signif(info$CI[2],3)))
    text(x[3],yPts[1],pos=4,labels=paste("LCL:",signif(info$CI[1],3)))
  }
  tenRefresh <- function(){
    rect(x[2],y[2],x[4],y[3],density=-1,col='white')
    for(i in 1:10){
      samp = sample(dat,length(dat),replace=TRUE)
      BSenv$means = c(BSenv$means,mean(samp))
      BSenv$n=BSenv$n+1
    }
    info = myHist(x[1],y[1],x[3],y[2],BSenv$means,CIs=TRUE)
    yPts = seq(y[1],y[2],length.out=5)[2:4]
    rect(x[3],y[1],x[4],y[2],density=-1,col='white')
    text(x[3],yPts[3],pos=4,labels=paste("n =",BSenv$n),cex=1.25)
    text(x[3],yPts[2],pos=4,labels=paste("UCL:",signif(info$CI[2],3)))
    text(x[3],yPts[1],pos=4,labels=paste("LCL:",signif(info$CI[1],3)))
  }
  hundredRefresh <- function(){
    rect(x[2],y[2],x[4],y[3],density=-1,col='white')
    for(i in 1:100){
      samp = sample(dat,length(dat),replace=TRUE)
      BSenv$means = c(BSenv$means,mean(samp))
      BSenv$n=BSenv$n+1
    }
    info = myHist(x[1],y[1],x[3],y[2],BSenv$means,CIs=TRUE)
    yPts = seq(y[1],y[2],length.out=5)[2:4]
    rect(x[3],y[1],x[4],y[2],density=-1,col='white')
    text(x[3],yPts[3],pos=4,labels=paste("n =",BSenv$n),cex=1.25)
    text(x[3],yPts[2],pos=4,labels=paste("UCL:",signif(info$CI[2],3)))
    text(x[3],yPts[1],pos=4,labels=paste("LCL:",signif(info$CI[1],3)))
  }
  m <- tktoplevel()
  tkwm.title(m,"Bootstrapping")
  resampleButton <- tkbutton(m,command=refresh,text='resample')
  tenButton <- tkbutton(m,command=tenRefresh,text='take 10 samples')
  hundredButton <- tkbutton(m,command=hundredRefresh,text='take 100 samples')
  tkgrid(resampleButton,padx=100,pady=10,columnspan=2)
  tkgrid(tenButton,hundredButton)
}
