# bootstrap demonstration 1: single samples (marbles)
library(tcltk)
bootSingle <- function(){
  par(mar=c(0,0,0,0))
  plot(0,xlim=c(0,10),ylim=c(0,13),xlab='',ylab='',type='n',axes=FALSE)
  abline(h=6.25,lwd=2)
  # add titles here
  text(5,12.5,pos=1,"Bootstrap Sample",cex=3)
  text(5,6,pos=1,"Original Sample",cex=3)
  x=rep(1:9,3)[-27]
  yBot=c(rep(4,9),rep(2.5,9),rep(1,9))[-27]
  yTop=c(rep(10,9),rep(8.5,9),rep(7,9))[-27]
  symbols(x,yBot,circles=rep(0.35,26),add=TRUE,bg='grey75',inches=FALSE)
  points(x,yBot,pch=LETTERS)
  color = rainbow(26,s=0.75)
  #plotting function # 1
  refresh = function(){
    ind = sort(sample(1:26,26,replace=TRUE))
    botCol = 2*(1:26%in%ind) -1*!(1:26%in%ind)
    botCol[botCol==-1] = 'grey75'
    symbols(x,yBot,circles=rep(0.35,26),add=TRUE,bg=botCol,inches=FALSE)
    points(x,yBot,pch=LETTERS)
    symbols(x,yTop,circles=rep(0.35,26),add=TRUE,bg=color[ind],inches=FALSE)
    points(x,yTop,pch=LETTERS[ind])
    BSenv$plotted = 27
  }
  # refresh2 info: need to track new samples and number of points plotted
  BSenv <- new.env()
  BSenv$indSample = sample(1:26,26,replace=TRUE)
  BSenv$plotted = 0
  refresh2 <-  function(...){
    if(BSenv$plotted==25){
      tkconfigure(indivButton,text='Sort the Sample')
    }
    if(BSenv$plotted==26){
      symbols(x,yTop,circles=rep(0.35,26),add=TRUE,bg=color[sort(BSenv$indSample)],inches=FALSE)
      points(x,yTop,pch=LETTERS[sort(BSenv$indSample)])
      botCol = 2*(1:26%in%BSenv$indSample) -1*!(1:26%in%BSenv$indSample)
      botCol[botCol==-1] = 'grey75'
      symbols(x,yBot,circles=rep(0.35,26),add=TRUE,bg=botCol,inches=FALSE)
      points(x,yBot,pch=LETTERS)
      BSenv$plotted = BSenv$plotted+1
      tkconfigure(indivButton,text='SAMPLE Step-by-Step')
      
    }
    else{
      if(BSenv$plotted==27){
        BSenv$plotted=0
        BSenv$indSample = sample(1:26,26,replace=TRUE)
        rect(0,6.5,10,11,density=-1,col='white',border=NA)
      }
      BSenv$plotted = BSenv$plotted+1
      symbols(x[BSenv$plotted],yTop[BSenv$plotted],circles=0.35,add=TRUE,bg=color[BSenv$indSample[BSenv$plotted]],inches=FALSE)
      points(x[BSenv$plotted],yTop[BSenv$plotted],pch=LETTERS[BSenv$indSample[BSenv$plotted]])
      col = rep('grey75',26)
      col[BSenv$indSample[BSenv$plotted]] = 'red'
      symbols(x,yBot,circles=rep(0.35,26),add=TRUE,bg=col,inches=FALSE)
      points(x,yBot,pch=LETTERS)
    }
  }
  
  m = tktoplevel()
  tkwm.title(m,'Simple Bootstrapping')
  allButton <- tkbutton(m,command=refresh,text='SAMPLE All')
  indivButton <- tkbutton(m,command=refresh2,text='SAMPLE Step-by-Step')
  tkgrid(allButton,padx=100,pady=10)
  tkgrid(indivButton,padx=100,pady=10)
}

