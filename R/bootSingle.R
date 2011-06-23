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
    BSenvir$plotted = 27
  }
  # refresh2 info: need to track new samples and number of points plotted
  BSenvir <- new.env()
  BSenvir$indSample = sample(1:26,26,replace=TRUE)
  BSenvir$plotted = 0
  refresh2 <-  function(...){
    if(BSenvir$plotted==25){
      tkconfigure(indivButton,text='Sort the Sample')
    }
    if(BSenvir$plotted==26){
      symbols(x,yTop,circles=rep(0.35,26),add=TRUE,bg=color[sort(BSenvir$indSample)],inches=FALSE)
      points(x,yTop,pch=LETTERS[sort(BSenvir$indSample)])
      botCol = 2*(1:26%in%BSenvir$indSample) -1*!(1:26%in%BSenvir$indSample)
      botCol[botCol==-1] = 'grey75'
      symbols(x,yBot,circles=rep(0.35,26),add=TRUE,bg=botCol,inches=FALSE)
      points(x,yBot,pch=LETTERS)
      BSenvir$plotted = BSenvir$plotted+1
      tkconfigure(indivButton,text='SAMPLE Step-by-Step')
      
    }
    else{
      if(BSenvir$plotted==27){
        BSenvir$plotted=0
        BSenvir$indSample = sample(1:26,26,replace=TRUE)
        rect(0,6.5,10,11,density=-1,col='white',border=NA)
      }
      BSenvir$plotted = BSenvir$plotted+1
      symbols(x[BSenvir$plotted],yTop[BSenvir$plotted],circles=0.35,add=TRUE,bg=color[BSenvir$indSample[BSenvir$plotted]],inches=FALSE)
      points(x[BSenvir$plotted],yTop[BSenvir$plotted],pch=LETTERS[BSenvir$indSample[BSenvir$plotted]])
      col = rep('grey75',26)
      col[BSenvir$indSample[BSenvir$plotted]] = 'red'
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

