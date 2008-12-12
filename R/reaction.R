# Paul Corey's Reaction Time Game
reactGame <- function(){
  color <- tclVar(0)
  save <- tclVar(0)
  background <- tclVar(0)

  draw <- function(x,y,rX,rY=rX,lev=0.25,border='black',col='red',lty=1,lwd=1){
    dat = ellipse(x=0,centre=c(x,y),level=lev,scale=c(rX,rY))
    polygon(dat[,1],dat[,2],density=-1,col=col,border=border,lty=lty,lwd=lwd)
  }
  reaction <- function(id="000000",color = "red",runs=10){
    dat = data.frame(matrix(rep(0,6*runs),ncol=6),row.names=1:runs)
    names(dat) <- c("ID","reaction time","color","bg","x","y")
    plot(0,xlim=c(0,10),ylim=c(0,10),axes=F,xlab='',ylab='',main='',type='n')
    bg = par('bg')
    text(5,5,pos=3,cex=4,labels='ARE YOU READY?')
    text(5,5,pos=1,cex=2,labels='click to start')
    l <- locator(1,type='n')
    par(mar=c(0,0,0,0))
    plot(0,xlim=c(0,10),ylim=c(0,10),axes=F,xlab='',ylab='',main='',type='n')
    for(i in 1:runs){
      x = runif(1,1,9)
      y = runif(1,1,9)
      rX = runif(1,0.5,min(abs(10-x),x))
      rY = runif(1,0.5,min(abs(10-y),y))
      level = 0.25
      Sys.sleep(runif(1,1,7))
      start = Sys.time()
      draw(x,y,rX,rY,level,col=color,border=NA)
      l <- locator(1,type='n')
      end = Sys.time()
      if(bg!='white')
        back='colored'
      else
        back='clear'
      if(color==3)
        color='green1'
      else if(color==2)
        color='red1'
      dat[i,] = c(id,as.numeric(end-start),color,back,x,y)
      draw(x,y,rX,rY,level,col=bg,border=bg)    
    }
    text(5,9,pos=3,cex=4,labels='The Game is Done!')
    dat
  }

  refresh <- function(){
    col = switch(tclvalue(color),
      '0' = 2,
      '1' = 3)    
    ID = tclvalue(tkget(idEntry))
    back = as.numeric(tclvalue(background))*col
    if(back==2){
      back='red2'
      col='red1'
    }
    else if(back==3){
      back='green2'
      col='green1'
    }
    else{
      back='white'
    }
    if(ID == ''){
      ID = '000000'
    }
    par(bg=back)
    d = reaction(ID,color=col,runs=10)
    write = as.numeric(tclvalue(save))
    if(write){
      file = tclvalue(tkget(saveEntry))
      if(file=='')
        file='responseData.txt'
      write.table(d,file=file,append=TRUE,sep=',',quote=F,col.names=F,row.names=F)
      text(5,9,pos=1,cex=2.5,labels='ANOVA table for reaction time data')
      allDat = read.csv(file,header=F)
      names(allDat) = c("ID","reactionTime","color","bg","x","y")
      mod = aov(reactionTime~color*bg,data=allDat)
      t1 = round(myAOV(mod,collapse=1:3),3)
      t2 = round(myAOV(mod)[1:3,],3)
      t3 = table(allDat$color,allDat$bg)
      ypts = c(7.5,7,6.5)
      ypts2 = c(5.5,5,4.5)
      xpts = c(4,5,6,7,8)
      for(i in 1:3){
        for(j in 1:6){
          text(xpts[j],ypts[i],t1[i,j])
          text(xpts[j],ypts2[i],t2[i,j])
        }
      }
      text(xpts,8,labels=names(t1))
      text(3,ypts,labels=row.names(t1))
      text(xpts,6,labels=names(t2))
      text(3,ypts2,labels=row.names(t2))
      text(c(7,7,8,8),c(2,1,2,1),labels=t3)
      text(c(7,8),3,labels=c("white","colored"))
      text(6,c(1,2),labels=c("green","red"))
      rect(0.5,0.5,4,4,density=-1,col='white')
      means = tapply(allDat$reactionTime,interaction(allDat$color,allDat$bg,lex.order=T),mean)
      means = means*(2/max(means))+0.75
      points(c(1,3),means[1:2],type='o',col='red',pch=16)
      points(c(1,3),means[3:4],type='o',col='green',pch=16)
      points(c(1,3),c(0.4,0.4),pch='|')
      text(c(1,3),c(0.4,0.4),pos=1,labels=c('clear','colored'))
      text(2.25,4,pos=1,labels='Reaction Times',cex=1.25)
    }
    else{
      print(d)
    }
  }

  m <- tktoplevel()
  tkwm.title(m,"Reaction Time Game")
  colFrame <- tkframe(m)
  saveFrame <- tkframe(m)
  idFrame <- tkframe(m)
  bgFrame <- tkframe(m)
  idEntry <- tkentry(idFrame,width=20,bg='white')
  colButton0 <- tkradiobutton(colFrame,variable=color,value=0,text='red')
  colButton1 <- tkradiobutton(colFrame,variable=color,value=1,text='green')
  saveButton0 <- tkradiobutton(saveFrame,variable=save,value=0,text='practice')
  saveButton1 <- tkradiobutton(saveFrame,variable=save,value=1,text='save results')
  bgButton0 <- tkradiobutton(bgFrame,variable=background,value=0,text='white')
  bgButton1 <- tkradiobutton(bgFrame,variable=background,value=1,text='colored')
  saveEntry <- tkentry(saveFrame,width=20,bg='white')
  runButton <- tkbutton(m,command=refresh,text='Play Game!')

  tkgrid(idFrame,columnspan=2)
  tkgrid(tklabel(idFrame,text='Enter your ID'),idEntry)
  tkgrid(colFrame,bgFrame,saveFrame)
  tkgrid(colButton0)
  tkgrid(colButton1)
  tkgrid(bgButton0)
  tkgrid(bgButton1)
  tkgrid(saveButton0)
  tkgrid(saveButton1,saveEntry)
  tkgrid(runButton,columnspan=2)
}
