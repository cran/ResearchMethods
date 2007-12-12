rocCurve <- function(x = rnorm(25,10,1), y = rnorm(25,11,2)){
    RCenv <<- new.env()
    assign("split",tclVar(max(x,y)),env=RCenv)
    assign("x",x,env=RCenv)
    assign("y",y,env=RCenv)
    assign("spec",vector(),env=RCenv)
    assign("sens",vector(),env=RCenv)
    assign("bestCut",max(x,y),env=RCenv)
    assign("bestDist",Inf,env=RCenv)
    refresh <- function(...){
        cutoff <- as.numeric(tclvalue(RCenv$split))
        RCenv$spec <- c(RCenv$spec,mean(RCenv$x > cutoff))
        RCenv$sens <- c(RCenv$sens,mean(RCenv$y > cutoff))
#        par(fig=c(0,0.5,0.5,1),mar=c(5,4,4,2))
        plot(sort(RCenv$spec),sort(RCenv$sens),xlab="1-Specificity",ylab = "Sensitivity",main="ROC curve",xlim = c(0,1),ylim = c(0,1),cex.main=1,type='o',pty='s')
        dist <- sqrt((0-RCenv$spec)^2+(1-RCenv$sens)^2)
        best <- sort(dist,index.return=TRUE)$ix[1]
        if(min(dist) < RCenv$bestDist){
            RCenv$bestDist <- min(dist)
            RCenv$bestCut <- cutoff
        }
        points(c(0,RCenv$spec[best]),c(1,RCenv$sens[best]),type='l',col='purple')
        points(RCenv$spec[best],RCenv$sens[best],pch=16,col='purple')
        points(mean(RCenv$x>cutoff),mean(RCenv$y>cutoff),pch=16,col='green')

#        par(new=TRUE)
#        par(fig=c(0.5,1,0.5,1),mar=c(5,4,4,2))
#        mat = matrix(c(0,0,0,0),ncol=2,dimnames=list(c("X","Y"),c("X","Y")))
#        mat[1,1] = sum(RCenv$x < cutoff)
#        mat[1,2] = sum(RCenv$x >= cutoff)
#        mat[2,1] = sum(RCenv$y < cutoff)
#        mat[2,2] = sum(RCenv$y >= cutoff)
#        textplot(mat,cex=1.75,col=1:8)
#        text(0.6,0.9,label="Classed as",pos=1,cex=1.75,col='brown')
#        text(0,0.5,label="True",cex=1.75,pos=4,col='brown')
#        text(0,0.4,label="Class",cex=1.75,pos=4,col='brown')

#        par(new=TRUE)
#        par(fig=c(0,0.80,0,0.5),mar=c(5,3,4,0))
#        xSort = RCenv$x
#        ySort = RCenv$y
#        xColor <- rep(2,length(RCenv$x))
#        yColor <- rep(2,length(RCenv$y))
#        xColor <- xColor +2*(xSort < cutoff)
 #       yColor <- yColor +2*(ySort >= cutoff)
 #       y <- c(rep(0.2,length(x)),rep(0.8,length(y)))
 #       plot(c(xSort[1],ySort[1]),c(0.2,0.8),pch=16,xlim = c(min(xSort,ySort),max(xSort,ySort)),col=c(xColor[1],yColor[1]),axes=F,ylab='',xlab='Value',main="Classification of x and y",cex.main=1,frame.plot=T)
#        axis(1)
#        axis(2,at=c(0.2,0.8),labels=c("X","Y"))
#        points(c(xSort[1],ySort[1]),c(0.2,0.8),pch=16,type='l',xlim = c(min(xSort,ySort),max(xSort,ySort)),col=1)
#        for(i in 2:length(RCenv$x)){
#            points(c(xSort[i],ySort[i]),c(0.2,0.8),pch=16,col=c(xColor[i],yColor[i]))
#            points(c(xSort[i],ySort[i]),type='l',c(0.2,0.8),pch=16,col=1)
#        }
#        points(c(cutoff,cutoff),c(-1,2),type='l',col='green')

#        par(new=T)
#        par(fig=c(0.80,1,0,0.5),mar=c(0,0,0,0))
#        plot(0,xlab='',ylab='',axes=FALSE,type='n')
#        legend("center",legend=c("Correct","Incorrect"),col=c("blue","red"),pch=16)
#        points(c(RCenv$bestCut,RCenv$bestCut),c(0,1),type='l',col='purple')
    }
    m <- tktoplevel()
    tkwm.title(m,"ROC curve")
    cutFrame <- tkframe(m)
    cutSlider <- tkscale(cutFrame,command=refresh,from=min(x,y),to=max(x,y),resolution = 0.1,orient='horiz',variable=RCenv$split)

    tkpack(tklabel(cutFrame,text='cutoff:'),side='left')
    tkpack(cutFrame,cutSlider,side='bottom')
}
