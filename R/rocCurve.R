rocCurve <- function(x = rnorm(25,10,1), y = rnorm(25,11,2)){
    RCenvir <<- new.env()
    assign("split",tclVar(max(x,y)),envir=RCenvir)
    assign("x",x,envir=RCenvir)
    assign("y",y,envir=RCenvir)
    assign("spec",vector(),envir=RCenvir)
    assign("sens",vector(),envir=RCenvir)
    assign("bestCut",max(x,y),envir=RCenvir)
    assign("bestDist",Inf,envir=RCenvir)
    refresh <- function(...){
        cutoff <- as.numeric(tclvalue(RCenvir$split))
        RCenvir$spec <- c(RCenvir$spec,mean(RCenvir$x > cutoff))
        RCenvir$sens <- c(RCenvir$sens,mean(RCenvir$y > cutoff))
#        par(fig=c(0,0.5,0.5,1),mar=c(5,4,4,2))
        plot(sort(RCenvir$spec),sort(RCenvir$sens),xlab="1-Specificity",ylab = "Sensitivity",main="ROC curve",xlim = c(0,1),ylim = c(0,1),cex.main=1,type='o',pty='s')
        dist <- sqrt((0-RCenvir$spec)^2+(1-RCenvir$sens)^2)
        best <- sort(dist,index.return=TRUE)$ix[1]
        if(min(dist) < RCenvir$bestDist){
            RCenvir$bestDist <- min(dist)
            RCenvir$bestCut <- cutoff
        }
        points(c(0,RCenvir$spec[best]),c(1,RCenvir$sens[best]),type='l',col='purple')
        points(RCenvir$spec[best],RCenvir$sens[best],pch=16,col='purple')
        points(mean(RCenvir$x>cutoff),mean(RCenvir$y>cutoff),pch=16,col='green')

#        par(new=TRUE)
#        par(fig=c(0.5,1,0.5,1),mar=c(5,4,4,2))
#        mat = matrix(c(0,0,0,0),ncol=2,dimnames=list(c("X","Y"),c("X","Y")))
#        mat[1,1] = sum(RCenvir$x < cutoff)
#        mat[1,2] = sum(RCenvir$x >= cutoff)
#        mat[2,1] = sum(RCenvir$y < cutoff)
#        mat[2,2] = sum(RCenvir$y >= cutoff)
#        textplot(mat,cex=1.75,col=1:8)
#        text(0.6,0.9,label="Classed as",pos=1,cex=1.75,col='brown')
#        text(0,0.5,label="True",cex=1.75,pos=4,col='brown')
#        text(0,0.4,label="Class",cex=1.75,pos=4,col='brown')

#        par(new=TRUE)
#        par(fig=c(0,0.80,0,0.5),mar=c(5,3,4,0))
#        xSort = RCenvir$x
#        ySort = RCenvir$y
#        xColor <- rep(2,length(RCenvir$x))
#        yColor <- rep(2,length(RCenvir$y))
#        xColor <- xColor +2*(xSort < cutoff)
 #       yColor <- yColor +2*(ySort >= cutoff)
 #       y <- c(rep(0.2,length(x)),rep(0.8,length(y)))
 #       plot(c(xSort[1],ySort[1]),c(0.2,0.8),pch=16,xlim = c(min(xSort,ySort),max(xSort,ySort)),col=c(xColor[1],yColor[1]),axes=F,ylab='',xlab='Value',main="Classification of x and y",cex.main=1,frame.plot=T)
#        axis(1)
#        axis(2,at=c(0.2,0.8),labels=c("X","Y"))
#        points(c(xSort[1],ySort[1]),c(0.2,0.8),pch=16,type='l',xlim = c(min(xSort,ySort),max(xSort,ySort)),col=1)
#        for(i in 2:length(RCenvir$x)){
#            points(c(xSort[i],ySort[i]),c(0.2,0.8),pch=16,col=c(xColor[i],yColor[i]))
#            points(c(xSort[i],ySort[i]),type='l',c(0.2,0.8),pch=16,col=1)
#        }
#        points(c(cutoff,cutoff),c(-1,2),type='l',col='green')

#        par(new=T)
#        par(fig=c(0.80,1,0,0.5),mar=c(0,0,0,0))
#        plot(0,xlab='',ylab='',axes=FALSE,type='n')
#        legend("center",legend=c("Correct","Incorrect"),col=c("blue","red"),pch=16)
#        points(c(RCenvir$bestCut,RCenvir$bestCut),c(0,1),type='l',col='purple')
    }
    m <- tktoplevel()
    tkwm.title(m,"ROC curve")
    cutFrame <- tkframe(m)
    cutSlider <- tkscale(cutFrame,command=refresh,from=min(x,y),to=max(x,y),resolution = 0.1,orient='horiz',variable=RCenvir$split)

    tkpack(tklabel(cutFrame,text='cutoff:'),side='left')
    tkpack(cutFrame,cutSlider,side='bottom')
}
