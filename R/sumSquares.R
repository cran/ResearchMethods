sumSquares <- function(x,y){
    SSenvir <<- new.env()
    assign("X",x,envir=SSenvir)
    assign("Y",y,envir=SSenvir)
    X <- SSenvir$X
    Y <- SSenvir$Y
    model <- lm(Y~X)
    SSenvir$intercept <- model$coeff[1]
    oldIntercept <- Inf
    SSenvir$slope <- model$coeff[2]
    oldSlope <- Inf
    colors <- rainbow(length(X))
    lim <- .05*diff(range(y))
    refresh <- function(l){
#      cat(":",SSenvir$intercept,SSenvir$slope,":")
#      cat(":",l$x,l$y,":")
      if(!is.na(l$x) && !is.na(l$y) && abs(SSenvir$intercept+SSenvir$slope*l$x-l$y)<lim){
        if(l$x < min(X)){
          abline(SSenvir$intercept,SSenvir$slope,col='blue')
          l3 <- locator(1,type='n')
#          cat(":l3",l3$x,l3$y)
          SSenvir$intercept <- l3$y - SSenvir$slope*l3$x
        }
        else{
          abline(SSenvir$intercept,SSenvir$slope,col='red')
          l2 <- locator(1,type='n')
#          cat(":l2",l2$x,l2$y)
          SSenvir$slope <- (l2$y-SSenvir$intercept)/l2$x
        }
      }
      else if(!is.na(l$x) && !is.na(l$y) && abs(l$x-mean(c(0,max(X))))/max(X) < .1 && l$y > max(Y)){
        SSenvir$intercept <- model$coeff[1]
        oldIntercept <- Inf
        SSenvir$slope <- model$coeff[2]
        oldSlope <- Inf
      }
      if(SSenvir$slope!=oldSlope || SSenvir$intercept != oldIntercept){
        # Residual Plots
        par(fig=c(0.5,1,0.3,1),mar=c(5,3,4,1))
        plot(X,Y,main="Residual Squares",cex.main=1,pch=20,col=colors,xlim=c(0,max(X)),ylim=c(0,max(Y)))
        abline(SSenvir$intercept,SSenvir$slope,lwd=2)
        fit <- SSenvir$intercept+SSenvir$slope*X
        resid <- Y - fit
        index <- sort(resid^2,decreasing=T,index.return=T)$ix
        for(i in index){
            side <- resid[i]
            rect(X[i]-side,fit[i],X[i],fit[i]+side,density=-1,col=colors[i])
        }
        # Legend and RSS
        par(new=T)
        par(fig=c(0,1,0,0.3),mar=c(0,0,2,0))
        plot(0,1,type='n',axes=F,xlab="",ylab="",main="Area of the Squares (i.e. Sum of Residuals Squared)",cex.main=1)
        legend("top",legend=round(resid^2,2),pch=15,col=colors,ncol=7)
        legend("bottom",legend=paste("Total Sum of Squares:",round(sum(resid^2))),pch=" ",bty='n')
        # Main Plot
        par(new=T)
        par(fig=c(0,0.5,0.3,1),mar=c(5,3,4,1))
        plot(X,Y,col=colors,xlim=c(0,max(x)),ylim=c(0,max(y)),pch=16,main="Regression Plot",cex.main=1)
        mtext("RESET",side=3,font=2,cex=1.5)
        abline(SSenvir$intercept,SSenvir$slope,lwd=2,font=2)
      }
    }
    refresh(list(x=NA,y=NA))
    while(1){
      l <- locator(1,type='n')
#      cat(":l",l$x,l$y)
      refresh(l)
    }
}
