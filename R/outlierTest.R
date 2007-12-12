outlierTest <- function(y,x,output=FALSE){
    out = vector(length=0)
    xLim = range(x)
    yLim = range(y)
    mapDist = diff(xLim)^2+diff(yLim)^2
    thresh = mapDist/5000
    plotColor = rep(3,length(x))
    while(1){
        if(length(out)>0) model = lm(y[-out]~x[-out])
        else model = lm(y~x)
        if(output) print(summary(model))
        result = anova(model)
        plot.new()
        par(fig=c(0.75,1,0,1),mar=c(5,0,4,0))
        plot(0,xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",axes=FALSE,type='n')
        leg = c(paste("y = ",round(model$coeff[1],2)," + ",round(model$coeff[2],2),"x",sep=""),
                paste("s = ",round(sqrt(sum(model$resid^2)/(length(model$resid)-2)),2)),
                paste("F-value = ",round(result$F[1],2)),
                paste("p-value = ",round(result$P[1],2)))
        legend("center",legend=leg,pch='',col='black',bty='n')
        legend("top",legend=c("real data points","added points","inactive points"),pch=16,col=c(3,4,2))
        par(new=TRUE)
        par(fig=c(0,0.75,0,1),mar=c(5,4,4,0))
        plot(x,y,xlim=xLim,ylim=yLim)
        if(length(out) > 0){
            points(x[-out],y[-out],pch=16,col=plotColor[-out])
            points(x[out],y[out],pch=16,col='red')
        }
        else
            points(x,y,pch=16,col=plotColor)
        abline(model$coeff[1],model$coeff[2],col='black',lwd=3)
        #trying to use locator() rather than identify
        l = locator(1,type='n')
        dist = (l$x-x)^2+(l$y-y)^2
        if(min(dist)<thresh){
            point = sort(dist,index.return=TRUE)$ix[1]
            if(sum(out==point)==0) out = c(out,point)
            else out = out[out != point]
        }
        else{
            x = c(x,l$x)
            y = c(y,l$y)
            plotColor = c(plotColor,4)
        }
    }
}
