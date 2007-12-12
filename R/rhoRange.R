`rhoRange` <-
function(x,y,gui=TRUE,xMin=NaN,xMax=NaN,perc=NaN){
    RRenv <<- new.env()
    ind = sort(x,index.return=TRUE)$ix
    dat = cbind(x[ind],y[ind])
    if(is.nan(xMin)){
        if(is.nan(perc)){
            xMin = min(range(dat[,1]))*1.25
        }
        else{
            x = dat[,1]
            n = length(x)
            p1 = round((n-perc*n)/2,0)
            if(p1==0) p1=1
            p2 = round((n+perc*n)/2,0)
            xMin = x[p1]
            xMax = x[p2]
        }
    }
    if(is.nan(xMax)){
        if(is.nan(perc)){
            xMax = max(range(dat[,1]))*.75
        }
    }
    if(is.nan(perc)){
        perc = sum(sum(dat[,1]<=xMax)-sum(dat[,1]<=xMin)+sum(dat[,1]==-1))/dim(dat)[1]
    }
    else if(perc > 1 || perc < 0){
        stop("Perc must be between 0 and 1\n")
    }

    dat = cbind(dat,(1:dim(dat)[1])/dim(dat)[1])
    assign("dat",dat,env=RRenv)
    assign("xLim",range(dat[,1]),env=RRenv)
    assign("yLim",range(dat[,2]),env=RRenv)
    assign("minX",tclVar(xMin),env=RRenv)
    assign("maxX",tclVar(xMax),env=RRenv)
    assign("perc",tclVar(perc),env=RRenv)
    refresh <- function(...){
        color = (RRenv$dat[,1] <= as.numeric(evalq(tclvalue(RRenv$maxX)))) - (RRenv$dat[,1] < as.numeric(evalq(tclvalue(RRenv$minX))))
        dat2 = RRenv$dat[color==1,]
        color = 3-color-(color==-1)
        plot(RRenv$dat[,1],RRenv$dat[,2],xlim = RRenv$xLim, ylim = RRenv$yLim, xlab=names(RRenv$dat)[1],ylab=names(RRenv$dat)[2],pch=16,col=color) 
        m1 <- lm(RRenv$dat[,2] ~ RRenv$dat[,1])
        abline(m1$coeff[1],m1$coeff[2],col="green")
        rho1 = round(cor(RRenv$dat)[1,2],2)
        xFull = max(RRenv$xLim)*.9
        yFull = m1$coeff[1]+m1$coeff[2]*xFull
        if(yFull > max(RRenv$yLim)){
            yFull = 0.9*max(RRenv$yLim)
            xFull = (yFull - m1$coeff[2])/m1$coeff[1]
        }
        text(xFull,yFull,substitute(paste(rho,"=",r),list(r=rho1)),col="dark green")
        if(dim(dat2)[1] > 0){
            m2 <- lm(dat2[,2]~dat2[,1])
            if(is.na(m2$coeff[2])){
                m2$coeff[2]=0
            }
            abline(m2$coeff[1],m2$coeff[2],col="darkred")
            rho2 = round(cor(dat2)[1,2],2)
            xRed = as.numeric(evalq(tclvalue(RRenv$maxX)))*.9
            yRed = m2$coeff[1]+m2$coeff[2]*xRed
            if(yRed > max(RRenv$yLim)){
                yRed = 0.9*max(RRenv$yLim)
                xRed = (yRed - m2$coeff[2])/m2$coeff[1]
            }
            text(xRed,yRed,substitute(paste(rho,"=",r),list(r=rho2)),col="red")
        }
        
    }    
    percent <- function(...){
        p = as.numeric(evalq(tclvalue(RRenv$perc)))
        x = RRenv$dat[,1]
        n = length(x)
        p1 = round((n-p*n)/2,0)
        if(p1==0) p1=1
        p2 = round((n+p*n)/2,0)
        tkset(xMinScale,x[p1])
        tkset(xMaxScale,x[p2])
        assign("minX",tclVar(x[p1]),env=RRenv)
        assign("maxX",tclVar(x[p2]),env=RRenv)
        evalq(tkconfigure(xMinScale,variable=minX),env=RRenv)
        evalq(tkconfigure(xMaxScale,variable=maxX),env=RRenv)
        refresh()      
    }
    move <- function(...){
        low = as.numeric(evalq(tclvalue(RRenv$minX)))
        high = as.numeric(evalq(tclvalue(RRenv$maxX)))
        p = as.numeric(evalq(tclvale(RRenv$perc)))
        x = RRenv$dat[,1]
        n = length(x)
        p = sum((x <= high) - (x < low))
        tkset(percentScale,p/n)
        assign("perc",tclVar(sum(p)/n),env=RRenv)
        evalq(tkconfigure(percentScale,variable=perc),env=RRenv)
        refresh()  
    }
    if(gui){
        m <- tktoplevel()
        tkwm.title(m,"Effect of Data Size on Rho")
        xMinScale <- tkscale(m, command=refresh,from=.9*min(RRenv$xLim),to=1.1*max(RRenv$xLim),orient="horiz",label=paste("Lowest ",names(dat)[1]),resolution=0.1,variable=RRenv$minX)
        xMaxScale <- tkscale(m, command=refresh,from=.9*min(RRenv$xLim),to=1.1*max(RRenv$xLim),orient="horiz",label=paste("Highest ",names(dat)[1]),resolution=0.1,variable=RRenv$maxX)
        percentScale <- tkscale(m, command=percent,from=0,to = 1,orient="horiz",label="proportion of points",resolution=0.01,length=125,variable=RRenv$perc)
        tkpack(xMinScale,side="top")
        tkpack(xMaxScale,side="top")
        tkpack(percentScale,side="bottom")
    }
    else{ refresh() }
}

