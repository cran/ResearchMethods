`rhoRange` <-
function(x,y,gui=TRUE,xMin=NaN,xMax=NaN,perc=NaN){
    RRenvir <<- new.env()
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
    assign("dat",dat,envir=RRenvir)
    assign("xLim",range(dat[,1]),envir=RRenvir)
    assign("yLim",range(dat[,2]),envir=RRenvir)
    assign("minX",tclVar(xMin),envir=RRenvir)
    assign("maxX",tclVar(xMax),envir=RRenvir)
    assign("perc",tclVar(perc),envir=RRenvir)
    refresh <- function(...){
        color = (RRenvir$dat[,1] <= as.numeric(evalq(tclvalue(RRenvir$maxX)))) - (RRenvir$dat[,1] < as.numeric(evalq(tclvalue(RRenvir$minX))))
        dat2 = RRenvir$dat[color==1,]
        color = 3-color-(color==-1)
        plot(RRenvir$dat[,1],RRenvir$dat[,2],xlim = RRenvir$xLim, ylim = RRenvir$yLim, xlab=names(RRenvir$dat)[1],ylab=names(RRenvir$dat)[2],pch=16,col=color) 
        m1 <- lm(RRenvir$dat[,2] ~ RRenvir$dat[,1])
        abline(m1$coeff[1],m1$coeff[2],col="green")
        rho1 = round(cor(RRenvir$dat)[1,2],2)
        xFull = max(RRenvir$xLim)*.9
        yFull = m1$coeff[1]+m1$coeff[2]*xFull
        if(yFull > max(RRenvir$yLim)){
            yFull = 0.9*max(RRenvir$yLim)
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
            xRed = as.numeric(evalq(tclvalue(RRenvir$maxX)))*.9
            yRed = m2$coeff[1]+m2$coeff[2]*xRed
            if(yRed > max(RRenvir$yLim)){
                yRed = 0.9*max(RRenvir$yLim)
                xRed = (yRed - m2$coeff[2])/m2$coeff[1]
            }
            text(xRed,yRed,substitute(paste(rho,"=",r),list(r=rho2)),col="red")
        }
        
    }    
    percent <- function(...){
        p = as.numeric(evalq(tclvalue(RRenvir$perc)))
        x = RRenvir$dat[,1]
        n = length(x)
        p1 = round((n-p*n)/2,0)
        if(p1==0) p1=1
        p2 = round((n+p*n)/2,0)
        tkset(xMinScale,x[p1])
        tkset(xMaxScale,x[p2])
        assign("minX",tclVar(x[p1]),envir=RRenvir)
        assign("maxX",tclVar(x[p2]),envir=RRenvir)
        evalq(tkconfigure(xMinScale,variable=minX),envir=RRenvir)
        evalq(tkconfigure(xMaxScale,variable=maxX),envir=RRenvir)
        refresh()      
    }
    move <- function(...){
        low = as.numeric(evalq(tclvalue(RRenvir$minX)))
        high = as.numeric(evalq(tclvalue(RRenvir$maxX)))
        p = as.numeric(evalq(tclvale(RRenvir$perc)))
        x = RRenvir$dat[,1]
        n = length(x)
        p = sum((x <= high) - (x < low))
        tkset(percentScale,p/n)
        assign("perc",tclVar(sum(p)/n),envir=RRenvir)
        evalq(tkconfigure(percentScale,variable=perc),envir=RRenvir)
        refresh()  
    }
    if(gui){
        m <- tktoplevel()
        tkwm.title(m,"Effect of Data Size on Rho")
        xMinScale <- tkscale(m, command=refresh,from=.9*min(RRenvir$xLim),to=1.1*max(RRenvir$xLim),orient="horiz",label=paste("Lowest ",names(dat)[1]),resolution=0.1,variable=RRenvir$minX)
        xMaxScale <- tkscale(m, command=refresh,from=.9*min(RRenvir$xLim),to=1.1*max(RRenvir$xLim),orient="horiz",label=paste("Highest ",names(dat)[1]),resolution=0.1,variable=RRenvir$maxX)
        percentScale <- tkscale(m, command=percent,from=0,to = 1,orient="horiz",label="proportion of points",resolution=0.01,length=125,variable=RRenvir$perc)
        tkpack(xMinScale,side="top")
        tkpack(xMaxScale,side="top")
        tkpack(percentScale,side="bottom")
    }
    else{ refresh() }
}

