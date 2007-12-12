confInt <- function(...){
    CIenv <<- new.env()
    assign("mean",tclVar(0),env=CIenv)
    assign("sig",tclVar(1),env=CIenv)
    assign("n",tclVar(25),env=CIenv)
    assign("reps",tclVar(25),env=CIenv)
    assign("alpha",tclVar(0.05),env=CIenv)
    assign("upperTail",tclVar(1),env=CIenv)
    assign("lowerTail",tclVar(1),env=CIenv)
    mat <- matrix(rnorm(25*25,0,1),ncol=25)
    assign("mat",mat,env=CIenv)
    resample <- function(...){
        n <- as.numeric(tclvalue(tkget(CIenv$nEntry)))
        reps <- as.numeric(tclvalue(tkget(CIenv$repEntry)))
        m <- as.numeric(tclvalue(CIenv$mean))
        sig <- as.numeric(tclvalue(CIenv$sig))
        mat <- matrix(rnorm(reps*n,mean=m,sd=sig),ncol=n)
        assign("mat",mat,env=CIenv)
        refresh()
    }
    refresh <- function(...){
        plot.new()
        mat <- CIenv$mat
        means <- rowMeans(mat)
        n <- as.numeric(tclvalue(CIenv$n))
        sd <- sqrt(diag(var(t(mat)))/n)
        z <- qnorm(1-as.numeric(tclvalue(CIenv$alpha))/2)
        m <- as.numeric(tclvalue(CIenv$mean))
        yLim <- c(-3.5,3.5)*max(sd)+m
        up <- as.numeric(tclvalue(CIenv$upperTail))
        low <- as.numeric(tclvalue(CIenv$lowerTail))
        plotCI(means,uiw=z*sd*up,liw=z*sd*low,ylim=yLim,pch=20)
        sig <- as.numeric(tclvalue(CIenv$sig))/sqrt(n)
        lLim = m-z*sig
        uLim = m+z*sig
        abline(lLim,0,col='green')
        abline(uLim,0,col='green')
        abline(m,0,col='blue')
        outside <- (means > uLim) + (means < lLim)
        if(sum(outside)>0){
            means[outside==0]=NA
            sd[outside==0]=NA
            par(new=TRUE)
            plotCI(means,uiw=z*sd,col='red',ylim=yLim) 
        }
    }

    m <- tktoplevel()
    tkwm.title(m,"Confidence Interval Demonstration")
    meanSlider <- tkscale(m,from=-10,to=10,label="mean",orient="horiz",resolution=0.5,variable=CIenv$mean)
    sigSlider <- tkscale(m,from=0,to=10,label="sigma",orient="horiz",resolution=0.2,variable=CIenv$sig)
    alphaSlider <- tkscale(m,command=refresh,from=0.01,to=0.99,label="alpha",orient="horiz",resolution=0.01,variable=CIenv$alpha)
    entryFrame <- tkframe(m)
    nEntry <- tkentry(entryFrame,width=5,bg='white')
    repEntry <- tkentry(entryFrame,width=5,bg='white')
    sampleButton <- tkbutton(m,command=resample,text='Re-sample')
    tkinsert(nEntry,0,"25")
    tkinsert(repEntry,0,"25")
    
    tkpack(meanSlider,side="top")
    tkpack(sigSlider,side="top")
    tkpack(alphaSlider,side="top")
    tkpack(tklabel(entryFrame,text="n:"),side="left")
    tkpack(entryFrame,nEntry,side="left")
    tkpack(tklabel(entryFrame,text="reps:"),side="left")
    tkpack(entryFrame,repEntry,side="top")
    tkpack(sampleButton,side='top')

    assign("nEntry",nEntry,env=CIenv)
    assign("repEntry",repEntry,env=CIenv)
}
