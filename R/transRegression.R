transRegression <- function(y,x,weights=rep(1,length(y))){
    TRenv <<- new.env()
    assign("constant",5,env=TRenv)
    assign("n",length(y),env=TRenv)
    assign("y",y,env=TRenv)
    assign("newY",y,env=TRenv)
    assign("x",x,env=TRenv)
    assign("w",weights,env=TRenv)
    assign("color",2,env=TRenv)

    change <- function(...){
        weight = FALSE
        trans = FALSE
        index <- as.numeric(tclvalue(tkcurselection(TRenv$weightsList)))
        if(!is.na(index)){
            weight = TRUE
            typeWeight = tclvalue(tkget(TRenv$weightsList,index))
        }
        else {
            index <- as.numeric(tclvalue(tkcurselection(TRenv$transList)))
            if(!is.na(index)){
                trans=TRUE
                typeTrans = tclvalue(tkget(TRenv$transList,index))
            }
        }
        if(weight){
            switch(typeWeight,
                "constant" = TRenv$w <- rep(TRenv$constant,TRenv$n),
                "input" = TRenv$w <- weights,
                "non-weighted" = TRenv$w <- rep(1,TRenv$n))
        }
        if(trans){
            switch(typeTrans,
                "logistic" = TRenv$newY <- log(TRenv$y),
                "square root" = TRenv$newY <- sqrt(TRenv$y),
                "inverse" = TRenv$newY <- 1/TRenv$y,
                "non-transformed" = TRenv$newY <- TRenv$y)
        }
        refresh()
    }

    refresh <- function(...){
        model <- lm(TRenv$newY~TRenv$x,weights=TRenv$w)
        par(mfrow=c(2,1))
        plot(TRenv$x,TRenv$newY,pch=16,col="blue")
        abline(model$coeff[1],model$coeff[2],col=TRenv$color)
        s = sqrt(sum(model$resid^2)/(length(TRenv$y)-2))
        plot(model$fit,model$resid,pch=16,ylim=c(-2.5*s,2.5*s),col='red')
        abline(2*s,0,col='blue')
        abline(-2*s,0,col='blue')
        assign("model",model,env=TRenv)
    }
    m <- tktoplevel()
    frame1 <- tkframe(m)
    frame2 <- tkframe(m)
    weightsList <- tklistbox(frame1,height=-1,width=15)
    transList <- tklistbox(frame2,height=-1,width=15)
    plotButton <- tkbutton(m,command=change,text="Plot")
    tkinsert(weightsList,"end","constant")
    tkinsert(weightsList,"end","input")
    tkinsert(weightsList,"end","non-weighted")
    tkinsert(transList,"end","logistic")
    tkinsert(transList,"end","inverse")
    tkinsert(transList,"end","square root")
    tkinsert(transList,"end","non-transformed")
    tkpack(tklabel(frame1,text="weights"),side="top")
    tkpack(frame1,weightsList,side="left")
    tkpack(tklabel(frame2,text="transformations"),side="top")
    tkpack(frame2,transList,side="left")
    tkpack(plotButton,side="bottom")

    assign("weightsList",weightsList,env=TRenv)
    assign("transList",transList,env=TRenv)

#    tkbind(weightsList,"ButtonPress-1",change())
    
}
