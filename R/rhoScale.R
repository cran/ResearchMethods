`rhoScale` <-
function(x,y,gui=TRUE,sc=1,sh=0,xlab='x',ylab='y'){
    RSenvir <<- new.env()
    scale = sc
    shift = sh
    assign("scale",tclVar(scale),envir = RSenvir)
    assign("shift",tclVar(shift),envir = RSenvir)
    assign("x",x,envir = RSenvir)
    assign("y",y,envir = RSenvir)    
    assign("xLab",xlab,envir = RSenvir)
    assign("yLab",ylab,envir = RSenvir)
    limx = range(x)
    limx = c(0.5*limx[1],2*limx[2])
    limy = range(y)
    limy = c(0.5*limy[1],2*limy[2])
    assign("limx",limx,envir = RSenvir)
    assign("limy",limy,envir = RSenvir)
    assign("rho",round(cor(x,y),2),envir=RSenvir)

    refresh <- function(...){
        sc <- as.numeric(evalq(tclvalue(scale),envir = RSenvir))
        sh <- as.numeric(evalq(tclvalue(shift),envir = RSenvir))
        x = RSenvir$x
        y = RSenvir$y
        X <- sc*x + sh
        Y <- y
        assign("X",X,envir=RSenvir)
        assign("Y",Y,envir=RSenvir)        
        #plotting original data
        plot(x, y, xlab = RSenvir$xLab, ylab = RSenvir$yLab, xlim = RSenvir$limx, ylim = RSenvir$limy, main = "", col = 'black')
        title(main = substitute(paste(rho," = ",r,sep=""),list(r=RSenvir$rho)))
        m1 <- lm(y~x)
        abline(m1$coeff[1],m1$coeff[2],col='black')
        xT = .9*RSenvir$limx[2]
        yT = m1$coeff[1]+m1$coeff[2]*xT
        if(yT > RSenvir$limy[2]){
            yT = (10-1)/10*RSenvir$limy[2]
            xT = (yT - m1$coeff[1])/m1$coeff[2]         
        }
        text(xT,yT,paste("icc=",round(icc(cbind(x,y))$value,2)),cex=1,col = 'black')
        #plotting the adjusted line
        points(X,Y,col="red")
        title(sub=paste(RSenvir$yLab, " = ", sc, RSenvir$xLab, if(sh<0){ " - " }else{" + "}, abs(sh), sep=""),cex=1,col.sub="red")
        model <- lm(Y~X)
        abline(model$coeff[1],model$coeff[2],col="red")
        yT = model$coeff[1] + model$coeff[2]*xT
        if(yT > RSenvir$limy[2]){
            yT = (10-1)/10*RSenvir$limy[2]
            xT = (yT - model$coeff[1])/model$coeff[2]         
        }
        text(xT,yT,paste("icc=",round(icc(cbind(X,Y))$value,2)),pos=4,offset=0,cex=1, col = "red")
    }
    if(gui){
        m <- tktoplevel()
        tkwm.title(m,"Scaling and Shifting the Data")
        scaleSlide <- tkscale(m,command=refresh,from=0.1,to=5,orient="horiz",label="Scale",resolution = 0.1, showvalue = TRUE,variable=RSenvir$scale)
        shiftSlide <- tkscale(m,command=refresh,from=-round(.5*diff(range(RSenvir$x)),0),to=round(.5*diff(range(RSenvir$y)),1),orient="horiz",label="Shift",resolution = 0.1, showvalue = TRUE,variable=RSenvir$shift)
        tkpack(scaleSlide,side="left")
        tkpack(shiftSlide,side="right")
    }
    else{refresh()}
}

