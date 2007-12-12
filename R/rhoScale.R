`rhoScale` <-
function(x,y,gui=TRUE,sc=1,sh=0,xlab='x',ylab='y'){
    RSenv <<- new.env()
    scale = sc
    shift = sh
    assign("scale",tclVar(scale),env = RSenv)
    assign("shift",tclVar(shift),env = RSenv)
    assign("x",x,env = RSenv)
    assign("y",y,env = RSenv)    
    assign("xLab",xlab,env = RSenv)
    assign("yLab",ylab,env = RSenv)
    limx = range(x)
    limx = c(0.5*limx[1],2*limx[2])
    limy = range(y)
    limy = c(0.5*limy[1],2*limy[2])
    assign("limx",limx,env = RSenv)
    assign("limy",limy,env = RSenv)
    assign("rho",round(cor(x,y),2),env=RSenv)

    refresh <- function(...){
        sc <- as.numeric(evalq(tclvalue(scale),env = RSenv))
        sh <- as.numeric(evalq(tclvalue(shift),env = RSenv))
        x = RSenv$x
        y = RSenv$y
        X <- sc*x + sh
        Y <- y
        assign("X",X,env=RSenv)
        assign("Y",Y,env=RSenv)        
        #plotting original data
        plot(x, y, xlab = RSenv$xLab, ylab = RSenv$yLab, xlim = RSenv$limx, ylim = RSenv$limy, main = "", col = 'black')
        title(main = substitute(paste(rho," = ",r,sep=""),list(r=RSenv$rho)))
        m1 <- lm(y~x)
        abline(m1$coeff[1],m1$coeff[2],col='black')
        xT = .9*RSenv$limx[2]
        yT = m1$coeff[1]+m1$coeff[2]*xT
        if(yT > RSenv$limy[2]){
            yT = (10-1)/10*RSenv$limy[2]
            xT = (yT - m1$coeff[1])/m1$coeff[2]         
        }
        text(xT,yT,paste("icc=",round(icc(cbind(x,y))$value,2)),cex=1,col = 'black')
        #plotting the adjusted line
        points(X,Y,col="red")
        title(sub=paste(RSenv$yLab, " = ", sc, RSenv$xLab, if(sh<0){ " - " }else{" + "}, abs(sh), sep=""),cex=1,col.sub="red")
        model <- lm(Y~X)
        abline(model$coeff[1],model$coeff[2],col="red")
        yT = model$coeff[1] + model$coeff[2]*xT
        if(yT > RSenv$limy[2]){
            yT = (10-1)/10*RSenv$limy[2]
            xT = (yT - model$coeff[1])/model$coeff[2]         
        }
        text(xT,yT,paste("icc=",round(icc(cbind(X,Y))$value,2)),pos=4,offset=0,cex=1, col = "red")
    }
    if(gui){
        m <- tktoplevel()
        tkwm.title(m,"Scaling and Shifting the Data")
        scaleSlide <- tkscale(m,command=refresh,from=0.1,to=5,orient="horiz",label="Scale",resolution = 0.1, showvalue = TRUE,variable=RSenv$scale)
        shiftSlide <- tkscale(m,command=refresh,from=-round(.5*diff(range(RSenv$x)),0),to=round(.5*diff(range(RSenv$y)),1),orient="horiz",label="Shift",resolution = 0.1, showvalue = TRUE,variable=RSenv$shift)
        tkpack(scaleSlide,side="left")
        tkpack(shiftSlide,side="right")
    }
    else{refresh()}
}

