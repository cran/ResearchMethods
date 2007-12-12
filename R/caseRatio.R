caseRatio <- function(GUI=TRUE,alpha=0.05,beta=0.1){
    CRenv <<- new.env()
    assign("r",tclVar(0.5),env=CRenv)
    assign("alpha",tclVar(alpha),env=CRenv)
    assign("beta",tclVar(beta),env=CRenv)
    power <- function(r,a,b){
        pnorm(2*(qnorm(1-a)+qnorm(1-b))*sqrt(r*(1-r))-qnorm(1-a))
    }
    getA <- function(...){
        if(GUI) a <- as.numeric(tclvalue(tkget(CRenv$aEntry)))
        else a <- alpha
        if(!is.na(a) && a < 1 && a > 0) return(a)
        else{
            tkdelete(CRenv$aEntry,0,"end")
            tkinsert(CRenv$aEntry,0,"0.05")
            return(0.05)
        }
    }
    getB <- function(...){
        if(GUI) b <- as.numeric(tclvalue(tkget(CRenv$bEntry)))
        else b <- beta
        if(!is.na(b) && b < 1 && b > 0) return(b)
        else{
            tkdelete(CRenv$bEntry,0,"end")
            tkinsert(CRenv$bEntry,0,"0.10")
            return(0.10)
        }
    }
    refresh <- function(...){
        a <- getA()
        b <- getB()
        r <- as.numeric(tclvalue(CRenv$r))
        x <- seq(0.5,1,0.01)
        y <- power(x,a,b)
        z <- power(r,a,b)
        plot(x,y,type='l',xlab="Proportion on new treatment",ylab="Power",main="Effect of unequal treatment assignment on power",xlim=c(0.5,1),ylim=c(0,1))
        points(c(0,r,r),c(z,z,-10),type='l',col='red')
        text(0.5,z,labels=round(z,3),pos=3,col='red')
    }
    if(GUI){
        m <- tktoplevel()
        tkwm.title(m,"Case:Control Ratio")
        frame1 <- tkframe(m)
        frame2 <- tkframe(m)
        rSlider <- tkscale(frame2,command=refresh,from=0.5,to=1,resolution=0.01,orient='horiz',variable=CRenv$r)
        aEntry <- tkentry(frame1,width=5,bg='white')
        bEntry <- tkentry(frame1,width=5,bg='white')
        f1 <- tkfont.create(m,family='symbol')

        tkinsert(aEntry,0,"0.05")
        tkinsert(bEntry,0,"0.10")

        tkpack(tklabel(frame1,text="a:",font=f1),side='left')
        tkpack(frame1,aEntry,side='left')
        tkpack(tklabel(frame1,text="  b",font=f1),side='left')
        tkpack(tklabel(frame1,text="(when proportion=0.5):"),side='left')
        tkpack(frame1,bEntry,side='top')
        tkpack(tklabel(frame2,text="Proportion New:"),side='left')
        tkpack(frame2,rSlider,side='bottom')

        assign("aEntry",aEntry,env=CRenv)
        assign("bEntry",bEntry,env=CRenv)
    }
    else refresh()
}
