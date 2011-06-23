caseRatio <- function(GUI=TRUE,alpha=0.05,beta=0.1){
    CRenvir <<- new.env()
    assign("r",tclVar(0.5),envir=CRenvir)
    assign("alpha",tclVar(alpha),envir=CRenvir)
    assign("beta",tclVar(beta),envir=CRenvir)
    power <- function(r,a,b){
        pnorm(2*(qnorm(1-a)+qnorm(1-b))*sqrt(r*(1-r))-qnorm(1-a))
    }
    getA <- function(...){
        if(GUI) a <- as.numeric(tclvalue(tkget(CRenvir$aEntry)))
        else a <- alpha
        if(!is.na(a) && a < 1 && a > 0) return(a)
        else{
            tkdelete(CRenvir$aEntry,0,"end")
            tkinsert(CRenvir$aEntry,0,"0.05")
            return(0.05)
        }
    }
    getB <- function(...){
        if(GUI) b <- as.numeric(tclvalue(tkget(CRenvir$bEntry)))
        else b <- beta
        if(!is.na(b) && b < 1 && b > 0) return(b)
        else{
            tkdelete(CRenvir$bEntry,0,"end")
            tkinsert(CRenvir$bEntry,0,"0.10")
            return(0.10)
        }
    }
    refresh <- function(...){
        a <- getA()
        b <- getB()
        r <- as.numeric(tclvalue(CRenvir$r))
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
        rSlider <- tkscale(frame2,command=refresh,from=0.5,to=1,resolution=0.01,orient='horiz',variable=CRenvir$r)
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

        assign("aEntry",aEntry,envir=CRenvir)
        assign("bEntry",bEntry,envir=CRenvir)
    }
    else refresh()
}
