multHypothesis <- function(GUI=TRUE,alpha=0.05,hyp=1){
    # variable initiation
    MHenvir <<- new.env()
    assign("alpha",tclVar(alpha),envir=MHenvir)
    assign("cases",tclVar(hyp),envir=MHenvir)
    # subfunction
    getAlpha <- function(...){
        if(GUI) a <- as.numeric(tclvalue(tkget(MHenvir$alphaEntry)))
        else a <- 0.05
        if(!is.na(a) && a < 1 && a > 0)
            return(a)
        else{
            tkdelete(MHenvir$alphaEntry,0,"end")
            tkinsert(MHenvir$alphaEntry,0,"0.05")
            return(0.05)
        }
    }
    #refresh
    refresh <- function(...){
        a <- getAlpha()
        k <- as.numeric(tclvalue(MHenvir$cases))
        ylim <- c(0,1)
        x <- 0:100
        y <- 1 - (1-a)^x
        z <- round(1 - (1-a)^k,3)
        plot(x,y,type='l',ylab=expression(1-(1-alpha)^k),xlab="Number of hypotheses tested (k)",main="Probability that AT LEAST ONE of k hypotheses is significant")
        points(c(-10,k),c(z,z),type='l',col='red')
        points(c(k,k),c(-1,z),type='l',col='red')
        text(5,z,labels=z,pos=3,col='red')
    }
    # GUI design
    if(GUI){
        m <- tktoplevel()
        tkwm.title(m,"Testing Multiple Hypotheses")
        caseFrame <- tkframe(m)
        alphaFrame <- tkframe(m)
        caseSlider <- tkscale(caseFrame,command=refresh,from=0,to=100,orient='horiz',resolution=1,showvalue=TRUE,variable=MHenvir$cases)
        alphaEntry <- tkentry(alphaFrame,width=5,bg='white')
        tkpack(tklabel(alphaFrame,text='alpha:'),side='left')
        tkpack(alphaFrame,alphaEntry,side='top')
        tkpack(tklabel(caseFrame,text='Number of Hypotheses:'),side='left')
        tkpack(caseFrame,caseSlider,side='top')
    
        assign("alphaEntry",alphaEntry,envir=MHenvir)
    }
    else refresh()
        
}
