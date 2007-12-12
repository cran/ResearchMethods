caseDist <- function(pObs=0.9,gui=TRUE,row1=.5, col1=.5){
    if(pObs>1||pObs<0||row1>1||row1<0||col1>1||col1<0){
        stop("pObs, row1 and col1 are all proportions, they must all be between 0 and 1\n")
    }
    if(odd(100*pObs)||odd(100*row1)||odd(100*col1)){
        stop("the function only accepts even numbers for its proportions, to prevent decimal numbers in the cell counts\n")
    }
    pop <- function(...){
        x1 = round(100*as.numeric(evalq(tclvalue(CDenv$x1))))
        y1 = round(100*as.numeric(evalq(tclvalue(CDenv$y1))))
        p = 100*as.numeric(evalq(tclvalue(CDenv$p)))
        x12 = (y1+(100-x1)-p)/2
        x11 = y1 - x12
        x21 = x1 - x11
        x22 = p - x11
        return(matrix(c(x11,x12,x11+x12,x21,x22,x21+x22,x11+x21,x12+x22,100),ncol=3,byrow=TRUE,dimnames=list(c("y1","y2","row totals"),c("x1","x2","column totals"))))
    }
    # a function to create the three values to print
    prop <- function(...){
        p = as.numeric(evalq(tclvalue(CDenv$p)))
        x1 = round(100*as.numeric(evalq(tclvalue(CDenv$x1))))
        y1 = round(100*as.numeric(evalq(tclvalue(CDenv$y1))))
        pe = ((x1*y1)/100+(100-x1)*(100-y1)/100)/100
        k = (p-pe)/(1-pe)
        return(c(p,round(pe,2),round(k,2)))
    }
    refresh <- function(...){
        x <- pop()
        output <- prop()
        basicX = CDenv$basicX
        par(fig=c(0,1,0.5,1),mar=c(0,0,0,0))
        textplot(basicX,halign="center",valign="top",cex=1.75)
        text(0,0.9,"Basic Table",cex=2,pos=4,col='red')
        text(0.5,0.3,substitute(paste("pObs = ",po,", pExp = ",pe,", ",kappa," = ",k),list(po=0.9,pe=0.5,k=0.8)),col='red')
        par(new=TRUE)
        par(fig=c(0,1,0,0.5),mar=c(0,0,0,0))
        textplot(x,halign="center",valign="top",cex=1.75)
        text(0,0.9,"Modified Table",cex=2,pos=4,col='blue')
        if(min(x) >= 0){
            text(0.5,0.3,substitute(paste("pObs = ",po,", pExp = ",pe,", ",kappa," = ",k),list(po=output[1],pe=output[2],k=output[3])),col='blue')
        }
        else{
            text(0.5,0.3,"table values not possible",cex=1,pos=3,col='blue')
        }
    }

    CDenv <<- new.env()
    x1 = col1
    y1 = row1
    assign("x1",tclVar(.5),env=CDenv)
    assign("y1",tclVar(.5),env=CDenv)
    assign("p",tclVar(pObs),env=CDenv)
    basicX = pop()
    assign("basicX",basicX,env=CDenv)
    assign("x1",tclVar(x1),env=CDenv)
    assign("y1",tclVar(y1),env=CDenv)  
    # a function to create the new matrix
    
    if(gui){
        m <- tktoplevel()
        tkwm.title(m,"Case Distribution")
        xFrame <-tkframe(m)
        yFrame <- tkframe(m)
#        pObsScale <- tkscale(m,command=refresh,from=0.01,to=0.99,orient="horiz",label="Prop. of Agreement",resolution=0.02,variable=CDenv$p)
        x1Scale <- tkscale(xFrame,command=refresh,from=.01,to=.99,orient="horiz",label="x1",resolution=.02,variable=CDenv$x1)
        y1Scale <- tkscale(yFrame,command=refresh,from=.01,to=.99,orient="horiz",label="y1",resolution=.02,variable=CDenv$y1)
        tkpack(xFrame,side="top")
        tkpack(xFrame,x1Scale,side="left")
        tkpack(yFrame,side="top")
        tkpack(yFrame,y1Scale,side="left")
#        assign("pObsScale",pObsScale,env=CDenv)
#        assign("x1Scale",x1Scale,env=CDenv)
#        assign("y1Scale",y1Scale,env=CDenv)
#        evalq(tkconfigure(pObsScale,var=p),env=CDenv)
#        evalq(tkconfigure(x1Scale,var=x1),env=CDenv)
#        evalq(tkconfigure(y1Scale,var=y1),env=CDenv)
    }
    else{refresh();refresh()}

}
