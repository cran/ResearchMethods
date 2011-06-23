menuSampleSize <- function(...){
    a = "a:"
    b = "b:"
    u1 = "m1:"
    u2 = "m2:"
    s1 = "s1:"
    s2 = "s2:"
    e = "e OR d:"
    r = "r:"
    kp1 = "k1 OR p1:"
    kp2 = "k2 OR p2:"
    ui = "u's:"
    SSenvir <<- new.env()
    # sample size variables
    assign("nCalc",tclVar(1),envir=SSenvir)
    assign("alpha",tclVar(0.05),envir=SSenvir)
    assign("beta",tclVar(0.10),envir=SSenvir)
    assign("mu0",tclVar(1),envir=SSenvir)
    assign("mu1",tclVar(5),envir=SSenvir)
    assign("sig0",tclVar(1),envir=SSenvir)
    assign("sig1",tclVar(1),envir=SSenvir)
    assign("Qe",tclVar(0.5),envir=SSenvir)
    assign("delta",tclVar(1),envir=SSenvir)
    assign("rho",tclVar(0),envir=SSenvir)
    assign("pi0",tclVar(0.25),envir=SSenvir)
    assign("pi1",tclVar(0.75),envir=SSenvir)
    assign("n",tclVar(1),envir=SSenvir)
    assign("T",tclVar(100),envir=SSenvir)
    assign("mui",tclVar("10,20,30,40,50"),envir=SSenvir)
    assign("type",2,envir=SSenvir)
    assign("printResult","Select a Calculation",envir=SSenvir)
    assign("eqnNumber","(!)",envir=SSenvir)
    assign("plotType",1,envir=SSenvir)
    # get input from the GUI, reseting to default values for incorrect input
    getInput <- function(...){
        if(!is.na(as.numeric(tclvalue(tkget(mu0Entry)))))
            assign("mu0",tclVar(as.numeric(tclvalue(tkget(mu0Entry)))),envir=SSenvir)
        else{
            tkdelete(mu0Entry,0,"end")
            tkinsert(mu0Entry,0,tclvalue(SSenvir$mu0))
        }
        if(!is.na(as.numeric(tclvalue(tkget(mu1Entry)))))
            assign("mu1",tclVar(as.numeric(tclvalue(tkget(mu1Entry)))),envir=SSenvir)
        else{
            tkdelete(mu1Entry,0,"end")
            tkinsert(mu1Entry,0,tclvalue(SSenvir$mu1))
        }
        if(!is.na(as.numeric(tclvalue(tkget(sig0Entry)))))
            assign("sig0",tclVar(as.numeric(tclvalue(tkget(sig0Entry)))),envir=SSenvir)
        else{
            tkdelete(sig0Entry,0,"end")
            tkinsert(sig0Entry,0,tclvalue(SSenvir$sig0))
        }
        if(!is.na(as.numeric(tclvalue(tkget(sig1Entry)))))
            assign("sig1",tclVar(as.numeric(tclvalue(tkget(sig1Entry)))),envir=SSenvir)
        else{
            tkdelete(sig1Entry,0,"end")
            tkinsert(sig1Entry,0,tclvalue(SSenvir$sig1))
        }
        if(!is.na(as.numeric(tclvalue(tkget(deltaEntry)))))
            assign("delta",tclVar(as.numeric(tclvalue(tkget(deltaEntry)))),envir=SSenvir)
        else{
            tkdelete(deltaEntry,0,"end")
            tkinsert(deltaEntry,0,tclvalue(SSenvir$delta))
        }
        if(!is.na(as.numeric(tclvalue(tkget(QeEntry)))) && as.numeric(tclvalue(tkget(QeEntry))) < 1 && as.numeric(tclvalue(tkget(QeEntry))) > 0)
            assign("Qe",tclVar(as.numeric(tclvalue(tkget(QeEntry)))),envir=SSenvir)
        else{
            tkdelete(QeEntry,0,"end")
            tkinsert(QeEntry,0,tclvalue(SSenvir$Qe))
        }
        if(!is.na(as.numeric(tclvalue(tkget(rhoEntry)))) && as.numeric(tclvalue(tkget(rhoEntry))) < 1 && as.numeric(tclvalue(tkget(rhoEntry))) > -1)
            assign("rho",tclVar(as.numeric(tclvalue(tkget(rhoEntry)))),envir=SSenvir)
        else{
            tkinsert(rhoEntry,0,tclvalue(SSenvir$rho))
        }
        if(!is.na(as.numeric(tclvalue(tkget(pi0Entry)))) && as.numeric(tclvalue(tkget(pi0Entry))) < 1 && as.numeric(tclvalue(tkget(pi0Entry))) > 0)
            assign("pi0",tclVar(as.numeric(tclvalue(tkget(pi0Entry)))),envir=SSenvir)
        else{
            tkdelete(pi0Entry,0,"end")
            tkinsert(pi0Entry,0,tclvalue(SSenvir$pi0))
        }
        if(!is.na(as.numeric(tclvalue(tkget(pi1Entry)))) && as.numeric(tclvalue(tkget(pi1Entry))) < 1 && as.numeric(tclvalue(tkget(pi1Entry))) > 0)
            assign("pi1",tclVar(as.numeric(tclvalue(tkget(pi1Entry)))),envir=SSenvir)
        else{
            tkdelete(pi1Entry,0,"end")
            tkinsert(pi1Entry,0,tclvalue(SSenvir$pi1))
        }
        if(!is.na(as.numeric(tclvalue(tkget(TEntry)))) && as.numeric(tclvalue(tkget(TEntry))) > 0)
            assign("T",tclVar(as.numeric(tclvalue(tkget(TEntry)))),envir=SSenvir)
        else{
            tkdelete(TEntry,0,"end")
            tkinsert(TEntry,0,tclvalue(SSenvir$T))
        }
        if(!is.na(all(as.numeric(strsplit(tclvalue(tkget(muiEntry)),split=',')[[1]]))))
            assign("mui",tclVar((tclvalue(tkget(muiEntry)))),envir=SSenvir)
        else{
            tkdelete(muiEntry,0,"end")
            tkinsert(muiEntry,0,tclvalue(SSenvir$mui))
        }


    }

    ### PROCESS: for calculating the sample size: 1) activate and de-activate the appropriate input variables,
    ### 2) get the input via getInput() 3) calculate n 

    # sample size for a mean point estimation
    meanPoint <- function(...){
        evalq(tkconfigure(betaSlider,state="disabled",troughcolor='grey'))
        evalq(tkconfigure(nSlider,state="disabled",troughcolor='grey'))
        evalq(tkconfigure(QeEntry,state="disabled"))
        evalq(tkconfigure(deltaEntry,state="normal")) 
        evalq(tkconfigure(mu0Entry,state="normal"))
        evalq(tkconfigure(sig0Entry,state="normal"))
        evalq(tkconfigure(mu1Entry,state="disabled"))
        evalq(tkconfigure(sig1Entry,state="disabled"))
        evalq(tkconfigure(rhoEntry,state="disabled")) 
        evalq(tkconfigure(pi0Entry,state="disabled"))
        evalq(tkconfigure(pi1Entry,state="disabled"))
        evalq(tkconfigure(TEntry,state="disabled"))
        evalq(tkconfigure(muiEntry,state="disabled"))
        getInput()
        n = qnorm(1-as.numeric(tclvalue(SSenvir$alpha))/2)^2*as.numeric(tclvalue(SSenvir$sig0))^2/(as.numeric(tclvalue(SSenvir$delta))*as.numeric(tclvalue(SSenvir$mu0)))^2
        assign("n",tclVar(ceiling(n)),envir=SSenvir)
        assign("type",3,envir=SSenvir)
        out = c("Mean: Point Estimate*\nn =",ceiling(n))
        sub = "* : see equation (11)"
        assign("printResult",out,envir=SSenvir)
        assign("eqnNumber",sub,envir=SSenvir)
        assign("plotType",1,envir=SSenvir)
        
    }
    # sample size for a 1-group mean hypothesis test
    mean1 <- function(...){
        if(as.numeric(tclvalue(SSenvir$nCalc))==1){
            evalq(tkconfigure(betaSlider,state="active",troughcolor=ccc))
            evalq(tkconfigure(betaSlider,var=beta),envir=SSenvir)
            evalq(tkconfigure(nSlider,state="disabled",troughcolor='grey'))
        }
        else{
            evalq(tkconfigure(betaSlider,state="disabled",troughcolor='grey'))
            evalq(tkconfigure(nSlider,state="active",troughcolor=ccc))
            evalq(tkconfigure(nSlider,var=n),envir=SSenvir)
        }
        evalq(tkconfigure(QeEntry,state="disabled"))
        evalq(tkconfigure(deltaEntry,state="disabled")) 
        evalq(tkconfigure(mu0Entry,state="normal"))
        evalq(tkconfigure(sig0Entry,state="normal"))
        evalq(tkconfigure(mu1Entry,state="normal"))
        evalq(tkconfigure(sig1Entry,state="normal"))
        evalq(tkconfigure(rhoEntry,state="disabled")) 
        evalq(tkconfigure(pi0Entry,state="disabled"))
        evalq(tkconfigure(pi1Entry,state="disabled"))
        evalq(tkconfigure(TEntry,state="disabled"))
        evalq(tkconfigure(muiEntry,state="disabled"))
        getInput()
        if(as.numeric(tclvalue(SSenvir$nCalc))==1){
            n = ((qnorm(1-as.numeric(tclvalue(SSenvir$alpha))/2)*as.numeric(tclvalue(SSenvir$sig0)) + 
                  qnorm(1-as.numeric(tclvalue(SSenvir$beta)))*as.numeric(tclvalue(SSenvir$sig1)))/
                (as.numeric(tclvalue(SSenvir$mu0))-as.numeric(tclvalue(SSenvir$mu1))))^2
            assign("n",tclVar(ceiling(n)),envir=SSenvir)
            assign("type",1,envir=SSenvir)
            out = c("Mean: 1 Group*\nn =",ceiling(n)) 
            sub = "* : see equation (1)"
            assign("eqnNumber",sub,envir=SSenvir)
            assign("printResult",out,envir=SSenvir)
            assign("plotType",2,envir=SSenvir)
        }
        else{
            beta = 1 - pnorm((sqrt(as.numeric(tclvalue(SSenvir$n)))*abs(as.numeric(tclvalue(SSenvir$mu0))-as.numeric(tclvalue(SSenvir$mu1)))-qnorm(1-as.numeric(tclvalue(SSenvir$alpha))/2)*as.numeric(tclvalue(SSenvir$sig0)))/as.numeric(tclvalue(SSenvir$sig1)))
            assign("beta",tclVar(beta),envir=SSenvir)
            assign("type",1,envir=SSenvir)
            out = c(expression("Mean: 1 Group*\n",beta,"= "),round(beta,4),"power =",1-round(beta,4)) 
            sub = "* : see equation (15)"
            assign("eqnNumber",sub,envir=SSenvir)
            assign("printResult",out,envir=SSenvir)
            assign("plotType",2,envir=SSenvir)
        }
    }
    # a sample size for 2 independent group mean hypothesis test
    mean2Indep <- function(...){
        if(as.numeric(tclvalue(SSenvir$nCalc))==1){
            evalq(tkconfigure(betaSlider,state="active",troughcolor=ccc))
            evalq(tkconfigure(betaSlider,var=beta),envir=SSenvir)
            evalq(tkconfigure(nSlider,state="disabled",troughcolor='grey'))
        }
        else{
            evalq(tkconfigure(betaSlider,state="disabled",troughcolor='grey'))
            evalq(tkconfigure(nSlider,state="active",troughcolor=ccc))
            evalq(tkconfigure(nSlider,var=n),envir=SSenvir)
        }
        evalq(tkconfigure(QeEntry,state="normal"))
        evalq(tkconfigure(deltaEntry,state="normal")) 
        evalq(tkconfigure(mu0Entry,state="normal"))
        evalq(tkconfigure(sig0Entry,state="normal"))
        evalq(tkconfigure(mu1Entry,state="disabled"))
        evalq(tkconfigure(sig1Entry,state="disabled"))
        evalq(tkconfigure(rhoEntry,state="disabled")) 
        evalq(tkconfigure(pi0Entry,state="disabled"))
        evalq(tkconfigure(pi1Entry,state="disabled"))
        evalq(tkconfigure(TEntry,state="disabled"))
        evalq(tkconfigure(muiEntry,state="disabled"))
        getInput()
        if(as.numeric(tclvalue(SSenvir$nCalc))==1){
            n = as.numeric(tclvalue(SSenvir$sig0))^2*(1/as.numeric(tclvalue(SSenvir$Qe))+1/(1-as.numeric(tclvalue(SSenvir$Qe))))*
                (qnorm(1-as.numeric(tclvalue(SSenvir$alpha))/2) + qnorm(1-as.numeric(tclvalue(SSenvir$beta))))^2/
                as.numeric(tclvalue(SSenvir$delta))^2
            assign("n",tclVar(ceiling(n)),envir=SSenvir)
            assign("mu1",tclVar(as.numeric(tclvalue(SSenvir$mu0))+as.numeric(tclvalue(SSenvir$delta))),envir=SSenvir)
            assign("sig1",tclVar(as.numeric(tclvalue(SSenvir$sig0))),envir=SSenvir)
            assign("type",1,envir=SSenvir)
            out = c("Mean: 2 Independent Groups*\nn =",ceiling(n)) 
            sub = "* : see equation (2)"
            assign("eqnNumber",sub,envir=SSenvir)
            assign("printResult",out,envir=SSenvir)
            assign("plotType",3,envir=SSenvir)
        }
        else{
            beta = 1-pnorm(as.numeric(tclvalue(SSenvir$delta))/as.numeric(tclvalue(SSenvir$sig0))*sqrt(as.numeric(tclvalue(SSenvir$n))/(1/as.numeric(tclvalue(SSenvir$Qe))+1/(1-as.numeric(tclvalue(SSenvir$Qe)))))-qnorm(1-as.numeric(tclvalue(SSenvir$alpha))/2))
            assign("beta",tclVar(beta),envir=SSenvir)
            assign("type",1,envir=SSenvir)
            out=c(expression("Mean: 2 Independent Groups*\n",beta,"= "),round(beta,4),"power =",1-round(beta,4)) 
            sub = "* : see equation (16)"
            assign("eqnNumber",sub,envir=SSenvir)
            assign("printResult",out,envir=SSenvir)
            assign("plotType",3,envir=SSenvir)
        }
        
    }
    # a sample size for sets of paired observations
    meanPaired <- function(...){
        if(as.numeric(tclvalue(SSenvir$nCalc))==1){
            evalq(tkconfigure(betaSlider,state="active",troughcolor=ccc))
            evalq(tkconfigure(betaSlider,var=beta),envir=SSenvir)
            evalq(tkconfigure(nSlider,state="disabled",troughcolor='grey'))
        }
        else{
            evalq(tkconfigure(betaSlider,state="disabled",troughcolor='grey'))
            evalq(tkconfigure(nSlider,state="active",troughcolor=ccc))
            evalq(tkconfigure(nSlider,var=n),envir=SSenvir)
        }
        evalq(tkconfigure(QeEntry,state="disabled"))
        evalq(tkconfigure(deltaEntry,state="normal")) 
        evalq(tkconfigure(mu0Entry,state="normal"))
        evalq(tkconfigure(sig0Entry,state="normal"))
        evalq(tkconfigure(mu1Entry,state="disabled"))
        evalq(tkconfigure(sig1Entry,state="disabled"))
        evalq(tkconfigure(rhoEntry,state="normal")) 
        evalq(tkconfigure(pi0Entry,state="disabled"))
        evalq(tkconfigure(pi1Entry,state="disabled"))
        evalq(tkconfigure(TEntry,state="disabled"))
        evalq(tkconfigure(muiEntry,state="disabled"))
        getInput()
        if(as.numeric(tclvalue(SSenvir$nCalc))==1){
            n = 2*as.numeric(tclvalue(SSenvir$sig0))^2*(1-as.numeric(tclvalue(SSenvir$rho)))*
                (qnorm(1-as.numeric(tclvalue(SSenvir$alpha))/2)+qnorm(1-as.numeric(tclvalue(SSenvir$beta))))^2/
                as.numeric(tclvalue(SSenvir$delta))^2
            assign("n",tclVar(ceiling(n)),envir=SSenvir)
            assign("mu1",tclVar(as.numeric(tclvalue(SSenvir$mu0))+as.numeric(tclvalue(SSenvir$delta))),envir=SSenvir)
            assign("sig1",tclVar(as.numeric(tclvalue(SSenvir$sig0))),envir=SSenvir)
            assign("type",1,envir=SSenvir)
            out = c("Mean: Paired Observations*\nn =",ceiling(n)) 
            sub = "* : see equation (3)"
            assign("eqnNumber",sub,envir=SSenvir)
            assign("printResult",out,envir=SSenvir)
            assign("plotType",4,envir=SSenvir)
        }
        else{
            beta = 1 - pnorm(as.numeric(tclvalue(SSenvir$delta))/(2*as.numeric(tclvalue(SSenvir$sig0))^2*(1-as.numeric(tclvalue(SSenvir$rho))))*sqrt(as.numeric(tclvalue(SSenvir$n)))-qnorm(1-as.numeric(tclvalue(SSenvir$alpha))/2))
            assign("beta",tclVar(beta),envir=SSenvir)
            assign("type",1,envir=SSenvir)
            out = c(expression("Mean: Paired Observations*\n",beta, "= "),round(beta,4),"power =",1-round(beta,4)) 
            sub = "* : see equation (17)"
            assign("eqnNumber",sub,envir=SSenvir)
            assign("printResult",out,envir=SSenvir)
            assign("plotType",4,envir=SSenvir)
        }
    }
    # sample size for proportion point estimation
    propPoint <- function(...){
        evalq(tkconfigure(betaSlider,state="disabled",troughcolor='grey'))
        evalq(tkconfigure(nSlider,state="disabled",troughcolor='grey'))
        evalq(tkconfigure(QeEntry,state="disabled"))
        evalq(tkconfigure(deltaEntry,state="normal")) 
        evalq(tkconfigure(mu0Entry,state="disabled"))
        evalq(tkconfigure(sig0Entry,state="disabled"))
        evalq(tkconfigure(mu1Entry,state="disabled"))
        evalq(tkconfigure(sig1Entry,state="disabled"))
        evalq(tkconfigure(rhoEntry,state="disabled")) 
        evalq(tkconfigure(pi0Entry,state="normal"))
        evalq(tkconfigure(pi1Entry,state="disabled"))
        evalq(tkconfigure(TEntry,state="disabled"))
        evalq(tkconfigure(muiEntry,state="disabled"))
        getInput()
        n = qnorm(1-as.numeric(tclvalue(SSenvir$alpha))/2)^2*as.numeric(tclvalue(SSenvir$pi0))*(1-as.numeric(tclvalue(SSenvir$pi0)))/as.numeric(tclvalue(SSenvir$delta))^2
        assign("n",tclVar(ceiling(n)),envir=SSenvir)
        assign("pi1",SSenvir$pi0,envir=SSenvir)
        assign("type",2,envir=SSenvir)
        out = c("Proportion: Point Estimate*\nn =",ceiling(n)) 
        sub = "* : see equation (12)"
        assign("eqnNumber",sub,envir=SSenvir)
        assign("printResult",out,envir=SSenvir)
        assign("plotType",5,envir=SSenvir)
    } 

    # sample size for 1 group of proportions
    prop1 <- function(...){
        if(as.numeric(tclvalue(SSenvir$nCalc))==1){
            evalq(tkconfigure(betaSlider,state="active",troughcolor=ccc))
            evalq(tkconfigure(betaSlider,var=beta),envir=SSenvir)
            evalq(tkconfigure(nSlider,state="disabled",troughcolor='grey'))
        }
        else{
            evalq(tkconfigure(betaSlider,state="disabled",troughcolor='grey'))
            evalq(tkconfigure(nSlider,state="active",troughcolor=ccc))
            evalq(tkconfigure(nSlider,var=n),envir=SSenvir)
        }
        evalq(tkconfigure(QeEntry,state="disabled"))
        evalq(tkconfigure(deltaEntry,state="disabled")) 
        evalq(tkconfigure(mu0Entry,state="disabled"))
        evalq(tkconfigure(sig0Entry,state="disabled"))
        evalq(tkconfigure(mu1Entry,state="disabled"))
        evalq(tkconfigure(sig1Entry,state="disabled"))
        evalq(tkconfigure(rhoEntry,state="disabled")) 
        evalq(tkconfigure(pi0Entry,state="normal"))
        evalq(tkconfigure(pi1Entry,state="normal"))
        evalq(tkconfigure(TEntry,state="disabled"))
        evalq(tkconfigure(muiEntry,state="disabled"))
        getInput()
        if(as.numeric(tclvalue(SSenvir$nCalc))==1){
            n = (qnorm(1-as.numeric(tclvalue(SSenvir$alpha))/2)*sqrt(as.numeric(tclvalue(SSenvir$pi0))*(1-as.numeric(tclvalue(SSenvir$pi0))))+
                 qnorm(1-as.numeric(tclvalue(SSenvir$beta)))*sqrt(as.numeric(tclvalue(SSenvir$pi1))*(1-as.numeric(tclvalue(SSenvir$pi1)))))^2/
                (as.numeric(tclvalue(SSenvir$pi1))-as.numeric(tclvalue(SSenvir$pi0)))^2
            assign("n",tclVar(ceiling(n)),envir=SSenvir)
            assign("type",2,envir=SSenvir)
            out = c("Proportion: 1 Group*\nn =",ceiling(n)) 
            sub = "* : see equation (4)"
            assign("eqnNumber",sub,envir=SSenvir)
            assign("printResult",out,envir=SSenvir)
            assign("plotType",6,envir=SSenvir)
        }
        else{
            beta = 1-pnorm((sqrt(as.numeric(tclvalue(SSenvir$n)))*abs(as.numeric(tclvalue(SSenvir$pi0))-as.numeric(tclvalue(SSenvir$pi1)))-sqrt(as.numeric(tclvalue(SSenvir$pi0))*(1-as.numeric(tclvalue(SSenvir$pi1))))*qnorm(1-as.numeric(tclvalue(SSenvir$alpha))))/sqrt(as.numeric(tclvalue(SSenvir$pi1))*(1-as.numeric(tclvalue(SSenvir$pi0)))))
            assign("beta",tclVar(beta),envir=SSenvir)
            assign("type",2,envir=SSenvir)
            out = c(expression("Proportion: 1 Group*\n",beta ,"= "),round(beta,4),"power =",1-round(beta,4)) 
            sub = "* : see equation (18)"
            assign("eqnNumber",sub,envir=SSenvir)
            assign("printResult",out,envir=SSenvir)
            assign("plotType",6,envir=SSenvir)
        }
    }
    # sample size for 2 independent groups of proportions
    prop2Indep <- function(...){
        if(as.numeric(tclvalue(SSenvir$nCalc))==1){
            evalq(tkconfigure(betaSlider,state="active",troughcolor=ccc))
            evalq(tkconfigure(betaSlider,var=beta),envir=SSenvir)
            evalq(tkconfigure(nSlider,state="disabled",troughcolor='grey'))
        }
        else{
            evalq(tkconfigure(betaSlider,state="disabled",troughcolor='grey'))
            evalq(tkconfigure(nSlider,state="active",troughcolor=ccc))
            evalq(tkconfigure(nSlider,var=n),envir=SSenvir)
        }
        evalq(tkconfigure(QeEntry,state="normal"))
        evalq(tkconfigure(deltaEntry,state="disabled")) 
        evalq(tkconfigure(mu0Entry,state="disabled"))
        evalq(tkconfigure(sig0Entry,state="disabled"))
        evalq(tkconfigure(mu1Entry,state="disabled"))
        evalq(tkconfigure(sig1Entry,state="disabled"))
        evalq(tkconfigure(rhoEntry,state="disabled")) 
        evalq(tkconfigure(pi0Entry,state="normal"))
        evalq(tkconfigure(pi1Entry,state="normal"))
        evalq(tkconfigure(TEntry,state="disabled"))
        evalq(tkconfigure(muiEntry,state="disabled"))
        getInput()
        if(as.numeric(tclvalue(SSenvir$nCalc))==1){
            n = bsamsize(as.numeric(tclvalue(SSenvir$pi0)),as.numeric(tclvalue(SSenvir$pi1)),frac=as.numeric(tclvalue(SSenvir$Qe)),alpha=as.numeric(tclvalue(SSenvir$alpha)),power=1-as.numeric(tclvalue(SSenvir$beta)))
            assign("n",tclVar(ceiling(n[1])+ceil(n[2])),envir=SSenvir)
            assign("type",2,envir=SSenvir)
            out = c("Proportion: 2 Independent Groups*\nn =",ceiling(n[1])+ceiling(n[2])) 
            sub = "* : see equation (5)"
            assign("eqnNumber",sub,envir=SSenvir)
            assign("printResult",out,envir=SSenvir)
            assign("plotType",7,envir=SSenvir)
        }
        else{
            piBar = as.numeric(tclvalue(SSenvir$Qe))*as.numeric(tclvalue(SSenvir$pi0)) + (1-as.numeric(tclvalue(SSenvir$Qe)))*as.numeric(tclvalue(SSenvir$pi1))
            beta = 1 - pnorm(sqrt(as.numeric(tclvalue(SSenvir$n))/(4*piBar*(1-piBar)))*abs(as.numeric(tclvalue(SSenvir$pi0))-as.numeric(tclvalue(SSenvir$pi1)))-qnorm(1-as.numeric(tclvalue(SSenvir$alpha))))
            assign("beta",tclVar(beta),envir=SSenvir)
            out = c(expression("Proportion: 2 Independent Groups*\n",beta ,"= "),round(beta,4),"power =",1-round(beta,4)) 
            sub = "* : see equation (19)"
            assign("eqnNumber",sub,envir=SSenvir)
            assign("printResult",out,envir=SSenvir)
            assign("plotType",7,envir=SSenvir)
        }
    }
    # sample size for paired observations of proportions
    propPaired <- function(...){
        if(as.numeric(tclvalue(SSenvir$nCalc))==1){
            evalq(tkconfigure(betaSlider,state="active",troughcolor=ccc))
            evalq(tkconfigure(betaSlider,var=beta),envir=SSenvir)
            evalq(tkconfigure(nSlider,state="disabled",troughcolor='grey'))
        }
        else{
            evalq(tkconfigure(betaSlider,state="disabled",troughcolor='grey'))
            evalq(tkconfigure(nSlider,state="active",troughcolor=ccc))
            evalq(tkconfigure(nSlider,var=n),envir=SSenvir)
        }
        evalq(tkconfigure(QeEntry,state="disabled"))
        evalq(tkconfigure(deltaEntry,state="disabled")) 
        evalq(tkconfigure(mu0Entry,state="disabled"))
        evalq(tkconfigure(sig0Entry,state="disabled"))
        evalq(tkconfigure(mu1Entry,state="disabled"))
        evalq(tkconfigure(sig1Entry,state="disabled"))
        evalq(tkconfigure(rhoEntry,state="disabled")) 
        evalq(tkconfigure(pi0Entry,state="normal"))
        evalq(tkconfigure(pi1Entry,state="normal"))
        evalq(tkconfigure(TEntry,state="disabled"))
        evalq(tkconfigure(muiEntry,state="disabled"))
        getInput()
        piB = as.numeric(tclvalue(SSenvir$pi1))*(1-as.numeric(tclvalue(SSenvir$pi0)))
        piC = as.numeric(tclvalue(SSenvir$pi0))*(1-as.numeric(tclvalue(SSenvir$pi1)))
        if(as.numeric(tclvalue(SSenvir$nCalc))==1){
            n = (qnorm(1-as.numeric(tclvalue(SSenvir$alpha))/2)*sqrt(piB+piC)+qnorm(1-as.numeric(tclvalue(SSenvir$beta)))*sqrt(4*piB*piC/(piB+piC)))^2/(piB-piC)^2
            assign("n",tclVar(ceiling(n)),envir=SSenvir)
            assign("type",2,envir=SSenvir)
            out = c("Proportion: Paired Observations*\nn =", ceiling(n)) 
            sub = "* : see equation (6)"
            assign("eqnNumber",sub,envir=SSenvir)
            assign("printResult",out,envir=SSenvir)
            assign("plotType",8,envir=SSenvir)
        }
        else{
            beta = 1 - pnorm((sqrt(as.numeric(tclvalue(SSenvir$n)))*abs(piB-piC)-qnorm(1-as.numeric(tclvalue(SSenvir$alpha))/2)*sqrt(piB+piC))*sqrt((piB+piC)/(4*piB*piC)))
            assign("beta",tclVar(beta),envir=SSenvir)
            assign("type",2,envir=SSenvir)
            out = c(expression("Proportion: Paired Observations*\n",beta ,"= "),round(beta,4),"power =",1-round(beta,4)) 
            sub = "* : see equation (20)"
            assign("eqnNumber",sub,envir=SSenvir)
            assign("printResult",out,envir=SSenvir)
            assign("plotType",8,envir=SSenvir)
        }
    }
    # survival analysis based on ratios of group means, scaled so that lowest mean equals 1
    survScaled <- function(...){
        if(as.numeric(tclvalue(SSenvir$nCalc))==1){
            evalq(tkconfigure(betaSlider,state="active",troughcolor=ccc))
            evalq(tkconfigure(betaSlider,var=beta),envir=SSenvir)
            evalq(tkconfigure(nSlider,state="disabled",troughcolor='grey'))
        }
        else{
            evalq(tkconfigure(betaSlider,state="disabled",troughcolor='grey'))
            evalq(tkconfigure(nSlider,state="active",troughcolor=ccc))
            evalq(tkconfigure(nSlider,var=n),envir=SSenvir)
        }
        evalq(tkconfigure(QeEntry,state="disabled"))
        evalq(tkconfigure(deltaEntry,state="disabled")) 
        evalq(tkconfigure(mu0Entry,state="disabled"))
        evalq(tkconfigure(sig0Entry,state="disabled"))
        evalq(tkconfigure(mu1Entry,state="disabled"))
        evalq(tkconfigure(sig1Entry,state="disabled"))
        evalq(tkconfigure(rhoEntry,state="disabled")) 
        evalq(tkconfigure(pi0Entry,state="disabled"))
        evalq(tkconfigure(pi1Entry,state="disabled"))
        evalq(tkconfigure(TEntry,state="disabled"))
        evalq(tkconfigure(muiEntry,state="normal"))
        getInput()
        v = as.numeric(strsplit(tclvalue(SSenvir$mui),split=',')[[1]])
        k = length(v)
        v = log(v/min(v),base=exp(1))
        if(as.numeric(tclvalue(SSenvir$nCalc))==1){
            n = tau(k-1,as.numeric(tclvalue(SSenvir$alpha)),as.numeric(tclvalue(SSenvir$beta)))/sum((v-mean(v))^2)
            assign("n",tclVar(ceiling(n)),envir=SSenvir)
            assign("type",6,envir=SSenvir)
            out = c("Survival Analysis: Ratio of Means*\nd =",ceiling(n)) 
            sub = "* : see equation (8)"
            assign("eqnNumber",sub,envir=SSenvir)
            assign("printResult",out,envir=SSenvir)
            assign("plotType",9,envir=SSenvir)
        }
        else{
            tau = as.numeric(tclvalue(SSenvir$n))*sum((v-mean(v))^2)
            crit = qchisq(1-as.numeric(tclvalue(SSenvir$alpha)),df=k-1)
            beta = pchisq(crit,df=k-1,ncp=tau)
            assign("beta",tclVar(beta),envir=SSenvir)
            assign("type",6,envir=SSenvir)
            out = c(expression("Survival Analysis: Ratio of Means*\n",beta ,"= "),round(beta,4),"power =",1-round(beta,4)) 
            sub = "* : see equation (21)"
            assign("eqnNumber",sub,envir=SSenvir)
            assign("printResult",out,envir=SSenvir) 
            assign("plotType",9,envir=SSenvir)
        }
    }
    # survival analysis defined by the ratio of the largest to the smallest group means
    # take mu1 as the largest value, mu0 as the smallest
    survRatio <- function(...){
        if(as.numeric(tclvalue(SSenvir$nCalc))==1){
            evalq(tkconfigure(betaSlider,state="active",troughcolor=ccc))
            evalq(tkconfigure(betaSlider,var=beta),envir=SSenvir)
            evalq(tkconfigure(nSlider,state="disabled",troughcolor='grey'))
        }
        else{
            evalq(tkconfigure(betaSlider,state="disabled",troughcolor='grey'))
            evalq(tkconfigure(nSlider,state="active",troughcolor=ccc))
            evalq(tkconfigure(nSlider,var=n),envir=SSenvir)
        }

        evalq(tkconfigure(QeEntry,state="disabled"))
        evalq(tkconfigure(deltaEntry,state="disabled")) 
        evalq(tkconfigure(mu0Entry,state="normal"))
        evalq(tkconfigure(sig0Entry,state="disabled"))
        evalq(tkconfigure(mu1Entry,state="normal"))
        evalq(tkconfigure(sig1Entry,state="disabled"))
        evalq(tkconfigure(rhoEntry,state="disabled")) 
        evalq(tkconfigure(pi0Entry,state="disabled"))
        evalq(tkconfigure(pi1Entry,state="disabled"))
        evalq(tkconfigure(TEntry,state="normal"))
        evalq(tkconfigure(muiEntry,state="disabled"))
        getInput()
        if(as.numeric(tclvalue(SSenvir$nCalc))==1){
            n = 2*tau(as.numeric(tclvalue(SSenvir$T))-1,as.numeric(tclvalue(SSenvir$alpha)),as.numeric(tclvalue(SSenvir$beta)))/(log(as.numeric(tclvalue(SSenvir$mu1))/as.numeric(tclvalue(SSenvir$mu0)),base=exp(1)))^2
            assign("n",tclVar(ceiling(n)),envir=SSenvir)
            assign("type",4,envir=SSenvir)
            out = c("Survival Analysis: Largest Ratio*\nd =",ceiling(n)) 
            sub = "* : see equation (9) (ignore dotted line)"
            assign("eqnNumber",sub,envir=SSenvir)
            assign("printResult",out,envir=SSenvir)
            assign("plotType",11,envir=SSenvir)
        }
        else{
            tau = as.numeric(tclvalue(SSenvir$n))/2*log(as.numeric(tclvalue(SSenvir$mu1))/as.numeric(tclvalue(SSenvir$mu0)),base=exp(1))^2
            crit = qchisq(1-as.numeric(tclvalue(SSenvir$alpha)),df=as.numeric(tclvalue(SSenvir$T))-1)
            beta = pchisq(crit,df=as.numeric(tclvalue(SSenvir$T))-1,ncp=tau)
            assign("type",4,envir=SSenvir)
            out = c(expression("Survival Analysis: Largest Ratio*\n",beta ,"= "),round(beta,4),"power =",1-round(beta,4)) 
            sub = "* : see equation (22)"
            assign("eqnNumber",sub,envir=SSenvir)
            assign("printResult",out,envir=SSenvir)
            assign("plotType",11,envir=SSenvir)
        }
    }
    # a simple 2-group survival analysis sample size
    surv2Groups <- function(...){
        if(as.numeric(tclvalue(SSenvir$nCalc))==1){
            evalq(tkconfigure(betaSlider,state="active",troughcolor=ccc))
            evalq(tkconfigure(betaSlider,var=beta),envir=SSenvir)
            evalq(tkconfigure(nSlider,state="disabled",troughcolor='grey'))
        }
        else{
            evalq(tkconfigure(betaSlider,state="disabled",troughcolor='grey'))
            evalq(tkconfigure(nSlider,state="active",troughcolor=ccc))
            evalq(tkconfigure(nSlider,var=n),envir=SSenvir)
        }
        evalq(tkconfigure(QeEntry,state="normal"))
        evalq(tkconfigure(deltaEntry,state="disabled")) 
        evalq(tkconfigure(mu0Entry,state="normal"))
        evalq(tkconfigure(sig0Entry,state="disabled"))
        evalq(tkconfigure(mu1Entry,state="normal"))
        evalq(tkconfigure(sig1Entry,state="disabled"))
        evalq(tkconfigure(rhoEntry,state="disabled")) 
        evalq(tkconfigure(pi0Entry,state="disabled"))
        evalq(tkconfigure(pi1Entry,state="disabled"))
        evalq(tkconfigure(TEntry,state="normal"))
        evalq(tkconfigure(muiEntry,state="disabled"))
        getInput()
        psi <- function(mu){
            mu^3*as.numeric(tclvalue(SSenvir$T))/(mu*as.numeric(tclvalue(SSenvir$T))-1+exp(-mu*as.numeric(tclvalue(SSenvir$T))))
        }
        Qe = as.numeric(tclvalue(SSenvir$Qe))
        Qc = 1-Qe
        mu1 = as.numeric(tclvalue(SSenvir$mu1))
        mu0 = as.numeric(tclvalue(SSenvir$mu0))
        if(as.numeric(tclvalue(SSenvir$nCalc))==1){
            n = (qnorm(1-as.numeric(tclvalue(SSenvir$alpha))/2)*sqrt(psi(Qe*mu0+Qc*mu1)*(1/Qe+1/Qc))+qnorm(1-as.numeric(tclvalue(SSenvir$beta)))*sqrt(psi(mu0)/Qe+psi(mu1)/Qc))^2/(mu1-mu0)^2
            assign("n",tclVar(ceiling(n)),envir=SSenvir)
            assign("type",4,envir=SSenvir)
            out = c("Survival Analysis: 2 Groups*\nn =",ceiling(n)) 
            sub = "* : see equation (10)"
            assign("eqnNumber",sub,envir=SSenvir)
            assign("printResult",out,envir=SSenvir)
            assign("plotType",10,envir=SSenvir)
        }
        else{
            beta = 1-pnorm((sqrt(as.numeric(tclvalue(SSenvir$n)))*abs(mu1-mu0)-sqrt(psi(Qe*mu0+Qc*mu1)*(1/Qe+1/Qc))*qnorm(1-as.numeric(tclvalue(SSenvir$alpha))/2))/sqrt(psi(mu0)/Qe+psi(mu1)/Qc))
            assign("beta",tclVar(beta),envir=SSenvir)
            assign("type",4,envir=SSenvir)
            out = c(expression("Survival Analysis: 2 Groups*\n",beta ,"= "),round(beta,4),"power =",1-round(beta,4)) 
            sub = "* : see equation (23)"
            assign("eqnNumber",sub,envir=SSenvir)
            assign("printResult",out,envir=SSenvir)
            assign("plotType",10,envir=SSenvir)
        }
    }
    # The intra-class correlation for an ANOVA problem
    icc <- function(...){
        evalq(tkconfigure(betaSlider,state="disabled",troughcolor='grey'))
        evalq(tkconfigure(QeEntry,state="disabled"))
        evalq(tkconfigure(deltaEntry,state="normal")) 
        evalq(tkconfigure(mu0Entry,state="disabled"))
        evalq(tkconfigure(sig0Entry,state="disabled"))
        evalq(tkconfigure(mu1Entry,state="disabled"))
        evalq(tkconfigure(sig1Entry,state="disabled"))
        evalq(tkconfigure(rhoEntry,state="normal")) 
        evalq(tkconfigure(pi0Entry,state="disabled"))
        evalq(tkconfigure(pi1Entry,state="disabled"))
        evalq(tkconfigure(TEntry,state="normal"))
        evalq(tkconfigure(muiEntry,state="disabled"))
        getInput()
        n = nCalc(as.numeric(tclvalue(SSenvir$T)),as.numeric(tclvalue(SSenvir$rho)),as.numeric(tclvalue(SSenvir$delta)),as.numeric(tclvalue(SSenvir$alpha)))
        assign("n",tclVar(ceiling(n)),envir=SSenvir)
        assign("type",5,envir=SSenvir)
        out = c("ICC*\nn =",ceiling(n)) 
        sub = "* : see equation (13)"
        assign("eqnNumber",sub,envir=SSenvir)
        assign("printResult",out,envir=SSenvir)
        assign("plotType",12,envir=SSenvir)
    }
    # a sub-function used by icc to plot stuff
    nCalc <- function(k,p,eps,alpha){
        8*qnorm(1-alpha/2)*(1-p)^2*(1+(k-1)*p)^2/(k*(k-1)*eps^2)+1
    }
    # a hypothesis test on chance-corrected kappa
    kappa <- function(...){
        if(as.numeric(tclvalue(SSenvir$nCalc))==1){
            evalq(tkconfigure(betaSlider,state="active",troughcolor=ccc))
            evalq(tkconfigure(betaSlider,var=beta),envir=SSenvir)
            evalq(tkconfigure(nSlider,state="disabled",troughcolor='grey'))
        }
        else{
            evalq(tkconfigure(betaSlider,state="disabled",troughcolor='grey'))
            evalq(tkconfigure(nSlider,state="active",troughcolor=ccc))
            evalq(tkconfigure(nSlider,var=n),envir=SSenvir)
        }

        evalq(tkconfigure(QeEntry,state="disabled"))
        evalq(tkconfigure(deltaEntry,state="disabled")) 
        evalq(tkconfigure(mu0Entry,state="disabled"))
        evalq(tkconfigure(sig0Entry,state="disabled"))
        evalq(tkconfigure(mu1Entry,state="disabled"))
        evalq(tkconfigure(sig1Entry,state="disabled"))
        evalq(tkconfigure(rhoEntry,state="normal")) 
        evalq(tkconfigure(pi0Entry,state="normal"))
        evalq(tkconfigure(pi1Entry,state="normal"))
        evalq(tkconfigure(TEntry,state="disabled"))       
        evalq(tkconfigure(muiEntry,state="disabled"))
        getInput()
        p = as.numeric(tclvalue(SSenvir$rho))
        k0 = as.numeric(tclvalue(SSenvir$pi0))
        k1 = as.numeric(tclvalue(SSenvir$pi1))
        if(as.numeric(tclvalue(SSenvir$nCalc))==1){
            n = (qnorm(1-as.numeric(tclvalue(SSenvir$alpha))/2)+qnorm(1-as.numeric(tclvalue(SSenvir$beta))))^2/((p*(1-p)*(k1-k0))^2/(p^2+p*(1-p)*k0)+2*(p*(1-p)*(k1-k0))^2/(p*(1-p)*(1-k0))+(p*(1-p)*(k1-k0))^2/((1-p)^2+p*(1-p)*k0))
            assign("n",tclVar(ceiling(n)),envir=SSenvir)
            assign("type",7,envir=SSenvir)
            out=c("Kappa*\nn =",ceiling(n)) 
            sub = "* : see equation (14)"
            assign("eqnNumber",sub,envir=SSenvir)
            assign("printResult",out,envir=SSenvir)
            assign("plotType",13,envir=SSenvir)
        }
        else{
            beta = 1-pnorm(sqrt(as.numeric(tclvalue(SSenvir$n))*((p*(1-p)*(k1-k0))^2/(p^2+p*(1-p)*k0)+2*(p*(1-p)*(k1-k0))^2/(p*(1-p)*(1-k0))+(p*(1-p)*(k1-k0))^2/((1-p)^2+p*(1-p)*k0)))-qnorm(1-as.numeric(tclvalue(SSenvir$alpha))/2))
            assign("beta",tclVar(beta),envir=SSenvir)
            assign("type",5,envir=SSenvir)
            out=c(expression("Kappa*\n",beta ,"= "),round(beta,4),"power =",1-round(beta,4)) 
            sub = "* : see equation (24)"
            assign("eqnNumber",sub,envir=SSenvir)
            assign("printResult",out,envir=SSenvir)
            assign("plotType",13,envir=SSenvir)
        }
    }
    # sample size for estimating an odds ratio within a percent
    oddsRatioPoint <- function(...){
        evalq(tkconfigure(betaSlider,state="disabled",troughcolor='grey'))
        evalq(tkconfigure(nSlider,state="disabled",troughcolor='grey'))
        evalq(tkconfigure(QeEntry,state="disabled"))
        evalq(tkconfigure(deltaEntry,state="normal")) 
        evalq(tkconfigure(mu0Entry,state="disabled"))
        evalq(tkconfigure(sig0Entry,state="disabled"))
        evalq(tkconfigure(mu1Entry,state="disabled"))
        evalq(tkconfigure(sig1Entry,state="disabled"))
        evalq(tkconfigure(rhoEntry,state="disabled")) 
        evalq(tkconfigure(pi0Entry,state="normal"))
        evalq(tkconfigure(pi1Entry,state="normal"))
        evalq(tkconfigure(TEntry,state="disabled"))
        evalq(tkconfigure(muiEntry,state="disabled"))
        getInput()
        n = qnorm(1-as.numeric(tclvalue(SSenvir$alpha))/2)^2/log(1-as.numeric(tclvalue(SSenvir$delta)))^2*(1/(as.numeric(tclvalue(SSenvir$pi0))*(1-as.numeric(tclvalue(SSenvir$pi0))))+1/(as.numeric(tclvalue(SSenvir$pi1))*(1-as.numeric(tclvalue(SSenvir$pi1)))))
        assign("n",tclVar(ceiling(n)),envir=SSenvir)
        assign("type",7,envir=SSenvir)
        out = c("Odds Ratio: Point Estimate*\nn =",ceiling(n))
        sub = "* : see equation (16)"
        assign("printResult",out,envir=SSenvir)
        assign("eqnNumber",sub,envir=SSenvir)
        assign("plotType",14,envir=SSenvir)
    }
    # sample size for an odds ratio hypothesis test
    oddsRatioHyp <- function(...){
        if(as.numeric(tclvalue(SSenvir$nCalc))==1){
            evalq(tkconfigure(betaSlider,state="active",troughcolor=ccc))
            evalq(tkconfigure(betaSlider,var=beta),envir=SSenvir)
            evalq(tkconfigure(nSlider,state="disabled",troughcolor='grey'))
        }
        else{
            evalq(tkconfigure(betaSlider,state="disabled",troughcolor='grey'))
            evalq(tkconfigure(nSlider,state="active",troughcolor=ccc))
            evalq(tkconfigure(nSlider,var=n),envir=SSenvir)
        }
        evalq(tkconfigure(QeEntry,state="disabled"))
        evalq(tkconfigure(deltaEntry,state="disabled")) 
        evalq(tkconfigure(mu0Entry,state="disabled"))
        evalq(tkconfigure(sig0Entry,state="disabled"))
        evalq(tkconfigure(mu1Entry,state="disabled"))
        evalq(tkconfigure(sig1Entry,state="disabled"))
        evalq(tkconfigure(rhoEntry,state="disabled")) 
        evalq(tkconfigure(pi0Entry,state="normal"))
        evalq(tkconfigure(pi1Entry,state="normal"))
        evalq(tkconfigure(TEntry,state="disabled"))       
        evalq(tkconfigure(muiEntry,state="disabled"))
        getInput()
        p0 = as.numeric(tclvalue(SSenvir$pi0))
        p1 = as.numeric(tclvalue(SSenvir$pi1))
        if(as.numeric(tclvalue(SSenvir$nCalc))==1){
            n = (qnorm(1-as.numeric(tclvalue(SSenvir$alpha))/2)*sqrt(2*p1*(1-p1))+qnorm(1-as.numeric(tclvalue(SSenvir$beta)))*sqrt(p1*(1-p1)+p0*(1-p0)))^2/(p1-p0)^2
            assign("n",tclVar(ceiling(n)),envir=SSenvir)
            assign("type",7,envir=SSenvir)
            out=c("Odds Ratio: Hypothesis test*\nn =",ceiling(n)) 
            sub = "* : see equation (12)"
            assign("eqnNumber",sub,envir=SSenvir)
            assign("printResult",out,envir=SSenvir)
            assign("plotType",15,envir=SSenvir)
        }
        else{
            beta = 1-pnorm((sqrt(as.numeric(tclvalue(SSenvir$n)))*abs(p1-p0)-qnorm(1-as.numeric(tclvalue(SSenvir$alpha))/2)*sqrt(2*p1*(1-p1)))/sqrt(p1*(1-p1)+p0*(1-p0)))
            assign("beta",tclVar(beta),envir=SSenvir)
            assign("type",7,envir=SSenvir)
            out=c(expression("Odds Ratio: Hypothesis Test*\n",beta ,"= "),round(beta,4),"power =",1-round(beta,4)) 
            sub = "* : see equation (27)"
            assign("eqnNumber",sub,envir=SSenvir)
            assign("printResult",out,envir=SSenvir)
            assign("plotType",15,envir=SSenvir)
        }
    }
    # sample size for estimating a RR within a percent
    relativeRiskPoint <- function(...){
        evalq(tkconfigure(betaSlider,state="disabled",troughcolor='grey'))
        evalq(tkconfigure(nSlider,state="disabled",troughcolor='grey'))
        evalq(tkconfigure(QeEntry,state="disabled"))
        evalq(tkconfigure(deltaEntry,state="normal")) 
        evalq(tkconfigure(mu0Entry,state="disabled"))
        evalq(tkconfigure(sig0Entry,state="disabled"))
        evalq(tkconfigure(mu1Entry,state="disabled"))
        evalq(tkconfigure(sig1Entry,state="disabled"))
        evalq(tkconfigure(rhoEntry,state="disabled")) 
        evalq(tkconfigure(pi0Entry,state="normal"))
        evalq(tkconfigure(pi1Entry,state="normal"))
        evalq(tkconfigure(TEntry,state="disabled"))
        evalq(tkconfigure(muiEntry,state="disabled"))
        getInput()
        n = qnorm(1-as.numeric(tclvalue(SSenvir$alpha))/2)^2/log(1-as.numeric(tclvalue(SSenvir$delta)))^2*((1-as.numeric(tclvalue(SSenvir$pi0)))/(as.numeric(tclvalue(SSenvir$pi0)))+(1-as.numeric(tclvalue(SSenvir$pi1)))/(as.numeric(tclvalue(SSenvir$pi1))))
        assign("n",tclVar(ceiling(n)),envir=SSenvir)
        assign("type",7,envir=SSenvir)
        out = c("Relative Risk: Point Estimate*\nn =",ceiling(n))
        sub = "* : see equation (17)"
        assign("printResult",out,envir=SSenvir)
        assign("eqnNumber",sub,envir=SSenvir)
        assign("plotType",16,envir=SSenvir)
    }
    # sample size for a RR hypothesis test
    relativeRiskHyp <- function(...){
        if(as.numeric(tclvalue(SSenvir$nCalc))==1){
            evalq(tkconfigure(betaSlider,state="active",troughcolor=ccc))
            evalq(tkconfigure(betaSlider,var=beta),envir=SSenvir)
            evalq(tkconfigure(nSlider,state="disabled",troughcolor='grey'))
        }
        else{
            evalq(tkconfigure(betaSlider,state="disabled",troughcolor='grey'))
            evalq(tkconfigure(nSlider,state="active",troughcolor=ccc))
            evalq(tkconfigure(nSlider,var=n),envir=SSenvir)
        }
        evalq(tkconfigure(QeEntry,state="disabled"))
        evalq(tkconfigure(deltaEntry,state="disabled")) 
        evalq(tkconfigure(mu0Entry,state="disabled"))
        evalq(tkconfigure(sig0Entry,state="disabled"))
        evalq(tkconfigure(mu1Entry,state="disabled"))
        evalq(tkconfigure(sig1Entry,state="disabled"))
        evalq(tkconfigure(rhoEntry,state="disabled")) 
        evalq(tkconfigure(pi0Entry,state="normal"))
        evalq(tkconfigure(pi1Entry,state="normal"))
        evalq(tkconfigure(TEntry,state="disabled"))       
        evalq(tkconfigure(muiEntry,state="disabled"))
        getInput()
        p0 = as.numeric(tclvalue(SSenvir$pi0))
        p1 = as.numeric(tclvalue(SSenvir$pi1))
        if(as.numeric(tclvalue(SSenvir$nCalc))==1){
            n = (qnorm(1-as.numeric(tclvalue(SSenvir$alpha))/2)*sqrt((p1+p0)*(1-(p1+p0)/2))+qnorm(1-as.numeric(tclvalue(SSenvir$beta)))*sqrt(p1*(1-p1)+p0*(1-p0)))^2/(p1-p0)^2
            assign("n",tclVar(ceiling(n)),envir=SSenvir)
            assign("type",7,envir=SSenvir)
            out=c("Relative Risk: Hypothesis test*\nn =",ceiling(n)) 
            sub = "* : see equation (13)"
            assign("eqnNumber",sub,envir=SSenvir)
            assign("printResult",out,envir=SSenvir)
            assign("plotType",17,envir=SSenvir)
        }
        else{
            beta = 1-pnorm((sqrt(as.numeric(tclvalue(SSenvir$n)))*abs(p1-p0)-qnorm(1-as.numeric(tclvalue(SSenvir$alpha))/2)*sqrt((p1+p0)*(1-(p1+p0)/2)))/sqrt(p1*(1-p1)+p0*(1-p0)))
            assign("beta",tclVar(beta),envir=SSenvir)
            assign("type",7,envir=SSenvir)
            out=c(expression("Relative Risk: Hypothesis Test*\n",beta ,"= "),round(beta,4),"power =",1-round(beta,4)) 
            sub = "* : see equation (30)"
            assign("eqnNumber",sub,envir=SSenvir)
            assign("printResult",out,envir=SSenvir)
            assign("plotType",17,envir=SSenvir)
        }
    }
    # plotting function
    refresh2 <- function(index=NULL,...){
        plot.new()
        par(fig=c(0,1,0.70,1),mar=c(0,0,0,0))
        if(!is.numeric(index))
            index = SSenvir$plotType
        switch(index,
                "1" = meanPoint(),
                "2" = mean1(), 
                "3" = mean2Indep(),
                "4" = meanPaired(),
                "5" = propPoint(),
                "6" = prop1(),
                "7" = prop2Indep(),
                "8" = propPaired(),
                "9" = survScaled(),
                "10" = surv2Groups(),
                "11" = survRatio(),
                "12" = icc(),
                "13" = kappa(),
                "14" = oddsRatioPoint(),
                "15" = oddsRatioHyp(),
                "16" = relativeRiskPoint(),
                "17" = relativeRiskHyp()
        )
        out <- SSenvir$printResult
        if(length(out)==2){
            text(0.25,0.5,label=out[1],col=1,pos=4,family='',cex=1.7)
            text(0.32,0.41,label=out[2],col=6,pos=4,family='',cex=1.7)

        }
        else{
            text(0.25,0.50,out[1],col=1,pos=4,family='',cex=1.7)
            text(0.25,0.45,out[2],col=1,pos=4,family='',cex=1.7)
            text(0.28,0.45,out[3],col=1,pos=4,family='',cex=1.7)
            text(0.32,0.47,out[4],col=6,pos=4,family='',cex=1.7)
            text(0.25,0.25,out[5],col=1,pos=4,family='',cex=1.7)
            text(0.43,0.28,out[6],col=6,pos=4,family='',cex=1.7)
        }
        par(fig=c(0,1,0,0.70),mar=c(5,4,4,1))
        if(SSenvir$type==1){
            x1 = seq(from = as.numeric(tclvalue(SSenvir$mu0))-4*as.numeric(tclvalue(SSenvir$sig0)),
                    to = as.numeric(tclvalue(SSenvir$mu0))+4*as.numeric(tclvalue(SSenvir$sig0)),
                    length.out = 100)
            x2 = seq(from = as.numeric(tclvalue(SSenvir$mu1))-4*as.numeric(tclvalue(SSenvir$sig1)),
                    to = as.numeric(tclvalue(SSenvir$mu1))+4*as.numeric(tclvalue(SSenvir$sig1)),
                    length.out = 100)
            xLim = c(min(x1,x2),max(x1,x2))
            yLim = c(0,max(dnorm(x1,as.numeric(tclvalue(SSenvir$mu0)),as.numeric(tclvalue(SSenvir$sig0))),dnorm(x2,as.numeric(tclvalue(SSenvir$mu1)),as.numeric(tclvalue(SSenvir$sig1)))))
            par(new=T)
            par(fig=c(0,1,0,0.75),mar=c(5,3,1,1))
            plot(x1,dnorm(x1,as.numeric(tclvalue(SSenvir$mu0)),as.numeric(tclvalue(SSenvir$sig0))),xlim=xLim,ylim=yLim,type='l',col='red',xlab="",ylab="Frequency")
            points(x2,dnorm(x2,as.numeric(tclvalue(SSenvir$mu1)),as.numeric(tclvalue(SSenvir$sig1))),type='l',col='blue')
            legend("topright",legend=c(expression(paste("N(",mu[0],",",sigma[0],")",sep='')),expression(paste("N(",mu[1],",",sigma[1],")",sep=''))),pch=15,col=c(2,4))
            title(xlab=SSenvir$eqnNumber)
        }
        if(SSenvir$type==2){
            x1 = 1:100
            xLim=c(0,100)
            yLim=c(0,1)
            par(new=T)
            plot(dbinom(x1,100,as.numeric(tclvalue(SSenvir$pi0))),type='l',col='red',xlab="",ylab="count")
            points(dbinom(x1,100,as.numeric(tclvalue(SSenvir$pi1))),type='l',col='blue')
            legend("topright",legend=c(expression(paste("Bin(",pi[0],",100)",sep='')),expression(paste("Bin(",pi[1],",100)",sep=''))),pch=15,col=c(2,4))
            title(xlab=SSenvir$eqnNumber)
        }
        if(SSenvir$type==3){
            x1 = seq(from = as.numeric(tclvalue(SSenvir$mu0))-4*as.numeric(tclvalue(SSenvir$sig0)),
                    to = as.numeric(tclvalue(SSenvir$mu0))+4*as.numeric(tclvalue(SSenvir$sig0)),
                    length.out = 100)
            xLim = c(min(x1),max(x1))
            yLim = c(0,1)
            par(new=T)
            plot(x1,dnorm(x1,as.numeric(tclvalue(SSenvir$mu0)),as.numeric(tclvalue(SSenvir$sig0))),type='l',col='red',xlab="",ylab="frequency") 
            points(rep(as.numeric(tclvalue(SSenvir$mu0))-as.numeric(tclvalue(SSenvir$delta)),2),c(0,1),type='l',lty=2,col='blue')
            points(rep(as.numeric(tclvalue(SSenvir$mu0))+as.numeric(tclvalue(SSenvir$delta)),2),c(0,1),type='l',lty=2,col='blue')
            title(xlab=SSenvir$eqnNumber)
        }
        if(SSenvir$type==4){
            m = max(1.1*as.numeric(tclvalue(SSenvir$T)),1.5*as.numeric(tclvalue(SSenvir$mu0)),1.5*as.numeric(tclvalue(SSenvir$mu1)))
            x1 = seq(0,m,length.out=500)
            par(new=T)
            plot(x1,dexp(x1,rate=1/as.numeric(tclvalue(SSenvir$mu0))),type='l',col='red',xlab="",ylab="survived")
            points(x1,dexp(x1,rate=1/as.numeric(tclvalue(SSenvir$mu1))),type='l',col='blue')
            points(rep(as.numeric(tclvalue(SSenvir$T)),2),c(0,1),type='l',lty=2,col='black')
            title(xlab=SSenvir$eqnNumber)
        }
        if(SSenvir$type==5){
            x = 1:50
            e = as.numeric(tclvalue(SSenvir$delta))
            k = as.numeric(tclvalue(SSenvir$T))
            a = as.numeric(tclvalue(SSenvir$alpha))
            p = as.numeric(tclvalue(SSenvir$rho))
            nMain = nCalc(x,p,e,a)
            n1 = nCalc(x,0.1,e,a)
            n2 = nCalc(x,0.2,e,a)
            n3 = nCalc(x,0.3,e,a)
            n4 = nCalc(x,0.4,e,a)
            n5 = nCalc(x,0.5,e,a)
            n6 = nCalc(x,0.6,e,a)
            n7 = nCalc(x,0.7,e,a)
            n8 = nCalc(x,0.8,e,a)
            n9 = nCalc(x,0.9,e,a)
            par(new=T)
            yLim = c(0,n1[3])
            color = 2:6
            plot(x,nMain,type='l',lty=1,lwd=3,col=1,ylim=yLim,xlab="k",ylab="n")
            points(x,n1,type='l',lty=1,col=color[1])
#            points(x,n2,type='l',lty=2,col='blue')
            points(x,n3,type='l',lty=1,col=color[2])
#            points(x,n4,type='l',lty=2,col='blue')
            points(x,n5,type='l',lty=1,col=color[3])
#            points(x,n6,type='l',lty=2,col='blue')
            points(x,n7,type='l',lty=1,col=color[4])
#            points(x,n8,type='l',lty=2,col='blue')
            points(x,n9,type='l',lty=1,col=color[5])
            legend("topright",legend=expression(paste(rho,"=",.1),paste(rho,"=",.3),paste(rho,"=",.5),paste(rho,"=",.7),paste(rho,"=",.9)),pch=16,col=2:6)
            title(main=SSenvir$eqnNumber)
        }
        if(SSenvir$type==6){
            mui = as.numeric(strsplit(tclvalue(SSenvir$mui),split=',')[[1]])
            m = 1.6*max(mui)
            x = seq(0,m,length.out=500)
            lineCol = (rainbow(length(mui)))
            par(new=T)
            plot(x,dexp(x,rate=1/mui[1]),type='l',col=lineCol[1],xlab="",ylab="survived")
            for(i in 2:length(mui))
                points(x,dexp(x,rate=1/mui[i]),type='l',col=lineCol[i])
            legend("topright",legend=paste("=  ",mui),pch=181,col=lineCol) 
            title(xlab=SSenvir$eqnNumber)
        }
    }   
    ### setting up the GUI
    # buttons, sliders and inputs
    ccc = 'cyan'
    m <- tktoplevel()
    sliderFrame <- tkframe(m)
    f1 <- tkfont.create(family='symbol',size=10)
    alphaSlider <- tkscale(sliderFrame,command=refresh2,from=0.005,to=0.500,label=a,font=f1,orient = "horiz",resolution=0.005,state='active',troughcolor=ccc)
    betaSlider <- tkscale(sliderFrame,command=refresh2,from=0.005,to=0.500,label=b,font=f1,orient="horiz",resolution=0.005,troughcolor=ccc)
    nSlider <- tkscale(sliderFrame,command=refresh2,from=1,to=1000,label="n",orient="horiz",resolution=1,troughcolor=ccc)
    mu0Frame = tkframe(m)
    mu0Entry = tkentry(mu0Frame,width=5,bg=ccc)
    sig0Entry = tkentry(mu0Frame,width=5,bg=ccc)
    mu1Frame = tkframe(m)
    mu1Entry = tkentry(mu1Frame,width=5,bg=ccc)
    sig1Entry = tkentry(mu1Frame,width=5,bg=ccc)
    deltaFrame = tkframe(m)
    deltaEntry = tkentry(deltaFrame,width=5,bg=ccc)
    QFrame = tkframe(m)
    QeEntry = tkentry(QFrame,width=5,bg=ccc)
    rhoFrame = tkframe(m)
    rhoEntry = tkentry(rhoFrame,width=5,bg=ccc)
    piFrame = tkframe(m)
    pi1Entry = tkentry(piFrame,width=5,bg=ccc)
    pi0Entry = tkentry(piFrame,width=5,bg=ccc)
    TFrame = tkframe(m)
    TEntry = tkentry(TFrame,width=5,bg=ccc)
    muiFrame = tkframe(m)
    muiEntry = tkentry(muiFrame,width=35,bg=ccc)

    plotButton <- tkbutton(m,command=refresh2,text="Calculate")

    # assigning gui to the working envirironment

    # populate gui-items that need input
    tkinsert(mu0Entry,0,"1")
    tkinsert(mu1Entry,0,"5")
    tkinsert(sig0Entry,0,"1")
    tkinsert(sig1Entry,0,"1")
    tkinsert(deltaEntry,0,"0.5")
    tkinsert(QeEntry,0,"0.5")
    tkinsert(rhoEntry,0,"0")
    tkinsert(pi1Entry,0,"0.25")
    tkinsert(pi0Entry,0,"0.75")
    tkinsert(TEntry,0,"100")
    tkinsert(muiEntry,0,tclvalue(SSenvir$mui))

    # pack the gui
    tkpack(sliderFrame,side="top")
    tkpack(sliderFrame,alphaSlider,side="top")
    tkpack(sliderFrame,betaSlider,side="top")
    tkpack(sliderFrame,nSlider,side="top")
    tkpack(tklabel(mu0Frame,text=u1,font=f1),side='left')
    tkpack(mu0Frame,mu0Entry,side="left")
    tkpack(tklabel(mu0Frame,text=s1,font=f1),side="left")
    tkpack(mu0Frame,sig0Entry,side="top")
    tkpack(tklabel(mu1Frame,text=u2,font=f1),side='left')
    tkpack(mu1Frame,mu1Entry,side="left")
    tkpack(tklabel(mu1Frame,text=s2,font=f1),side='left')
    tkpack(mu1Frame,sig1Entry,side="top")
    tkpack(tklabel(deltaFrame,text="e",font=f1),side='left')
    tkpack(tklabel(deltaFrame,text="OR"),side='left')
    tkpack(tklabel(deltaFrame,text="d:",font=f1),side='left')
    tkpack(deltaFrame,deltaEntry,side="top")
    tkpack(tklabel(QFrame,text="Qe:"),side="left")
    tkpack(QFrame,QeEntry,side="top")
    tkpack(tklabel(rhoFrame,text=r,font=f1),side="left")
    tkpack(rhoFrame,rhoEntry,side="top")
    tkpack(tklabel(piFrame,text="k1",font=f1),side="left")
    tkpack(tklabel(piFrame,text="OR"),side="left")
    tkpack(tklabel(piFrame,text="p1:",font=f1),side="left")
    tkpack(piFrame,pi0Entry,side="left")
    tkpack(tklabel(piFrame,text="k2",font=f1),side="left")
    tkpack(tklabel(piFrame,text="OR"),side="left")
    tkpack(tklabel(piFrame,text="p2:",font=f1),side="left")
    tkpack(piFrame,pi1Entry,side="top")
    tkpack(tklabel(TFrame,text="k OR T"),side="left")
    tkpack(TFrame,TEntry,side="top")
    tkpack(tklabel(muiFrame,text="m",font=f1),side='left')
    tkpack(tklabel(muiFrame,text="i's:"),side='left')
    tkpack(muiFrame,muiEntry,side='top')

    tkpack(plotButton,side="bottom")

    # connecting the appropriate gui-elements
    assign("alphaSlider",alphaSlider,envir=SSenvir)
    assign("betaSlider",betaSlider,envir=SSenvir)
    assign("nSlider",nSlider,envir=SSenvir)
    evalq(tkconfigure(alphaSlider,var=alpha),envir=SSenvir)
    evalq(tkconfigure(betaSlider,var=beta),envir=SSenvir) 
    evalq(tkconfigure(nSlider,var=n),envir=SSenvir)

    # a happy attempt at menu-building
    topMenu <- tkmenu(m)
    meanMenu <- tkmenu(topMenu,tearoff=F)
    propMenu <- tkmenu(topMenu,tearoff=F)
    survMenu <- tkmenu(topMenu,tearoff=F)
    otherMenu <- tkmenu(topMenu,tearoff=F)
    mean1GroupMenu <- tkmenu(topMenu,tearoff=F)
    mean2IndepMenu <- tkmenu(topMenu,tearoff=F)
    meanPairedMenu<- tkmenu(topMenu,tearoff=F)
    prop1GroupMenu <- tkmenu(topMenu,tearoff=F)
    prop2IndepMenu <- tkmenu(topMenu,tearoff=F)
    propPairedMenu <- tkmenu(topMenu,tearoff=F)
    survRatioMenu <- tkmenu(topMenu,tearoff=F)
    surv2Menu <- tkmenu(topMenu,tearoff=F)
    survLargestMenu <- tkmenu(topMenu,tearoff=F)
    kappaMenu <- tkmenu(topMenu,tearoff=F)
    oddsMenu <- tkmenu(topMenu,tearoff=F)
    oddsHypMenu <- tkmenu(topMenu,tearoff=F)
    relRiskMenu <- tkmenu(topMenu,tearoff=F)
    relRiskHypMenu <- tkmenu(topMenu,tearoff=F)

    tkadd(topMenu,"cascade",label="Mean",menu=meanMenu)
    tkadd(topMenu,"cascade",label="Proportion",menu=propMenu)
    tkadd(topMenu,"cascade",label="Survival",menu=survMenu)
    tkadd(topMenu,"cascade",label="Other",menu=otherMenu)
    tkadd(meanMenu,"command",label="Point Estimate",command=function(){assign("nCalc",tclVar(0),envir=SSenvir);refresh2(1)})
    tkadd(meanMenu,"cascade",label="1 Group",menu=mean1GroupMenu)
    tkadd(meanMenu,"cascade",label="2 Independent Groups",menu=mean2IndepMenu)
    tkadd(meanMenu,"cascade",label="Paired Observations",menu=meanPairedMenu)
    tkadd(propMenu,"command",label="Point Estimate",command=function(){assign("nCalc",tclVar(0),envir=SSenvir);refresh2(5)})
    tkadd(propMenu,"cascade",label="1 Group",menu=prop1GroupMenu)
    tkadd(propMenu,"cascade",label="2 Independent Groups",menu=prop2IndepMenu)
    tkadd(propMenu,"cascade",label="Paired Observations",menu=propPairedMenu)
    tkadd(survMenu,"cascade",label="Ratio of Means",menu=survRatioMenu)
    tkadd(survMenu,"cascade",label="Two Groups",menu=surv2Menu)
    tkadd(survMenu,"cascade",label="Largest Ratio",menu=survLargestMenu)
    tkadd(otherMenu,"command",label="ICC",command=function(){assign("nCalc",tclVar(0),envir=SSenvir);refresh2(12)})
    tkadd(otherMenu,"cascade",label="Kappa",menu=kappaMenu)
    tkadd(otherMenu,"cascade",label="Odds",menu=oddsMenu)
    tkadd(otherMenu,"cascade",label="RR",menu=relRiskMenu)
    tkadd(oddsMenu,"cascade",label="Hypothesis",menu=oddsHypMenu)
    tkadd(relRiskMenu,"cascade",label="Hypothesis",menu=relRiskHypMenu)

    tkadd(mean1GroupMenu,"command",label="compute n",command=function(){assign("nCalc",tclVar(1),envir=SSenvir);refresh2(2)})
    tkadd(mean1GroupMenu,"command",label="compute beta",command=function(){assign("nCalc",tclVar(0),envir=SSenvir);refresh2(2)})
    tkadd(mean2IndepMenu,"command",label="compute n",command=function(){assign("nCalc",tclVar(1),envir=SSenvir);refresh2(3)})
    tkadd(mean2IndepMenu,"command",label="compute beta",command=function(){assign("nCalc",tclVar(0),envir=SSenvir);refresh2(3)})
    tkadd(meanPairedMenu,"command",label="compute n",command=function(){assign("nCalc",tclVar(1),envir=SSenvir);refresh2(4)})
    tkadd(meanPairedMenu,"command",label="compute beta",command=function(){assign("nCalc",tclVar(0),envir=SSenvir);refresh2(4)})
    tkadd(prop1GroupMenu,"command",label="compute n",command=function(){assign("nCalc",tclVar(1),envir=SSenvir);refresh2(6)})
    tkadd(prop1GroupMenu,"command",label="compute beta",command=function(){assign("nCalc",tclVar(0),envir=SSenvir);refresh2(6)})
    tkadd(prop2IndepMenu,"command",label="compute n",command=function(){assign("nCalc",tclVar(1),envir=SSenvir);refresh2(7)})
    tkadd(prop2IndepMenu,"command",label="compute beta",command=function(){assign("nCalc",tclVar(0),envir=SSenvir);refresh2(7)})
    tkadd(propPairedMenu,"command",label="compute n",command=function(){assign("nCalc",tclVar(1),envir=SSenvir);refresh2(8)})
    tkadd(propPairedMenu,"command",label="compute beta",command=function(){assign("nCalc",tclVar(0),envir=SSenvir);refresh2(8)})
    tkadd(survRatioMenu,"command",label="compute n",command=function(){assign("nCalc",tclVar(1),envir=SSenvir);refresh2(9)})
    tkadd(survRatioMenu,"command",label="compute beta",command=function(){assign("nCalc",tclVar(0),envir=SSenvir);refresh2(9)})
    tkadd(surv2Menu,"command",label="compute n",command=function(){assign("nCalc",tclVar(1),envir=SSenvir);refresh2(10)})
    tkadd(surv2Menu,"command",label="compute beta",command=function(){assign("nCalc",tclVar(0),envir=SSenvir);refresh2(10)})
    tkadd(survLargestMenu,"command",label="compute n",command=function(){assign("nCalc",tclVar(1),envir=SSenvir);refresh2(11)})
    tkadd(survLargestMenu,"command",label="compute beta",command=function(){assign("nCalc",tclVar(0),envir=SSenvir);refresh2(11)})
    tkadd(kappaMenu,"command",label="compute n",command=function(){assign("nCalc",tclVar(1),envir=SSenvir);refresh2(13)})
    tkadd(kappaMenu,"command",label="compute beta",command=function(){assign("nCalc",tclVar(0),envir=SSenvir);refresh2(13)})
    tkadd(oddsMenu,"command",label="point estimate",command=function(){assign("nCalc",tclVar(0),envir=SSenvir);refresh2(14)})
    tkadd(oddsHypMenu,"command",label="compute n",command=function(){assign("nCalc",tclVar(1),envir=SSenvir);refresh2(15)})
    tkadd(oddsHypMenu,"command",label="compute beta",command=function(){assign("nCalc",tclVar(0),envir=SSenvir);refresh2(15)})
    tkadd(relRiskMenu,"command",label="point estimate",command=function(){assign("nCalc",tclVar(0),envir=SSenvir);refresh2(16)})
    tkadd(relRiskHypMenu,"command",label="compute n",command=function(){assign("nCalc",tclVar(1),envir=SSenvir);refresh2(17)})
    tkadd(relRiskHypMenu,"command",label="compute beta",command=function(){assign("nCalc",tclVar(0),envir=SSenvir);refresh2(17)})


    tkconfigure(m,menu=topMenu)    
}
