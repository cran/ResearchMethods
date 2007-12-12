mainGUI <- function(...){
    data(MFSV)
    attach(MFSV)
    data(agpop)
    attach(agpop)
    Menv <<- new.env()
    refresh <- function(fn,...){
        if(fn=="BlandAltman"){
            go <- function(...){x <- eval(parse(text=tclvalue(tkget(xEntry))));y <- eval(parse(text=tclvalue(tkget(yEntry))));BlandAltman(x,y)}
            # need a function to clear the menu
            tkdestroy(Menv$m)
            repack()
            f <- tkframe(Menv$m)
            xEntry <- tkentry(f,width=10,bg='white')
            yEntry <- tkentry(f,width=10,bg='white')
            button <- tkbutton(f,text='Plot',command=go)
            tkinsert(xEntry,0,"MF")
            tkinsert(yEntry,0,"SV")
            tkgrid(tklabel(f,text='Bland Altman Plot'),columnspan=4,sticky='ew')
            tkgrid(tklabel(f,text='new treatment results\n(as a vector):'),xEntry,tklabel(f,text='existing treatment results\n(as a vector):'),yEntry)
            tkgrid(button,columnspan=4)
            tkgrid(f)
        }
        if(fn=="caseDist"){
            # need a function to clear the menu
            tkdestroy(Menv$m)
            repack()
            f <- tkframe(Menv$m)
            pEntry <- tkentry(f,width=5,bg='white')
            tkinsert(pEntry,0,"0.9")
            button <- tkbutton(f,text='Run',command=function(){caseDist(eval(parse(text=tclvalue(tkget(pEntry)))))})
            tkgrid(pEntry)
            tkgrid(button)
            tkgrid(f)
        }
    }

    repack <- function(...){
    m <- tktoplevel(width=250)
    main <- tkmenu(m)
    tkconfigure(m,menu=main)

    agreementMenu <- tkmenu(main)
    samplingMenu <- tkmenu(main)
    analysisMenu <- tkmenu(main)

    tkadd(main,"cascade",label="Agreement",menu=agreementMenu)
    tkadd(main,"cascade",label="Sampling",menu=samplingMenu)
    tkadd(main,"cascade",label="Analysis",menu=analysisMenu)

    tkadd(agreementMenu,"command",label='Bland Altman',command=function(){refresh("BlandAltman")})
    tkadd(agreementMenu,"command",label='Case Distribution',command=function(){refresh("caseDist")})
    tkadd(agreementMenu,"command",label='Outlier Test',command=function(){refresh("outlierTest")})
    tkadd(agreementMenu,"command",label='Rho Range',command=function(){refresh("rhoRange")})
    tkadd(agreementMenu,"command",label='Rho Scale',command=function(){refresh("rhoScale")})

        assign("m",m,env=Menv)
    }
    repack()
}
