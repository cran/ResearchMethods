minimAllocGUI <- function(factors=NULL,file=NULL,prob=1){

    getFile <- function(fileName){
        MAenv <<- new.env()
        input <- readLines(fileName)
        if(fileName!="tempFile.dat"){
            writeLines(input,"tempFile.dat")
            print("HERE")
        }
        MAenv$prob = as.numeric(input[1])
        factors <- list()
        factors[1] <- strsplit(input[2],';')
        for(i in 3:(length(factors[1][[1]])+2)){
            factors[i-1] <- strsplit(input[i],';')
        }
        patMat = c("",strsplit(input[length(factors[1][[1]])+3],';')[[1]])
        for(i in (length(factors[1][[1]])+4):length(input)){
            patMat = rbind(patMat,strsplit(input[i],';')[[1]])
        }
        colnames(patMat) = patMat[1,]
        patMat = patMat[2:dim(patMat)[1],]
        rownames(patMat) = patMat[,1]
        patMat = patMat[,2:dim(patMat)[2]]
        MAenv$factors = factors
        MAenv$patMat = patMat
        MAenv$fileName = fileName
    }

    putFile <- function(out){
        write(MAenv$prob,file=out)
        cat(c(MAenv$factors[1][[1]],'\n'),file=out,append=TRUE,sep=';')
        for(i in 2:(length(MAenv$factors[1][[1]])+1)){
            cat(MAenv$factors[i][[1]],file=out,append=TRUE,sep=';')
            cat('\n',file=out,append=TRUE,sep=';')
        }
        suppressWarnings(write.table(MAenv$patMat,file=out,append=TRUE,quote=FALSE,sep=';'))
    }

    plotResult <- function(...){
        # plotting the results
        numLev = length(unlist(MAenv$factors[2:1000]))
        numFact = length(MAenv$factors[1][[1]])
        
        xL = c(0,0.4,0.8,0.9)
        xR = (1:4)/4
        yB = (0:(numFact+numLev+1))/(numLev+numFact+2)
        yT = (1:numFact+numLev+2)/(numLev+numFact+2)
        k = 0
        par(mar=c(1,1,1,1))
        top = c("Factor","Levels","Trt. A","Trt. B")
        plot.new()
        #titles
        for(i in 1:4){
            text(xL[i],yB[numLev+numFact+2],top[i],pos=4,)
        }
        #first column
        first = numFact+numLev+1
        for(i in 2:(numFact+1)){
            lev = length(MAenv$factors[i][[1]])
            first = first - lev
            text(xL[1],mean(yB[first:(first+lev-1)]),MAenv$factors[1][[1]][i-1],pos=4,)
            first = first - 1
        }
        #second, third and fourth column
        first = numFact+numLev
        for(i in 1:numFact){
            lev = MAenv$factors[i+1][[1]]
            for(j in 1:length(lev)){
                patients = MAenv$patMat[MAenv$patMat[,i]==j,]
                text(xL[2],yB[first],lev[j],pos=4,)
                if(is.matrix(patients)){
                    aCount = sum(patients[,numFact+1]=="A")
                    bCount = sum(patients[,numFact+1]=="B")
                }
                else if(length(patients)>0){
                    aCount = sum(patients[numFact+1]=="A")
                    bCount = sum(patients[numFact+1]=="B")
                }
                else{
                    aCount = 0
                    bCount = 0
                }
                     
                text(xL[3],yB[first],aCount,pos=4,)
                text(xL[4],yB[first],bCount,pos=4,)
                first = first - 1
            }
            first = first - 1
        }
    }
    allocate <- function(...){
        # a function to coordinate the check buttons
        check <- function(...){
            numFact = length(MAenv$factors[1][[1]])
            for(i in 1:numFact){
                numLev = length(MAenv$factors[i+1][[1]])
                val = rep(0,numFact)
                for(j in 1:numLev){
                    val[j] <- as.numeric(tclvalue(get(paste("var",i,".",j,sep=''),env=MAenv)))
                }
                if(sum(val)>1){
                    # need to reset the points
                    for(j in 1:numLev){
                        tkdeselect(get(paste("check",i,".",j,sep=''),env=MAenv))
                    }
                }
            }
        }
        add <- function(...){
            if(!exists("patMat",env=MAenv)){
                patMat = matrix(rep(0,length(MAenv$factors[1][[1]])+1),nrow=1)
                colnames(patMat) <- c(MAenv$factors[1][[1]],"treatment")
                assign("patMat",patMat,env=MAenv)
            }
            # getting the appropriate input from the buttons
            ### THIS HAS CHANGED WITH THE RADIO BUTTONS
            tempRow = rep(0,length(MAenv$factors[1][[1]]))
            for(i in 1:(length(MAenv$factors[1][[1]]))){
                tempRow[i] = as.numeric(tclvalue(get(paste("var",i,sep=""),env=MAenv)))
            }
            if(!any(tempRow==0)){
                # setting the new treatment
                aCount = 0
                bCount = 0
                numFact = length(MAenv$factors[1][[1]])
                for(i in 1:numFact){
                    treat = MAenv$patMat[MAenv$patMat[,i]==tempRow[i],numFact+1]
                    aCount = aCount + sum(treat=="A")
                    bCount = bCount + sum(treat=="B")
                }
                if(aCount > bCount) {
                    tempRow[numFact+1] = LETTERS[rbinom(1,1,MAenv$prob)+1]
                }
                else if(aCount < bCount){
                    tempRow[numFact+1] = LETTERS[rbinom(1,1,1-MAenv$prob)+1]
                }
                else{
                    tempRow[numFact+1] = LETTERS[rbinom(1,1,0.5)+1]
                }
                MAenv$patMat = rbind(MAenv$patMat,tempRow)
                rownames(MAenv$patMat) <- 1:dim(MAenv$patMat)[1]
                plotResult()
            }
        }
        numFact = length(MAenv$factors[1][[1]])
        m <- tktoplevel()
        tkwm.title(m,"Minimum Allocation Technique")
        for(i in 1:numFact){
            tempFrame <- tkframe(m)
            numLev <- length(MAenv$factors[i+1][[1]])
            # gridding instead of packing now
            # tkpack(tklabel(tempFrame,text=MAenv$factors[1][[1]][i]),side="left")
            tkgrid(tempFrame)
            tkgrid(tklabel(tempFrame,text=MAenv$factors[1][[1]][i]),rowspan=numLev+1,sticky='e')
            assign(paste("var",i,sep=""),tclVar(0),env=MAenv)           
            for(j in 1:numLev){
                # using radio buttons instead of check buttons
                # assign(paste("var",i,".",j,sep=""),tclVar(0),env=MAenv)
                # tempCheck <- tkcheckbutton(tempFrame,text=MAenv$factors[i+1][[1]][j],variable=get(paste("var",i,".",j,sep=""),env=MAenv),command=check)
                # tkpack(tempFrame,tempCheck,side="top")
                # assign(paste("check",i,".",j,sep=""),tempCheck,env=MAenv)
                tempRadio <- tkradiobutton(tempFrame,text=MAenv$factors[i+1][[1]][j],variable=get(paste("var",i,sep=""),env=MAenv),value=j)
                tkgrid(tempRadio,row=j,column=2,sticky='w')
                assign(paste("radio",i,".",j,sep=""),tempRadio,env=MAenv)
            }
        }
        addButton <- tkbutton(m,command=add,text="Apply Treatment")
        qFrame <- tkframe(m)
        saveButton <- tkbutton(qFrame,command=function(...){putFile(MAenv$fileName);tkdestroy(m)},text="Save and Quit")
        quitButton <- tkbutton(qFrame,command = function(...)tkdestroy(m),text="Quit without saving")
        tkgrid(addButton,columnspan=2)
        tkgrid(qFrame)
        tkgrid(saveButton,quitButton)
    }

    if(prob > 1 || prob < 0)
        stop("prob must be between 0 and 1\n")
    if(prob < 0.5)
        warning("probability below 0.5 does not make sense: consult help file\n")
    if(!is.null(file)){
        getFile(file)
        plotResult()
        allocate()
    }
    else{
       MAenv <<- new.env()
       MAenv$factors <- list()
       MAenv$prob <- prob
       MAenv$fileName <- "tempFile.dat"
       if(is.null(factors)){
            refresh <- function(...){
                fact = tclvalue(tkget(factorName))
                MAenv$factors[1][[1]] = c(MAenv$factors[1][[1]],fact)
                levels = strsplit(tclvalue(tkget(factorDivides)),";")
                MAenv$factors[length(MAenv$factors)+1] = levels
                tkinsert(factorList,"end",paste(fact,": ",levels,sep=""))
                tkdelete(factorName,0,"end")
                tkdelete(factorDivides,0,"end")
            }
            m <- tktoplevel()
            nameFrame <- tkframe(m)
            divideFrame <- tkframe(m)
            listFrame <- tkframe(m)
            factorName <- tkentry(nameFrame,bg='white',width=15)
            factorDivides <- tkentry(divideFrame,bg='white',width=25)
            factorList <- tklistbox(listFrame,width=25,height=-1)
            buttonFrame <- tkframe(m)
            exitButton <- tkbutton(buttonFrame,text="Return Factors",command=function(){tkdestroy(m);allocate()})
            goButton <- tkbutton(buttonFrame,text="Continue",command=refresh)
      
            tkpack(tklabel(nameFrame,text="Enter the name of the next factor"),side="left")
            tkpack(nameFrame,factorName,side="top")
            tkpack(tklabel(divideFrame,text="Enter the individual factor names (separated by ;)"),side="top")
            tkpack(divideFrame,factorDivides,side="top")
            tkpack(tklabel(listFrame,text="Factors:"),side="top")
            tkpack(listFrame,factorList,side="top")
            tkpack(buttonFrame,goButton,side="right")
            tkpack(buttonFrame,exitButton,side="left")
            tkpack(buttonFrame,side="bottom")
        }
        else{
            MAenv$factors <- factors
            allocate()
        }
    }    
}
