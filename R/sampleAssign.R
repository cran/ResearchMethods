sampleAssign <- function(gui=T,n=30,treat=3,coinProb=0.6,type=0,...){
    SAenv <<- new.env()
    assign("n",tclVar(n),env=SAenv)
    assign("numTreat",tclVar(treat),env=SAenv)
    assign("allocType",type,env=SAenv)
    assign("title","",env=SAenv)

    getP <- function(...){
        if(gui) p <- as.numeric(tclvalue(tkget(pEntry)))
        else p = coinProb
        if(!is.na(p) && p <= 1 && p >= 0.5) return(p)
        else{
          if(gui){
            tkdelete(pEntry,0,"end")
            tkinsert(pEntry,0,"0.6")
          }
            return(0.6) 
        }
    }
    
    refresh <- function(...){
        if(gui) index = as.numeric(tclvalue(tkcurselection(schemeList)))
        else index = NA
        if(is.na(index)) index = SAenv$allocType
        else assign("allocType",index,env=SAenv)
        if(gui) n = as.numeric(tclvalue(tkget(nEntry)))
        else n = NA
        if(is.na(n) || n < 0){
          n = as.numeric(tclvalue(SAenv$n))
          if(gui){
            tkinsert(nEntry,0,n)
          }
        }
        else assign("n",tclVar(n),env=SAenv)
        if(gui) num = as.numeric(tclvalue(tkget(treatEntry)))
        else num = treat
        if(!is.na(num) && num > 0) assign("numTreat",tclVar(num),env=SAenv)
        #simple randomization
        if(index == 0){
            treat = LETTERS[1:num]
            treatCount = rep(0,length(treat))
            limit = floor(10/length(treat))*length(treat)-1
            numList = sample(0:limit,n,replace=TRUE)
            bound = split(0:limit,treat)
            assignList = rep("",n)
            colorList = rep(0,n)
            for(i in 1:length(treat)){
                for(j in 1:length(bound[i][[1]])){
                    assignList[numList==bound[i][[1]][j]]=treat[i]
                    treatCount[i] = treatCount[i] + sum(numList==bound[i][[1]][j])
                    colorList[numList==bound[i][[1]][j]]=rainbow(length(treat))[i]
                }
            }
            assign("title","Simple Randomization",env=SAenv)
        }
        # Random Permuted Blocks
        if(index==1){
            treat = LETTERS[1:num]
            treatCount = rep(0,length(treat))
            treatPerm = permutations(length(treat),length(treat),treat)
            numList = sample(1:dim(treatPerm)[1],ceiling(n/dim(treatPerm)[2]),replace=TRUE)
            assignList = as.vector(t(treatPerm[numList,]))[1:n]
            colorList = rep(0,n)
            for(i in 1:length(treat)){
                colorList[assignList==treat[i]]=rainbow(length(treat))[i]
                treatCount[i] = sum(assignList==treat[i])
            }
            assign("title","Random Permuted Blocks",env=SAenv)
        }
        # Biased Coin
        if(index==2){
          if(gui){
            assign("numTreat",tclVar(2),env=SAenv)
            tkdelete(treatEntry,0,"end")
            tkinsert(treatEntry,0,2)
          }
            treat = LETTERS[1:num]
            treatCount = rep(0,2)
            numList = sample(0:99,n,replace=TRUE)
            p = 100*getP()
            assignList = vector()
            colorList = vector()
            specialList = rep(0,n)
            for(i in 1:n){
                if(treatCount[1]==treatCount[2]){
                    if(numList[i]<50){
                        assignList = c(assignList,"A")
                        treatCount[1] = treatCount[1]+1 
                        colorList = c(colorList,"Red")
                    }
                    else{
                        assignList = c(assignList,"B")
                        treatCount[2] = treatCount[2]+1 
                        colorList = c(colorList,"Blue")
                    }
                }
                else if(treatCount[1]>treatCount[2]){
                    if(numList[i]<(100-p)){
                        assignList = c(assignList,"A")
                        treatCount[1] = treatCount[1]+1 
                        colorList = c(colorList,"Red")
                    }
                    else{
                        assignList = c(assignList,"B")
                        treatCount[2] = treatCount[2]+1
                        if(numList[i]<50) colorList = c(colorList,"Cyan")
                        else colorList = c(colorList,"Blue")
                    }
                }
                else{
                    if(numList[i]<p){
                        assignList = c(assignList,"A")
                        treatCount[1] = treatCount[1]+1
                        if(numList[i]>50) colorList = c(colorList,"Pink")
                        else colorList = c(colorList,"Red")
                    }
                    else{
                        assignList = c(assignList,"B")
                        treatCount[2] = treatCount[2]+1 
                        colorList = c(colorList,"Blue")
                    }
                }
            }
            assign("title","Biased Coin",env=SAenv)
        }
        #finding plot limits
        temp = round(n/(1:n)) - n/(1:n)
        fact = cbind(1:n,n/(1:n))[temp==0,]
        yCount = fact[sort(abs(fact[,1]-fact[,2]),index.return=TRUE)$ix[1],1]
        xCount = n/yCount
        xL = 0:(xCount-1)/xCount
        xR = 1:xCount/xCount
        yB = 0:(yCount-1)/yCount
        yT = 1:yCount/yCount

        plot.new()
        par(fig=c(0,1,0.3,1),mar=c(1,1,1,1))
        k = 0
        m = 0
        count=1
        for(i in 1:xCount){
            for(j in yCount:1){
                k = k+1
                rect(xL[i],yB[j],xR[i],yT[j],density=-1,col=colorList[k])
                text((xL[i]+xR[i])/2,(yB[j]+yT[j])/2,assignList[k])
            }
        }
        # boxing the permutations
        if(index==1){
            for(i in 1:xCount){
                for(j in yCount:1){
                    m = m + 1
                    color='black'
                    if(m <= (num)){
                        points(c(xL[i],xL[i]),c(yB[j],yT[j]),type='l',lwd=5,col=color)
                        points(c(xR[i],xR[i]),c(yB[j],yT[j]),type='l',lwd=5,col=color)
                    }
                    if(m==1)
                        points(c(xL[i],xR[i]),c(yT[j],yT[j]),type='l',lwd=5,col=color)
                    if(m==num)
                        points(c(xL[i],xR[i]),c(yB[j],yB[j]),type='l',lwd=5,col=color)
                    if(m%%(2*num)==0){ m=0;count=count+1 }

                }
            }
        }
        title(main=SAenv$title)
        par(new=TRUE)
        par(fig=c(0,1,0,0.3))
        output = paste(treat,":",treatCount)
        textplot(output,halign="center",valign="top")
        assign("letters",assignList,env=SAenv)
    }
    if(gui){
      m <- tktoplevel()
      tkwm.title(m,"Assigning Subjects to a Treatment")
      listFrame <- tkframe(m)
      schemeList <- tklistbox(listFrame,width=25,height=-1)
      nFrame <- tkframe(m)
      nEntry <- tkentry(nFrame,bg='white',width=5)
      pFrame <- tkframe(m)
      pEntry <- tkentry(pFrame,bg='white',width=5)
      treatFrame <- tkframe(m)
      treatEntry <- tkentry(treatFrame,bg='white',width=5)
      plotButton <- tkbutton(m,text="Sample",command=refresh)
  
      tkinsert(schemeList,"end","Simple Randomization")                   # 1
      tkinsert(schemeList,"end","Random Permuted Blocks")                 # 2
      tkinsert(schemeList,"end","Biased Coin")                            # 3
      tkinsert(nEntry,0,"30")
      tkinsert(treatEntry,0,"3")
      tkinsert(pEntry,0,"0.6")
 
      tkpack(tklabel(listFrame,text="Allocation Scheme"),side="top")
      tkpack(listFrame,schemeList,side="top")
      tkpack(tklabel(nFrame,text="n:"),side="left")
      tkpack(nFrame,nEntry,side="top")
      tkpack(tklabel(treatFrame,text="number of treatments:"),side="left")
      tkpack(treatFrame,treatEntry,side="top")
      tkpack(tklabel(pFrame,text='p (for Biased Coin):'),side='left')
      tkpack(pFrame,pEntry,side='top')
      tkpack(plotButton,side="bottom")
    }
    else{
      refresh()
    }
}  
