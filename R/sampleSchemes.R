sampleSchemes <- function(gui=TRUE,sampleType=0,sampleSize=30,clusterSize=5,...){
    x = cumsum(c(0,6,12,7,8,7))*0.025
    yl = cumsum(c(0,3,4,3,4,3,4,4))*0.04
    yr = cumsum(c(0,2,5,2,5,2,5,4))*0.04
    SCenv<<-new.env()
    assign("u1",rep(0,36),env=SCenv)
    assign("u2",rep(0,36),env=SCenv)
    assign("u3",rep(0,36),env=SCenv)
    assign("g1",rep(0,40),env=SCenv)
    assign("g2",rep(0,40),env=SCenv)
    assign("g3",rep(0,40),env=SCenv)
    assign("sampleType",sampleType,env=SCenv)
    assign("sampleName","Simple Random Sampling",env=SCenv)
    assign("clusterSize",clusterSize,env=SCenv)
    assign("size",tclVar(sampleSize),env=SCenv)

    getSample <- function(...){
        if(gui){
            type = as.numeric(tclvalue(tkcurselection(sampleList)))
            if(!is.na(type)) assign("sampleType",type,env=SCenv)
            else type = SCenv$sampleType
            n = as.numeric(tclvalue(SCenv$size))
        }
        else{
            type = sampleType
            n = sampleSize
        }
        # Simple Random Sample
        if(type==0){
            assign("sampleName","Simple Random Sampling",env=SCenv)
            units = sample(1:228,n,replace=FALSE)
            colors = rep(0,228)
            colors[units]=2
            assign("u1",colors[0:36],env=SCenv)
            assign("u2",colors[37:72],env=SCenv)
            assign("u3",colors[73:108],env=SCenv)
            assign("g1",colors[109:148],env=SCenv)
            assign("g2",colors[149:188],env=SCenv)
            assign("g3",colors[189:228],env=SCenv)
        }
        # Stratify by dorm
        if(type==1){
            assign("sampleName","Stratified Random Sampling (by dorm)",env=SCenv)
            nS = floor(n/6)
            extra = n-nS*6
            extraUnit = sample(1:6,extra,replace=FALSE)
            nSize = rep(nS,6)
            nSize[extraUnit] = nSize[extraUnit]+1
            colors = rep(0,36)
            units=sample(1:36,nSize[1],replace=FALSE)
            colors[units]=colors[units]+2
            assign("u1",colors,env=SCenv)
            colors = rep(0,36)
            units=sample(1:36,nSize[2],replace=FALSE)
            colors[units]=colors[units]+2
            assign("u2",colors,env=SCenv)
            colors = rep(0,36)
            units=sample(1:36,nSize[3],replace=FALSE)
            colors[units]=colors[units]+2
            assign("u3",colors,env=SCenv)
            colors = rep(0,40)
            units=sample(1:40,nSize[4],replace=FALSE)
            colors[units]=colors[units]+2
            assign("g1",colors,env=SCenv)
            colors = rep(0,40)
            units=sample(1:40,nSize[5],replace=FALSE)
            colors[units]=colors[units]+2
            assign("g2",colors,env=SCenv)
            colors = rep(0,40)
            units=sample(1:40,nSize[6],replace=FALSE)
            colors[units]=colors[units]+2
            assign("g3",colors,env=SCenv)
        }
        # Stratify by student type
        if(type==2){
            assign("sampleName","Stratified Random Sampling (by student type)",env=SCenv)
            nS = floor(n/2)
            nSize = rep(nS,2)
            if(odd(n)){
                if(rbinom(1,1,0.5)) nSize[0]=nSize[0]+1
                else nSize[1]=nSize[1]+1
            }
            units = c(sample(1:108,nSize[1],replace=FALSE),sample(109:228,nSize[2],replace=FALSE))
            colors = rep(0,228)
            colors[units] = colors[units]+2
            assign("u1",colors[0:36],env=SCenv)
            assign("u2",colors[37:72],env=SCenv)
            assign("u3",colors[73:108],env=SCenv)
            assign("g1",colors[109:148],env=SCenv)
            assign("g2",colors[149:188],env=SCenv)
            assign("g3",colors[189:228],env=SCenv)
        }
        # cluster sample
        if(type==3){
            assign("sampleName","Cluster Sample (by floor)",env=SCenv)
            if(gui){
                d = as.numeric(tclvalue(tkget(clusterEntry)))
            }
            else{
                d = NA
            }
            if(!is.na(d)) assign("clustSize",d,env=SCenv)
            else d = SCenv$clusterSize
            floors = sample(1:24,d,replace=FALSE)
            floorSample = rep(0,24)
            floorSample[floors] = floorSample[floors]+2
            colors = vector()
            for(i in 1:9)
                colors = c(colors,rep(floorSample[i],12))
            for(i in 10:24)
                colors = c(colors,rep(floorSample[i],8))
            assign("u1",colors[0:36],env=SCenv)
            assign("u2",colors[37:72],env=SCenv)
            assign("u3",colors[73:108],env=SCenv)
            assign("g1",colors[109:148],env=SCenv)
            assign("g2",colors[149:188],env=SCenv)
            assign("g3",colors[189:228],env=SCenv)
        }
        # Synthetic Sample
        if(type==4){
            assign("sampleName","Synthetic Sample of every ith room",env=SCenv)
            if(gui){d = as.numeric(tclvalue(tkget(clusterEntry)))}
            else{d=NA}
            if(!is.na(d)) assign("clustSize",d,env=SCenv)
            else d = SCenv$clusterSize
            start = sample(1:d,1)
            colors = 1:228
            units = even((colors-start)/d)+even((colors-start)/d+1)
            colors = rep(0,228)
            colors[units==1] = colors[units==1]+2
            assign("u1",colors[0:36],env=SCenv)
            assign("u2",colors[37:72],env=SCenv)
            assign("u3",colors[73:108],env=SCenv)
            assign("g1",colors[109:148],env=SCenv)
            assign("g2",colors[149:188],env=SCenv)
            assign("g3",colors[189:228],env=SCenv)

        }
    }
    
    refresh <- function(...){
        getSample()
        uCol1=SCenv$u1
        uCol2=SCenv$u2
        uCol3=SCenv$u3
        gCol1=SCenv$g1
        gCol2=SCenv$g2
        gCol3=SCenv$g3

        # the counts per floor, dorm, student
        # student
        uCount = sum(uCol1/2+uCol2/2+uCol3/2)
        gCount = sum(gCol1/2+gCol2/2+gCol3/2)
        #dorm
        uDorm = c(sum(uCol1)/2,sum(uCol2)/2,sum(uCol3)/2)
        gDorm = c(sum(gCol1)/2,sum(gCol2)/2,sum(gCol3)/2)
        #floor: rows are dorms, columns are floors
        uFloor = rbind(rowSums(matrix(uCol1,nrow=3,byrow=T)/2),rowSums(matrix(uCol2,nrow=3,byrow=T)/2),rowSums(matrix(uCol3,nrow=3,byrow=T)/2))
        gFloor = rbind(rowSums(matrix(gCol1,nrow=5,byrow=T)/2),rowSums(matrix(gCol1,nrow=5,byrow=T)/2),rowSums(matrix(gCol3,nrow=5,byrow=T)/2)) 
   
        #uDorm1
        par(fig=c(0,1,0,1),mar=c(0,0,3,0))
        plot.new() 
        title(main=SCenv$sampleName)
        xTemp = seq(x[2],x[3],length.out=13)
        xLeft = xTemp[1:12]
        xRight = xTemp[2:13]
        yTemp = seq(yl[6],yl[7],length.out=4)
        yBottom = c(rep(yTemp[1],12),rep(yTemp[2],12),rep(yTemp[3],12))
        yTop = c(rep(yTemp[2],12),rep(yTemp[3],12),rep(yTemp[4],12))
        rect(xLeft,yBottom,xRight,yTop,density=-1,col=uCol1)
    
        #uDorm2
        xTemp = seq(x[2],x[3],length.out=13)
        xLeft = xTemp[1:12]
        xRight = xTemp[2:13]
        yTemp = seq(yl[4],yl[5],length.out=4)
        yBottom = c(rep(yTemp[1],12),rep(yTemp[2],12),rep(yTemp[3],12))
        yTop = c(rep(yTemp[2],12),rep(yTemp[3],12),rep(yTemp[4],12))
        rect(xLeft,yBottom,xRight,yTop,density=-1,col=uCol2)
    
        #uDorm3
        xTemp = seq(x[2],x[3],length.out=13)
        xLeft = xTemp[1:12]
        xRight = xTemp[2:13]
        yTemp = seq(yl[2],yl[3],length.out=4)
        yBottom = c(rep(yTemp[1],12),rep(yTemp[2],12),rep(yTemp[3],12))
        yTop = c(rep(yTemp[2],12),rep(yTemp[3],12),rep(yTemp[4],12))
        rect(xLeft,yBottom,xRight,yTop,density=-1,col=uCol3)
   
        #gDorm1
        xTemp = seq(x[4],x[5],length.out=9)
        xLeft = xTemp[1:8]
        xRight = xTemp[2:9]
        yTemp = seq(yr[6],yr[7],length.out=6)
        yBottom = c(rep(yTemp[1],8),rep(yTemp[2],8),rep(yTemp[3],8),rep(yTemp[4],8),rep(yTemp[5],8))
        yTop = c(rep(yTemp[2],8),rep(yTemp[3],8),rep(yTemp[4],8),rep(yTemp[5],8),rep(yTemp[6],8))
        rect(xLeft,yBottom,xRight,yTop,density=-1,col=gCol1)
    
        #gDorm2
        xTemp = seq(x[4],x[5],length.out=9)
        xLeft = xTemp[1:8]
        xRight = xTemp[2:9]
        yTemp = seq(yr[4],yr[5],length.out=6)
        yBottom = c(rep(yTemp[1],8),rep(yTemp[2],8),rep(yTemp[3],8),rep(yTemp[4],8),rep(yTemp[5],8))
        yTop = c(rep(yTemp[2],8),rep(yTemp[3],8),rep(yTemp[4],8),rep(yTemp[5],8),rep(yTemp[6],8))       
        rect(xLeft,yBottom,xRight,yTop,density=-1,col=gCol2)
    
        #gDorm3
        xTemp = seq(x[4],x[5],length.out=9)
        xLeft = xTemp[1:8]
        xRight = xTemp[2:9]
        yTemp = seq(yr[2],yr[3],length.out=6)
        yBottom = c(rep(yTemp[1],8),rep(yTemp[2],8),rep(yTemp[3],8),rep(yTemp[4],8),rep(yTemp[5],8))
        yTop = c(rep(yTemp[2],8),rep(yTemp[3],8),rep(yTemp[4],8),rep(yTemp[5],8),rep(yTemp[6],8))
        rect(xLeft,yBottom,xRight,yTop,density=-1,col=gCol3)
    
        #student labels
        text((x[2]+x[3])/2,0.5*(yl[8]-yl[7])+yl[7],labels="Undergraduate",pos=1)
        text(0.9*(x[3]-x[2])+x[2],0.5*(yl[8]-yl[7])+yl[7],labels=paste("  (",uCount,")",sep=''),col=4,pos=1)
        text((x[4]+x[5])/2,0.5*(yl[8]-yl[7])+yl[7],labels="Graduate",pos=1)
        text(0.9*(x[5]-x[4])+x[4],0.5*(yl[8]-yl[7])+yl[7],labels=paste("  (",gCount,")",sep=''),col=4,pos=1)

        #uFloor labels
        xTemp = seq(x[3],x[4],length.out=11)
        yTemp = seq(yl[6],yl[7],length.out=7)
        text(xTemp[1],yTemp[6],labels="3rd flr",pos=4,cex=0.80)
        text(xTemp[4],yTemp[6],labels=paste("  (",uFloor[1,3],")",sep=''),col=4,pos=4,cex=0.8)
        text(xTemp[1],yTemp[4],labels="2nd flr",pos=4,cex=0.80)
        text(xTemp[4],yTemp[4],labels=paste("  (",uFloor[1,2],")",sep=''),col=4,pos=4,cex=0.8)
        text(xTemp[1],yTemp[2],labels="1st flr",pos=4,cex=0.80)
        text(xTemp[4],yTemp[2],labels=paste("  (",uFloor[1,1],")",sep=''),col=4,pos=4,cex=0.8)
        text(x[2],yTemp[6],labels="1st Dorm",pos=2,cex=0.9)
        yTemp = seq(yl[4],yl[5],length.out=7)
        text(x[3],yTemp[6],labels=paste("(",uFloor[2,3],")",sep=''),col=4,pos=4,cex=0.80)
        text(x[3],yTemp[4],labels=paste("(",uFloor[2,2],")",sep=''),col=4,pos=4,cex=0.80)
        text(x[3],yTemp[2],labels=paste("(",uFloor[2,1],")",sep=''),col=4,pos=4,cex=0.80)
        text(x[2],yTemp[6],labels="2nd Dorm",pos=2,cex=0.9)
        yTemp = seq(yl[2],yl[3],length.out=7)
        text(x[3],yTemp[6],labels=paste("(",uFloor[3,3],")",sep=''),col=4,pos=4,cex=0.80)
        text(x[3],yTemp[4],labels=paste("(",uFloor[3,2],")",sep=''),col=4,pos=4,cex=0.80)
        text(x[3],yTemp[2],labels=paste("(",uFloor[3,1],")",sep=''),col=4,pos=4,cex=0.80)
        text(x[2],yTemp[6],labels="3rd Dorm",pos=2,cex=0.9)

        #gFloor labels
        xTemp = seq(x[5],x[6],length.out=11)
        yTemp = seq(yr[6],yr[7],length.out=11)
        text(xTemp[1],yTemp[10],labels="5th flr",pos=4,cex=0.80)
        text(xTemp[5],yTemp[10],labels=paste("(",gFloor[1,5],")",sep=''),col=4,pos=4,cex=0.8)
        text(xTemp[1],yTemp[8],labels="4th flr",pos=4,cex=0.80)
        text(xTemp[5],yTemp[8],labels=paste("(",gFloor[1,4],")",sep=''),col=4,pos=4,cex=0.8)
        text(xTemp[1],yTemp[6],labels="3rd flr",pos=4,cex=0.80)
        text(xTemp[5],yTemp[6],labels=paste("(",gFloor[1,3],")",sep=''),col=4,pos=4,cex=0.8)
        text(xTemp[1],yTemp[4],labels="2nd flr",pos=4,cex=0.80)
        text(xTemp[5],yTemp[4],labels=paste("(",gFloor[1,2],")",sep=''),col=4,pos=4,cex=0.8)
        text(xTemp[1],yTemp[2],labels="1st flr",pos=4,cex=0.80)
        text(xTemp[5],yTemp[2],labels=paste("(",gFloor[1,1],")",sep=''),col=4,pos=4,cex=0.8)
        yTemp = seq(yr[4],yr[5],length.out=11)
        text(x[5],yTemp[10],labels=paste("(",gFloor[2,5],")",sep=''),col=4,pos=4,cex=0.80)
        text(x[5],yTemp[8],labels=paste("(",gFloor[2,4],")",sep=''),col=4,pos=4,cex=0.80)
        text(x[5],yTemp[6],labels=paste("(",gFloor[2,3],")",sep=''),col=4,pos=4,cex=0.80)
        text(x[5],yTemp[4],labels=paste("(",gFloor[2,2],")",sep=''),col=4,pos=4,cex=0.80)
        text(x[5],yTemp[2],labels=paste("(",gFloor[2,1],")",sep=''),col=4,pos=4,cex=0.80)
        yTemp = seq(yr[2],yr[3],length.out=11)
        text(x[5],yTemp[10],labels=paste("(",gFloor[3,5],")",sep=''),col=4,pos=4,cex=0.80)
        text(x[5],yTemp[8],labels=paste("(",gFloor[3,4],")",sep=''),col=4,pos=4,cex=0.80)
        text(x[5],yTemp[6],labels=paste("(",gFloor[3,3],")",sep=''),col=4,pos=4,cex=0.80)
        text(x[5],yTemp[4],labels=paste("(",gFloor[3,2],")",sep=''),col=4,pos=4,cex=0.80)
        text(x[5],yTemp[2],labels=paste("(",gFloor[3,1],")",sep=''),col=4,pos=4,cex=0.80)

        #dorm counts
        text((x[2]+x[3])/2,yl[6],labels=paste("(",uDorm[1],")",sep=''),col=4,pos=1,cex=0.9)
        text((x[2]+x[3])/2,yl[4],labels=paste("(",uDorm[2],")",sep=''),col=4,pos=1,cex=0.9)
        text((x[2]+x[3])/2,yl[2],labels=paste("(",uDorm[3],")",sep=''),col=4,pos=1,cex=0.9)
        text((x[4]+x[5])/2,yr[6],labels=paste("(",gDorm[1],")",sep=''),col=4,pos=1,cex=0.9)
        text((x[4]+x[5])/2,yr[4],labels=paste("(",gDorm[2],")",sep=''),col=4,pos=1,cex=0.9)
        text((x[4]+x[5])/2,yr[2],labels=paste("(",gDorm[3],")",sep=''),col=4,pos=1,cex=0.9)
    }
  if(gui){
    m <- tktoplevel()
    tkwm.title(m,"Sampling Schemes")
    listFrame <- tkframe(m)
    sampleList <- tklistbox(listFrame,width=-1,height=-1)
    plotButton <- tkbutton(m,command=refresh,text="Sample")
    nSlider <- tkscale(m,from=1,to=228,orient='horiz',label='n',variable=SCenv$size)
    clusterFrame <- tkframe(m)
    clusterEntry <- tkentry(clusterFrame,width=7,bg='white')

    tkinsert(sampleList,"end","Simple Random Sample")
    tkinsert(sampleList,"end","Stratified Random Sample (by dorm)")
    tkinsert(sampleList,"end","Stratified Random Sample (by student type)")
    tkinsert(sampleList,"end","Cluster Sample (by floor)")
    tkinsert(sampleList,"end","Synthetic Sample")
    tkinsert(clusterEntry,0,"5")

    tkpack(tklabel(listFrame,text="Methods"),side="top")
    tkpack(listFrame,sampleList,side="top")
    tkpack(nSlider,side="top")
    tkpack(tklabel(clusterFrame,text="synthetic step size / # of clusters:"),side='left')
    tkpack(clusterFrame,clusterEntry,side='top')
    tkpack(plotButton,side="top")
  }
  else{refresh()}
}
