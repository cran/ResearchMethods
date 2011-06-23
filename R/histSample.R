histSample <- function(dist=NA,gui=TRUE,yMax=FALSE,n=10000,sub=NaN,par=c(0,1)){
  if(is.na(dist)) type = 'norm'
  else type = dist
  HSenvir <<- new.env()
  if(is.nan(sub)){
    subset = .1*n
  }
  else{
    subset = sub
  }
  assign("subset",tclVar(subset),envir=HSenvir)
  choose <- function(...){
    if(gui){
      index = as.numeric(tclvalue(tkcurselection(HSenvir$options)))
      x1 = tclvalue(tkget(parEntry1))
      x2 = tclvalue(tkget(parEntry2))
      if(!is.na(index)){
          type <- HSenvir$names[index+1]
      }
      if(!is.na(x1)){
          if(is.na(x2) || type=='exp' || type=='pois') par <- eval(parse(text=x1))
          else par <- c(eval(parse(text=x1)),eval(parse(text=x2)))
      }
    }
    else{
      x1 = par[1]
      x2 = par[2]
      if(!is.na(x1)){
          if(is.na(x2) || type=='exp' || type=='pois') par <- eval(parse(text=x1))
          else par <- c(eval(parse(text=x1)),eval(parse(text=x2)))
      }
    }
    warn = paste("incorrect parameter format:\n\t\"norm\":\t\tpar = c(mu,sig) \n\t\"exp\":\t\tpar=lambda\n\t\"chisq\":\tpar=c(df,ncp)\n\t\"gamma\":\tpar=c(shape,scale)\n\t\"pois\":\t\tpar=lambda\nYou entered type =",type,"with par =",x1,x2)
    if(type == 'pois'){
      if(length(par)!=1 || par <= 0){
          if(gui)
          stop(warn)
      }
      dat <- rpois(n,par)
      title = substitute(paste("Poisson Distriubtion: ",lambda," = ",p1),list(p1=par[1]))
      lineData = seq(floor(min(dat)),ceiling(max(dat)),length.out=n)
      lineData = round(lineData)
      linePoints = dnorm(lineData,par)
    }
    if(type == 'norm'){
      if(length(par)!=2 || par[2] <= 0)
          stop(warn)
      dat <- rnorm(n,par[1],par[2])
      title = substitute(paste("Normal Distriubtion: ",mu," = ",p1,", ",sigma," = ",p2),list(p1=par[1],p2=par[2]))
      lineData = seq(floor(min(dat)),ceiling(max(dat)),length.out=n)
      linePoints = dnorm(lineData,par[1],par[2])
    }
    if(type == 'exp'){
      if(length(par)!=1 || par <= 0)
          stop(warn)
      dat <- rexp(n,par)
      title = substitute(paste("Exponential Distriubtion: ",lambda," = ",p1),list(p1=par[1]))
      lineData = seq(floor(min(dat)),ceiling(max(dat)),length.out=n)
      linePoints = dexp(lineData,par)
    }
    if(type == 'chisq'){
      if(length(par)!=2 || par[1] <= 0)
          stop(warn)
      dat <- rchisq(n,par[1],par[2])
      title = substitute(paste("Chi-Squared Distriubtion: df = ",p1,", nco = ",p2),list(p1=par[1],p2=par[2]))
      lineData = seq(floor(min(dat)),ceiling(max(dat)),length.out=n)
      linePoints = dchisq(lineData,par[1],par[2])
    }
    if(type == 'gamma'){
      if(length(par)!=2 || par[1] <= 0 || par[2] <= 0)
          stop(warn)
      dat <- rgamma(n,shape=par[1],scale=par[2])
      title = substitute(paste("Gamma Distriubtion: ",alpha," = ",p1,", ",beta," = ",p2),list(p1=par[1],p2=par[2]))
      lineData = seq(floor(min(dat)),ceiling(max(dat)),length.out=n)
      linePoints = dgamma(lineData,shape=par[1],scale=par[2])
    }
    if(yMax) yLim = c(0,max(linePoints)*1.5)
    else yLim = c(0,1)
    splits = quantile(lineData,prob=seq(0,1,0.04),names=FALSE)
    assign("runDown",0,envir=HSenvir)
    assign("dat",dat,envir=HSenvir)
    assign("lineData",lineData,envir=HSenvir)
    assign("linePoints",linePoints,envir=HSenvir)
    assign("yLim",yLim,envir=HSenvir)
    assign("splits",splits,envir=HSenvir)
    assign("title",title,envir=HSenvir)
    if(gui){
      evalq(tkconfigure(sampleSize,variable=subset),envir=HSenvir)
    }
    refresh()
  }

  refresh <- function(...){
    data = HSenvir$dat[1:as.numeric(tclvalue(HSenvir$subset))]
    plotData = hist(data,breaks=HSenvir$splits,freq=FALSE,ylim=HSenvir$yLim,plot=TRUE,main="",col='red')
    title(main = HSenvir$title)
    points(HSenvir$lineData,HSenvir$linePoints,type='l',lwd=3,col='blue') 
  }
  if(gui){
    m <- tktoplevel()
    eFrame <- tkframe(m)
    sampleSize <- tkscale(m,command=refresh,from=1,to=n,orient="horiz",label="Sample Size",resolution=1,showvalue=TRUE,length=500)
    dists <- tclVar("{Normal (mu,sig)} {Exponential (lambda)} {Chi-Squared (df,ncp)} {Gamma (shape,scale)} {Poisson (lambda)}")
    names <- c("norm","exp","chisq","gamma","pois")
    options <- tklistbox(m,listvariable=dists,height=-1,width=-1)
    changeButton <- tkbutton(eFrame,command=choose,text='change distribution')
    parEntry1 <- tkentry(eFrame,bg='white',width=5)
    parEntry2 <- tkentry(eFrame,bg='white',width=5)
    tkinsert(parEntry1,0,par[1])
    tkinsert(parEntry2,0,par[2])

    tkgrid(sampleSize,columnspan=2)
    tkgrid(options,eFrame)
    tkgrid.configure(options,sticky='e')
    tkgrid(changeButton,sticky='w',columnspan=2)
    tkgrid(parEntry1,tklabel(eFrame,text='(mu, lambda, df, shape)'),sticky='w')
    tkgrid(parEntry2,tklabel(eFrame,text='(sig, ncp, scale)'), sticky='w')
    assign("options",options,envir=HSenvir)
    assign("names",names,envir=HSenvir)
    choose()
    assign("sampleSize",sampleSize,envir=HSenvir)

  }
  else{choose()}
}
