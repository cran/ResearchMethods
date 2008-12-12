cltDemo <- function(){
  CDenv <- new.env()
  runVar <- tclVar(1)
  CDenv$n = 1
  refresh <- function(...){
    x = as.numeric(tclvalue(tkget(varEntry)))
    if(is.na(x)){
      x = 1
      tkdelete(varEntry,0,"end")
      tkinsert(varEntry,0,'1')
    }
    if(tclvalue(runVar)=='3' && (x != round(x) || x < 0)){
      cat("df for the chi-square distribution must be a positive integer\n")
      x = 1
      tkdelete(varEntry,0,"end")
      tkinsert(varEntry,0,'1')
    }
    if(tclvalue(runVar)=='2' && x < 0){
      cat("theta for the exponential distribution must be positive\n")
      x = 1
      tkdelete(varEntry,0,"end")
      tkinsert(varEntry,0,'1')
    }
    switch((tclvalue(runVar)),
           '1' = clt.unif(1),
           '2' = clt.exp(1,x),
           '3' = clt.chisq(1,x),
           '4' = clt.parab(1))
    CDenv$n=1
  }
  nextStep <- function(...){
    CDenv$n=CDenv$n+1
    x = as.numeric(tclvalue(tkget(varEntry)))
    if(is.na(x)){
      x = 1
      tkdelete(varEntry,0,"end")
      tkinsert(varEntry,0,'1')
    }
    if(tclvalue(runVar)=='3' && (x != round(x) || x < 0) ){
      cat("df for the chi-square distribution must be a positive integer\n")
      x = 1
      tkdelete(varEntry,0,"end")
      tkinsert(varEntry,0,'1')
    }
    if(tclvalue(runVar)=='2' && x < 0){
      cat("theta for the exponential distribution must be positive\n")
      x = 1
      tkdelete(varEntry,0,"end")
      tkinsert(varEntry,0,'1')
    }
    switch((tclvalue(runVar)),
           '1' = clt.unif(CDenv$n),
           '2' = clt.exp(CDenv$n,x),
           '3' = clt.chisq(CDenv$n,x),
           '4' = clt.parab(CDenv$n))    
  }
  clt.unif <- function(n=20) {
    N <- 10000
    par(mfrow = c(1,2), pty = "s")
    m <- (rowMeans(matrix(runif(N*n), N, n)) - 0.5)*sqrt(12*n)
    hist(m, breaks = "FD", xlim = c(-4,4),
         main = paste("Uniform(0,1), n = ",n,sep=""),
         prob = TRUE, ylim = c(0,0.5), col = "lemonchiffon")
    box()
    pu <- par("usr")[1:2]
    x <- seq(pu[1], pu[2], len = 500)
    lines(x, dnorm(x), col = "red")
    qqnorm(m, ylim = c(-4,4), xlim = c(-4,4), pch = ".", col = "blue")
    abline(0, 1, col = "red")
  }
  
  clt.exp <- function(n=20,theta=1) {
    N <- 10000
    par(mfrow = c(1,2), pty = "s")
    m <- (rowMeans(matrix(rexp(N*n,1/theta), N, n)) - theta)/sqrt(theta^2/n)
    hist(m, breaks = "FD", xlim = c(-4,4),
         main = paste("exp(",theta,"), n = ",n,sep=""),
         prob = TRUE, ylim = c(0,0.5), col = "lemonchiffon")
    box()
    pu <- par("usr")[1:2]
    x <- seq(pu[1], pu[2], len = 500)
    lines(x, dnorm(x), col = "red")
    qqnorm(m, ylim = c(-4,4), xlim = c(-4,4), pch = ".", col = "blue")
    abline(0, 1, col = "red")
  }
  
  clt.chisq <- function(n=20,nu=1) {
    N <- 10000
    par(mfrow = c(1,2), pty = "s")
    m <- (rowMeans(matrix(rchisq(N*n,nu), N, n)) - nu)/sqrt(2*nu/n)
    hist(m, breaks = "FD", xlim = c(-4,4),
         main = paste("Chi-Square(",nu,"), n = ",n,sep=""),
         prob = TRUE, ylim = c(0,0.5), col = "lemonchiffon")
    box()
    pu <- par("usr")[1:2]
    x <- seq(pu[1], pu[2], len = 500)
    lines(x, dnorm(x), col = "red")
    qqnorm(m, ylim = c(-4,4), xlim = c(-4,4), pch = ".", col = "blue")
    abline(0, 1, col = "red")
  }
  
  clt.parab <- function(n=20) {
    rparab <- function(nn) {
      u <- runif(nn,-1,1)
      sign(u)*abs(u)^(1/3)
    }
    N <- 10000
    par(mfrow = c(1,2), pty = "s")
    m <- (rowMeans(matrix(rparab(N*n), N, n)))/sqrt(3/(5*n))
    hist(m, breaks = "FD", xlim = c(-4,4),
         main = paste("n = ",n,sep=""),
         prob = TRUE, ylim = c(0,0.5), col = "lemonchiffon")
    box()
    pu <- par("usr")[1:2]
    x <- seq(pu[1], pu[2], len = 500)
    lines(x, dnorm(x), col = "red")
    qqnorm(m, ylim = c(-4,4), xlim = c(-4,4), pch = ".", col = "blue")
    abline(0, 1, col = "red")
  }
  m <- tktoplevel()
  buttonFrame <- tkframe(m)
  otherFrame <- tkframe(m)
  unifButton <- tkradiobutton(buttonFrame,command=refresh,variable=runVar,value=1,text='uniform distribution')
  expButton <- tkradiobutton(buttonFrame,command=refresh,variable=runVar,value=2,text='exponential distribution')
  chisqButton <- tkradiobutton(buttonFrame,command=refresh,variable=runVar,value=3,text='chi-square distribution')
  parButton <- tkradiobutton(buttonFrame,command=refresh,variable=runVar,value=4,text='parabolic distribution')
  nextButton <- tkbutton(otherFrame,command=nextStep,text='increase n')
  varEntry <- tkentry(otherFrame,bg='white',width=4)
  tkgrid(buttonFrame)
  tkgrid(otherFrame)
  tkgrid(unifButton)
  tkgrid(expButton)
  tkgrid(chisqButton)
  tkgrid(parButton)
  tkgrid(nextButton,columnspan=2)
  tkgrid(tklabel(otherFrame,text='theta OR df'),varEntry)
  tkinsert(varEntry,0,'1')
  refresh()
}
