tau <- function(k,alpha,beta){
    power<-function(level,noncentr,df=1) {
        crit.val<-qchisq(1-level,df=df)
        return(1-pchisq(crit.val,df=df,ncp=noncentr))
    }
    result = 0
    range = 1:100
    powRange = power(alpha,range,k)
    index = sort(abs(powRange-(1-beta)),index.return=TRUE)$ix[1]
    result = powRange[index]
    currTau = range[index]
    while(abs(result-(1-beta))>0.001){
        limits = c(currTau-abs(result-(1-beta)),currTau+(abs(result+(1-beta))))
        range = seq(limits[1],limits[2],length.out=50)
        powRange = power(alpha,range,k)
        index = sort(abs(powRange-(1-beta)),index.return=TRUE)$ix[1]
        result = powRange[index]
        currTau = range[index]
    }
    return(currTau)
}

