### Main corelations script

### Pearson
pearson = function(x,y) {
  n=length(x)
  mX=mean(x)
  mY=mean(y)
  R=sum((x-mX)*(y-mY)) / 
    sqrt(sum((x-mX)^2)*sum((y-mY)^2))
  T=R/sqrt(1-R^2)*sqrt(n-2)
  p=2*pt(abs(T), n-2, lower.tail=F)
  list(R=R, p=p)
}

### Spearman
spearman=function(x,y) {
  rangi=function(x) {
    n=length(x)
    new.x=sort(x)
    new.x=as.data.frame(new.x)
    colnames(new.x)='obs'
    x=as.data.frame(x)
    x$ranga=0
    colnames(x)='obs'
    new.n=length(unique(x))
    if (n==new.n) {
      new.x$rangi=1:n
      for (i in 1:n) {
        x$ranga[i]=new.x$rangi[new.x$obs==x$obs[i]]
      }   
    } else {
      new.x=cbind(new.x, 1:n)
      colnames(new.x)=c('obs','rank1')
      new.x=as.data.frame(new.x)
      tmp=aggregate(new.x$rank1, by=list(new.x$obs),
                    FUN='mean')
      colnames(tmp)=c('obs','ranga')
      for (i in 1:n) {
        x$ranga[i]=tmp$ranga[tmp$obs==x$obs[i]]
      } 
    }
    return(rangi=x$ranga)
  }
  R=rangi(x)
  S=rangi(y)
  n=length(x)
  
  Rs=1-6*sum((R-S)^2)/(n*(n^2-1))
  Ts=Rs/sqrt(1-Rs^2)*sqrt(n-2)
  p=2*pt(abs(Ts),n-2,lower.tail=FALSE)
  list(Rs=Rs, p=p)
}

### Kendalls-Tau
kendall = function(x,y){
  zgodny = 0
  niezgodny = 0
  
  for (i in X) {
    if (isTRUE(X[i] <= Y[i+1])) {
      zgodny = sum(zgodny + Y[i + 1] , na.rm = TRUE)
    } else if (isTRUE(X[i] >= Y[i+1])){
      niezgodny = sum(niezgodny + X[i+1], na.rm = TRUE)
    }
  }
  corelation = (zgodny - niezgodny) / (zgodny + niezgodny)
  return(corelation)
}

### Main functions, based on method argument

corelations = function(x,y,method){
  if (length(x)!=length(y)) {
    print("FALSE, the vectors are not equal")
  } else {
    if (method=="pearson"){
      pearson(x,y)
    } else if (method=="spearman") {
      spearman(x,y)
    } else if (method=="kendall"){
      kendall(x,y)
    } else {
      print("FALSE, there's no method")
    }
  }
}


### Testing vectors
X = as.numeric(seq(1:10))
Y = c(2,4,5,7,9,8,1,3,6,10)
Z = c(1,7,4,3,2,6,7,8)

