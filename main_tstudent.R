# Function for calculating T-student test and for testing hypothesis
# a) One sample
# b) Two samples independent
# c) Two samples dependent


#Vectors of data
X = rnorm(150, mean = 16, sd = 1)
Y = rnorm(140, mean = 12, sd = 4)

# Checking if vector contains only numeric values
check_nums = function(vec) {
  if (is.numeric(vec) == TRUE) {
    return(TRUE)
  } else {
    return("Vector has unallowed characters")
    break
  }
}
# Checking the normality of the data
check_normality = function(vec) {
  normality <- shapiro.test(vec)
  if (normality$p.value < 0.05) {
    print("Data does not come from a normal distribution")
  } else {
    return(TRUE)
  }
}
#Checking length of vectors
check_length = function(vec_a, vec_b){
  if (length(vec_a) != length(vec_b)) {
    print("Vectors length is not equal")
  } else {
    return(list(vec_a, vec_b))
  }
}

#Test for one sample
test_t1 = function(X,X0,alternative,alpha){
  n = length(X)
  mu = mean(X)
  S = sd(X)
  
  #Calculate the statistics
  test_value = sqrt(n) * (mu - X0) / S
  
  #Hipothesis decision
  if (alternative==1) {
    p=pt(test_value,n-1, lower.tail=F) 
  } else if (alternative==2) {
    p=pt(test_value,n-1) 
  } else {
    p=2*pt(abs(test_value),n-1, lower.tail=F) 
  }
  if (p>alpha) {
    dec=0
  } else {
    dec=1
  }
  list(statystyka=test_value, p_value=p, decyzja=dec)
}
#Test for two independent samples
test_t2 = function(X, Y, alternative, alpha) {
  m1=mean(X)
  m2=mean(Y)
  
  v1=var(X)
  v2=var(Y)
  
  n1=length(X)
  n2=length(Y)
  
  #Calculate the statistics
  test_value=(m1-m2)/sqrt((n1-1)*v1+(n2-1)*v2)*sqrt(n1*n2*(n1+n2-2)/(n1+n2))
  
  if (alternative==1) {
    p=pt(test_value,n1+n2-2, lower.tail=F) 
  } else if (alternative==2) {
    p=pt(test_value,n1+n2-2) 
  } else {
    p=2*pt(abs(test_value),n1+n2-2, lower.tail=F) 
  }
  if (p>alpha) {
    dec=0
  } else {
    dec=1
  }
  list(stat=test_value, p_value=p, dec=dec)
}
#Test for two dependent samples 
test_t3 = function(X, Y, alpha) {
  D=X-Y
  
  m=mean(D)
  s=sd(D)
  n=length(D)
  
  test_value=sqrt(n)*(m)/s
  
  p=pt(T,n-1, lower.tail=F) 
  
  if (p>alpha) {
    dec=0
  } else {
    dec=1
  }
  list(stat=test_value, p_value=p, dec=dec)
}

main_ttest = function(X,X0,Y,alternative,alpha){
  
  if (missing(X0)==TRUE && missing(alternative)==TRUE){
    if (length(X)==length(Y)) {
      print("T-test for two dependent samples")
      test_t3(X,Y,alpha)
    } else {
      return("End of program. Vector's lengths are not equal")
    }

  } else if (missing(X0)==TRUE) {
    print("T-Test for two independent samples")
    test_t2(X,Y,alternative,alpha)
  } else {
    print("T-test for one sample")
    test_t1(X,X0,alternative,alpha)
  }
}

