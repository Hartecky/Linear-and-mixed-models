### LINEAR CORRELATION FUNCTION

# Example dataset
dane = c(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Length, iris$Petal.Width)

#Main function
new_reg = function(y_var,...,dataset){
  # get the arguments from a function
  # and extract the x_variables
  
  arguments = as.list(match.call())
  arguments_len = length(arguments)
  x_arguments = arguments[4:arguments_len-1]
  
  x_variables = c()
  len = length(x_arguments)
  for (i in 1:len) {
    x_variables[i] = x_arguments[i]
  }

  #Create matrix
  n=dim(dataset)[1]
  X = matrix(1, length(y_var), length(x_variables)+1)
  
  #Get the values from dataset
  for (i in 1:length(x_variables)+1) {
   X[, i] = dataset[, i]
  }
  
  #Calculate coefficients
  k = dim(dataset)[2]-2
  y_var = as.matrix(dataset[,1])
  alpha = solve(t(X)%*%X)%*%t(X)%*%y_var
  
  #Calculate resids
  y_res = X%*%alpha
  residua = y_var -y_res
  
  S2e = sum(residua^2)/(n-k-1)
  
  D = S2e*solve(t(X)%*%X)
  Salpha = sqrt(diag(D))
  
  testT = alpha/Salpha
  p_wartosc = 2*pt(abs(testT), n-k-1, lower.tail=FALSE)
  
  # Show significance
  significance = c()

  for (i in 1:length(p_wartosc)) {
    if (p_wartosc[i] < 0.001) {
      significance[i] = " *** "
    }
    else if (p_wartosc[i] < 0.01) {
      significance = " ** "
    }
    else if (p_wartosc[i] < 0.05) {
      significance = " * "
    }
    else if (p_wartosc[i] < 0.1) {
      significance = " . "
    }
    else {
      significance = " ' ' "
    }
  }
  
  R2=1 - sum(residua^2)/sum((y_var-mean(y_var))^2)
  
  F=(R2/(1-R2))*(n-k-1)/(k)
  p_wartoscF = pf(F, k,n-k-1, lower.tail=FALSE)

    
  # Print the results
  cat("Call: \n lm(formula = " , as.character(arguments[2]), "~ ")
  cat(as.character(x_variables), sep = " + ")
  cat(", data =", as.character(tail(arguments, n=1)), ") \n")
  
  cat("\n Residuals: \n", as.character(summary(residua)), "\n")
  cat("\n Coefficients: \n")
  cat("\n Estimate: \n", "Intercept", as.character(x_variables), "\n" ,alpha, "\n")
  cat("\n Std. Error: \n","Intercept", as.character(x_variables), "\n", Salpha, "\n")
  cat("\n t value: \n", "Intercept", as.character(x_variables), "\n", testT, "\n")
  cat("\n Pr(>|t|)", as.character(x_variables), "\n", p_wartosc, "\n")
  cat(significance, "\n")
  cat("---\n")
  cat(" Signif. codes: 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1", "\n\n")
  cat(" Residual standard error: ", round(sqrt(S2e),5), "on ", n-k-1, "degrees of freedom \n",
      "R-squared: ", round(R2,5), "\n",
      "F-statistics: ", as.character(F), "on", k, "and", n-k-1, "DF", "p-value: ", p_wartoscF)
}


new_reg(iris$Sepal.Length, iris$Sepal.Width,iris$Petal.Length,iris$Petal.Width, dataset=iris)

summary(lm(iris$Sepal.Length ~ iris$Sepal.Width + iris$Petal.Length + iris$Petal.Width, data = iris))


