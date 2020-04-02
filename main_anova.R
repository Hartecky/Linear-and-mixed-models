# ANOVA Analysis of Variance

data("iris")
iris$season = rbinom(150,1,prob=0.5)

#One factor Anova

one_factor_anova = function(y_dependent, effect ,dataset){
  arguments = sys.call()
  n = dim(dataset)[1]
  
  mean_Y = mean(y_dependent)
  mean_G = aggregate(y_dependent, by = list(effect), FUN = "mean")
  
  colnames(mean_G) = c("Groups", "mean_G")

  unique_counter = length(unique(effect))

  # Statystyki
  SS_t = sum((y_dependent - mean_Y)^2)
  SS_b = sum((mean_G$mean_G - mean_Y)^2)*(n/unique_counter)
  
  # Średni kwadrat wewnątrz grup
  SS_w = SS_t - SS_b

  #Błąd średnio kwadratowy
  MSE = SS_w / (n - unique_counter)

  # Statystyka F oraz p-wartość
  F = (SS_b/(unique_counter-1))/(SS_w/(n-unique_counter))
  p_wartosc = pf(F, length(unique(effect)) - 1, dim(dataset)[1] - length(unique(effect)) , lower.tail = FALSE)
  p_wartosc = pf(F,length(unique(iris$Species))-1,dim(iris)[1]-length(unique(iris$Species)), lower.tail = FALSE)
  significance = c()
  # p-value signif codes
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
  # OUTPUT
  cat("Call: \n lm(formula = " , as.character(arguments[2]), "~ ", as.character(arguments[3]), ", data = ", as.character(arguments[4]), ") \n")
  
  #DF
  cat("\n Degrees of freedom : ", as.character(arguments[3]), ": ", as.character(unique_counter-1) )
  cat("\n -------------------: ", as.character("Residuals"), ": ", as.character(n - unique_counter), "\n" )
  
  #Sum SQ
  cat("\n Sum Sq : ", as.character(arguments[3]), ": ", SS_b)
  cat("\n ------ : ", as.character("Residuals"), ": ", SS_w, "\n")
  
  #Sum SQ
  cat("\n Mean Sq : ", as.character(arguments[3]), ": ", SS_t/unique_counter)
  cat("\n ------- : ", as.character("Residuals"), ": ", MSE, "\n")
  
  #F-val
  cat("\n F-value : ", as.character(arguments[3]), ": ", F)
  cat("\n --------- ")
  
  #P-value
  cat("\n Pr (>F) : ", as.character(arguments[3]), ": ", p_wartosc, significance)
  cat("\n ---------")
  
  #Signif codes
  cat("\n Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 \n")
  
  # Tukey test
  if (p_wartosc < 0.05) {
    HSD_TUKEY(n,unique_counter, MSE, mean_G)
  } else {
    break
  }
}

two_factor_anova = function(y_dependent, effect_a, effect_b, dataset){
  arguments = sys.call()
  n = dim(dataset)[1]
  unique_counter_a = length(unique(effect_a))
  unique_counter_b = length(unique(effect_b))

  mean_Y = mean(y_dependent)
  
  mean_a = aggregate(y_dependent, by = list(effect_a), FUN = "mean")
  mean_b = aggregate(y_dependent, by = list(effect_b), FUN = "mean")
  
  colnames(mean_a) = c("Groups_a", "Mean_G")
  colnames(mean_b) = c("Groups_b", "Mean_G")
  
  box = table(effect_b)
  #Statistics
  SSt = sum((y_dependent - mean_Y)^2)
  SSa = sum((mean_a$Mean_G - mean_Y)^2) * (n/unique_counter_a)
  SSb = sum((mean_b$Mean_G - mean_Y)^2 * c(box[1:length(box)]))

  SSe = SSt - SSa - SSb
  
  MSE = SSe / (n - unique_counter_a)
  MST = SSe / (n - unique_counter_b)
  MSR = SSe / (n- unique_counter_a - unique_counter_b)
  
  Fa = (SSa / (length(unique(effect_a)) - 1)) / (SSe / (-1+dim(dataset)[1]-length(unique(effect_a))))
  Fb = (SSb / (length(unique(effect_b)) - 1)) / (SSe / (-1+dim(dataset)[1]-length(unique(effect_b))))
  
  pA = pf(Fa, (n - unique_counter_a - 1),unique_counter_a -1, lower.tail = FALSE)
  pB = pf(Fb, (n - unique_counter_b - 2), (unique_counter_b - 1), lower.tail = FALSE)
  
  
  significancea = c()
  # p-value signif codes
  for (i in 1:length(p_wartosc)) {
    if (pA[i] < 0.001) {
      significancea[i] = " *** "
    }
    else if (pA[i] < 0.01) {
      significancea[i] = " ** "
    }
    else if (pA[i] < 0.05) {
      significancea[i] = " * "
    }
    else if (pA[i] < 0.1) {
      significancea[i] = " . "
    }
    else {
      significancea[i] = " ' ' "
    }
  } 
  
  significanceb = c()
  # p-value signif codes
  for (i in 1:length(p_wartosc)) {
    if (pB[i] < 0.001) {
      significance[i] = " *** "
    }
    else if (pB[i] < 0.01) {
      significanceb[i] = " ** "
    }
    else if (pB[i] < 0.05) {
      significanceb[i] = " * "
    }
    else if (pB[i] < 0.1) {
      significanceb[i] = " . "
    }
    else {
      significanceb[i] = " ' ' "
    }
  }
  
  
  # OUTPUT
  cat("Call: \n lm(formula = " , as.character(arguments[2]), "~ ", as.character(arguments[3]), "+", as.character(arguments[4]), ", data = ", as.character(arguments[4]), ") \n")
  
  #DF
  cat("\n Degrees of freedom : ", as.character(arguments[3]), ": ", as.character(unique_counter_a-1) )
  cat("\n -------------------: ", as.character(arguments[4]), ": ", as.character(unique_counter_b -1) )
  cat("\n -------------------: ", as.character("Residuals"), ": ", as.character(n - unique_counter_a - 1), "\n" )
  
  #Sum SQ
  cat("\n Sum Sq : ", as.character(arguments[3]), ": ", SSa)
  cat("\n ------ : ", as.character(arguments[4]), ": ", MSE)
  cat("\n ------ : ", as.character("Residuals"), ": ", SSw, "\n")
  
  #Mean SQ
  cat("\n Mean Sq : ", as.character(arguments[3]), ": ", SSt/unique_counter_a)
  cat("\n ------- : ", as.character(arguments[4]), ": ", MST)
  cat("\n ------- : ", as.character("Residuals"), ": ", MSR, "\n")

  # F-val
  cat("\n F-value : ", as.character(arguments[3]), ": ", Fa)
  cat("\n ------- : ", as.character(arguments[4]), ": ", Fb)
  cat("\n --------- ")

  #P-value
  cat("\n Pr (>F) : ", as.character(arguments[3]), ": ", pA, significancea)
  cat("\n ------- : ", as.character(arguments[4]), ": ", pB, significanceb)
  cat("\n ---------")

  #Signif codes
  cat("\n Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 \n")
}

HSD_TUKEY = function(n,unique_counter, MSE, mean_G){
  # Wartość z tablic standaryzowanego rozkładu t-studenta
  q = qtukey(0.05, (n/unique_counter) - 1, unique_counter, lower.tail = FALSE)
  
  # Statystyka HSD
  HSD = q * sqrt(MSE / unique_counter)
  
  # Średnie
  means = mean_G$mean_G
  
  # Różnice średnich
  diffs = combn(means, length(means)-1 , FUN = diff)
  diffs = as.vector(diffs)
  
  cat("\n Post-hoc HSD Tukey Test \n")
  cat("\n HSD Statistic Value: ", HSD)
  cat("\n Groups: \n", as.character(mean_G$Groups[1:length(mean_G$Groups)]))
  cat("\n Means: \n", as.character(mean_G$mean_G[1:length(mean_G$mean_G)]))
  cat("\n Differences: \n", diffs, "\n")
  
  for (i in 1:unique_counter) {
    if (diffs[i] > HSD) {
      cat(mean_G$Groups[i], "***")
    }
    else if (diffs[i] < HSD) {
      cat(mean_G$Groups[i], "There are no signif differences \n")
    }
  }
}

one_factor_anova(iris$Sepal.Length, iris$Species, iris)
two_factor_anova(iris$Sepal.Length, iris$Species, factor(iris$season), iris)

summary(aov(m1))
summary(aov(m2))

TukeyHSD(aov(m1))


