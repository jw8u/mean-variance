library(MASS)
library(quantmod)

mv_stock = function(n,k,risk_coef){
  #vector of ones
  one_vector = rep(1,k)
  
  #get stock data
  getSymbols(c("AAPL","AMGN","ADBE","CAT","COP","INTC","GIS","BA","JNJ","VZ"),from='2000-01-01')
  sample_data = cbind(weeklyReturn(last(AAPL,paste0(n," weeks"))), weeklyReturn(last(AMGN,paste0(n," weeks"))), weeklyReturn(last(ADBE,paste0(n," weeks"))), weeklyReturn(last(CAT,paste0(n," weeks"))), weeklyReturn(last(COP,paste0(n," weeks"))), weeklyReturn(last(INTC,paste0(n," weeks"))), weeklyReturn(last(GIS,paste0(n," weeks"))), weeklyReturn(last(BA,paste0(n," weeks"))), weeklyReturn(last(JNJ,paste0(n," weeks"))), weeklyReturn(last(VZ,paste0(n," weeks"))))
  sample_mean = colMeans(sample_data)
  cov_sample = (n-1)*cov(sample_data)
  
  Q = solve(cov_sample) - (solve(cov_sample) %*% one_vector %*% t(one_vector) %*% solve(cov_sample))/as.numeric(t(one_vector) %*% solve(cov_sample) %*% one_vector)
  expected_return_sample = (t(one_vector) %*% solve(cov_sample) %*% sample_mean)/(t(one_vector) %*% solve(cov_sample) %*% one_vector) + (t(sample_mean) %*% Q %*% sample_mean)/(risk_coef *1/(n-1))
  c = 1/(n-k-1) + (2*n-k-1)/(n*(n-k-1)*(n-k-2))
  expected_return_bayesian = (t(one_vector) %*% solve(cov_sample) %*% sample_mean)/(t(one_vector) %*% solve(cov_sample) %*% one_vector) + (t(sample_mean) %*% Q %*% sample_mean)/(risk_coef *c)
  
  R_S = (t(one_vector) %*% solve(cov_sample) %*% sample_mean)/(t(one_vector) %*% solve(cov_sample) %*% one_vector)
  V_S = 1/((n-1)*t(one_vector) %*% solve(cov_sample) %*% one_vector)
  V_S_Bayesian = c*(1/(t(one_vector) %*% solve(cov_sample) %*% one_vector))
  
  curve(R_S + sqrt((n-1)*(t(sample_mean) %*% Q %*% sample_mean)*(x - V_S)),from=0,to=0.1, ylab = 'R', xlab = 'V', col='black')
  curve(R_S + sqrt((t(sample_mean) %*% Q %*% sample_mean)/c *(x - V_S)),from=0,to=0.1, col='red', add=TRUE)
  title(main = paste0("k = ",k,", risk aversion = ",risk_coef,", n =",n))
  legend("bottomright", legend=c("Sample Frontier", "Bayesian"), col=c("black", "red"), lty=1, cex=0.3)
}

simulation_function = function(k,risk_coef,n,B,num_rep){
  mu = runif(k,-0.01,0.01)
  
  #low volatility
  D1 = diag(runif(k,0.002,0.005))
  
  #high volatility
  D2 = diag(runif(k,0.005,0.02))
  
  #vector of ones
  one_vector = replicate(k,1)
  
  #correlation matrix
  phi = 0.6
  corr_matrix = (1-phi)*diag(k) + phi * matrix(1,k,k)
  
  #covariance
  cov_1 = D1 %*% corr_matrix %*% D1
  cov_2 = D2 %*% corr_matrix %*% D2
  
  #population values
  #B is population size, X is population values
  X = mvrnorm(B,mu,cov_1)
  cov_pop = (B-1)/B *cov(X)
  population_mean = colMeans(X)
  R = solve(cov_pop) - (solve(cov_pop) %*% one_vector %*% t(one_vector) %*% solve(cov_pop))/as.numeric(t(one_vector) %*% solve(cov_pop) %*% one_vector)
  expected_return_pop = (t(one_vector) %*% solve(cov_pop) %*% population_mean)/(t(one_vector) %*% solve(cov_pop) %*% one_vector) + (t(population_mean) %*% R %*% population_mean)/risk_coef
  
  #collect sample
  
  #abs_dif = absolute difference between expected return of sample and population (averaged over num_rep repetitions)
  abs_dif = rep(NA,num_rep)
  abs_dif_bayesian = rep(NA,num_rep)
  R_S = rep(NA,num_rep)
  V_S = rep(NA,num_rep)
  V_S_Bayesian = rep(NA,num_rep)
  for (i in 1:num_rep){
    sample_data = X[sample(nrow(X),size=n,replace=FALSE),]
    sample_mean = colMeans(sample_data)
    cov_sample = (n-1)*cov(sample_data)
    
    Q = solve(cov_sample) - (solve(cov_sample) %*% one_vector %*% t(one_vector) %*% solve(cov_sample))/as.numeric(t(one_vector) %*% solve(cov_sample) %*% one_vector)
    expected_return_sample = (t(one_vector) %*% solve(cov_sample) %*% sample_mean)/(t(one_vector) %*% solve(cov_sample) %*% one_vector) + (t(sample_mean) %*% Q %*% sample_mean)/(risk_coef *1/(n-1))
    c = 1/(n-k-1) + (2*n-k-1)/(n*(n-k-1)*(n-k-2))
    expected_return_bayesian = (t(one_vector) %*% solve(cov_sample) %*% sample_mean)/(t(one_vector) %*% solve(cov_sample) %*% one_vector) + (t(sample_mean) %*% Q %*% sample_mean)/(risk_coef *c)
    
    abs_dif[i] = abs(expected_return_sample - expected_return_pop)
    abs_dif_bayesian[i] = abs(expected_return_bayesian - expected_return_pop)
    R_S[i] = (t(one_vector) %*% solve(cov_sample) %*% sample_mean)/(t(one_vector) %*% solve(cov_sample) %*% one_vector)
    V_S[i] = 1/((n-1)*t(one_vector) %*% solve(cov_sample) %*% one_vector)
    V_S_Bayesian = c*(1/(t(one_vector) %*% solve(cov_sample) %*% one_vector))
  }
  abs_dif = mean(abs_dif)
  abs_dif_bayesian = mean(abs_dif_bayesian)
  R_S = mean(R_S)
  R_S_Bayesian = R_S
  V_S = mean(V_S)
  V_S_Bayesian = mean(V_S_Bayesian)
  
  #Plot sample estimator of the population efficient frontier
  curve(R_S + sqrt((n-1)*(t(sample_mean) %*% Q %*% sample_mean)*(x - V_S)),from=0,to=0.1, ylab = 'R', xlab = 'V', col=1)
  curve(R_S + sqrt((t(sample_mean) %*% Q %*% sample_mean)/c *(x - V_S)),from=0,to=0.1, col=2, add=TRUE)
  title(main = paste0("Comparison of efficient frontiers, k = ",k))
  return(c(V_S,R_S,V_S_Bayesian,R_S_Bayesian))
}
