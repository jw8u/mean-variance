library(MASS)
library(quantmod)
library(BatchGetSymbols)

mv_stock = function(n,k,risk_coef){
  #vector of ones
  one_vector = rep(1,k)
  
  #get stock data
  sp500_sym = getSymbols(sample(GetSP500Stocks()$Tickers,k))
  weekly_returns = lapply(sp500_sym, function(x) {weeklyReturn(get(x))})
  sample_data = as.data.frame(do.call(cbind, weekly_returns))
  sample_data = tail(sample_data,n)
  
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
