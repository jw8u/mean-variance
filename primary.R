#k is number of assets in portfolio
#risk_coef = risk aversion coefficient
#n = size of sample
#B = population size
#num_rep = number of sample repetitions

#functions:
#mv_stock(n,k,risk_coef)
#simulation_function(k,risk_coef,n,B,num_rep)

source("functions.R")

#changing n
par(mfrow = c(2,2))
mv_stock(n=52,k=10,risk_coef=50)
mv_stock(n=78,k=10,risk_coef=50)
mv_stock(n=104,k=10,risk_coef=50)
mv_stock(n=130,k=10,risk_coef=50)

#changing k
mv_stock(n=130,k=5,risk_coef=50)
mv_stock(n=130,k=10,risk_coef=50)
mv_stock(n=130,k=25,risk_coef=50)
mv_stock(n=130,k=40,risk_coef=50)

#changing risk aversion coefficient
mv_stock(n=130,k=10,risk_coef=10)
mv_stock(n=130,k=10,risk_coef=25)
mv_stock(n=130,k=10,risk_coef=50)
mv_stock(n=130,k=10,risk_coef=100)
