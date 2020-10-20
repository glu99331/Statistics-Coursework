##########################################################
# clear working environment and set working directory
##########################################################
rm(list = ls())

##########################################################
# Install Packages
##########################################################
# install.packages(c("quantmod","fBasics")) # You only need to install the packages once

##########################################################
# Import Required Packages
##########################################################
library("quantmod") # add quantmod to the list of Packages
library("fBasics") # add fBasics to the list of Packages


##########################################################
# Fetch and Analyze data
##########################################################
getSymbols(Symbols ="AMZN",src = "yahoo", from = '2000/01/01/') # Download daily data for Amazon from Yahoo Finance
stock_adj_price <- as.matrix(AMZN[,6]) # or

n_obs = dim(stock_adj_price)[1]

stock_return <- diff(stock_adj_price)/stock_adj_price[1:n_obs-1,1]

stock_log_return = diff(log(stock_adj_price)) # Compute log returns

multi_return1 = prod(1+stock_return) - 1 # Multi period simple return
multi_return2 = (stock_adj_price[n_obs,] - stock_adj_price[1,])/stock_adj_price[1,]

multi_conpounded_return_1 = log(1+multi_return1) # Multi Period Compounded return
multi_conpounded_return_2 = sum(log(1+stock_return))
  
data_summary = basicStats(stock_return) 
simple_mean_return = data_summary["Mean",] # arithmetic mean of return
geo_mean_return = prod(1+stock_return)^(1/n_obs)-1 # geometric mean of return
geo_mean_return - simple_mean_return # the difference between geometric and arithmetic mean

mean(stock_return) # compute simple mean
var(stock_return) # compute variance
stdev(stock_return) # compute standard deviation

t.test(stock_return) # t test for the null hypothesis that the mean is zero

s3=skewness(stock_return) # compute skewness
t3=s3/sqrt(6/n_obs) # t test for the null hypothesis that the skewness is zero
pp3=2*(1-pnorm(t3)) # Compute p-value
c(t3, pp3)

s4 = kurtosis(stock_return) # compute excess kurtosis (kurtosis - 3)
t4=s4/sqrt(24/n_obs) # t test for the null hypothesis that the kurtosis is three
pp4=2*(1-pnorm(t4)) # Compute p-value
c(t4, pp4)

normalTest(stock_return,method='jb') # JB-test

hist(stock_return,nclass=30) # Histogram
d1 = density(stock_return) # Obtain density estimate
range(stock_return) # Range of returns

x=seq(-.1,.1,.001) # Create a sequence of x with increment 0.001.

y1=dnorm(x,mean(stock_return),stdev(stock_return)) # This command creates normal density

plot(d1$x,d1$y,xlab='rtn',ylab='densit',type='l',xlim=c(-.1,.1))
lines(x,y1,lty=2)

getSymbols(Symbols ="^GSPC",src = "yahoo",from = "2000/01/01") # Download daily data for S&P 500 from Yahoo Finance
sp_adj_price = as.matrix(GSPC[,6])
sp_return <- diff(sp_adj_price)/sp_adj_price[1:n_obs-1,1]

cor(stock_return,sp_return) # Obtain sample correlation

m1=lm(stock_return ~ sp_return) # run a linear regression of stock return on market return with intercept 
summary(m1)

plot(sp_return,stock_return,cex=0.8) # Obtain scatter plot
abline(0.0009215,1.2241395) # Add the linear regression line

