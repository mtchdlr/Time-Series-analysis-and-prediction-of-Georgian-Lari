

library(tidyverse)
library(readr)
library(Metrics)
library(aTSA)
library(astsa)
library(readxl)
library(quantmod)
library(fGarch)

#import data
data = read_excel("C:/Users/PC/Desktop/AR.xlsx")
data$t = seq(from=1, to=dim(data)[1], by=1)

#90% for train, 20% for testing
train_size = round(dim(data)[1]*0.9)
train_set = data[1:train_size,]
test_set = data[(train_size+1):dim(data)[1],]

acf(data$ex)

#estimate the model 
for (i in 1:20){
  #estimate model
  model_estimate = arima(train_set$ex, order = c(i, 0, 0), method = "ML")
  #make a prediction
  prediction = predict(model_estimate, 10)
  #RMSE
  print(paste(i, "...", rmse(prediction$pred, test_set$ex)))
}


#SARIMA

for (p in 1:10){
  for (q in 1:2){
    for (P in 1:3){
      for (D in 1:3){
        for(Q in 1:3){
          model_sarima = sarima.for(train_set$ex, n.ahead = dim(test_set)[1], p = p, d=0, q = q, P = P, D = D, Q = Q, S = 12, plot.all = FALSE)
          print(paste("........", rmse(model_sarima$pred, test_set$ex)))
        }
      }
    }
  }
}

model = sarima(train_set$ex, p = 1, d = 1, q = 1, P = 1, D = 2, Q = 2, S = 12)


#ARMA-GARCH

pacf(data$ex)

model_ar = arma(train_set$ex, order = c(1,0))
model_residuals = model_ar$residuals
model_residuals = model_residuals[2:length(model_residuals)]
acf(model_residuals)
#Thus the ARMA order based on the PACF and ACF is (1, 0)

#define the specification of GARCH(1,1) model without drift
specification = ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(1,1)), mean.model=list(armaOrder=c(1,0), include.mean=FALSE), distribution.model="norm")
#now fit the model with above RiskMetrics specification
garch_fit = ugarchfit(specification, data = train_set$ex)
garch_prediction = ugarchforecast(garch_fit, n.ahead = 672)
garch_prediction

model_garch = garchFit(~arma(1,0) + garch(1, 1), data = train_set$ex, trace = FALSE)
model_resid = residuals(model_garch)

sd(model_resid)

model_resid_standardize = residuals(model_garch, standardize = T)

Box.test(model_resid_standardize, type = "Ljung-Box")
