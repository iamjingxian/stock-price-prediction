##########nonnegative-lasso method##############
#####get ready
setwd("E:\\5205")
y = read.csv("data4_sgx_etfs.csv")[, c(1, 2, 4)]
es3si = which(y[, 2] == "ES3.SI")
g3bsi = which(y[, 2] == "G3B.SI")
s27 = which(y[, 2] == "S27.SI")
es3 = y[es3si, ]
g3b = y[g3bsi, ]
x = read.csv("data3_sgx_stocks.csv")[, c(1, 2, 4)]
library(ggplot2)

#####Sort data
x = as.matrix(read.csv("yunashi.csv"))[, 2:456]
library(missForest)
z = missForest(x)  
air.full = as.matrix(z$ximp)
#write.csv(air.full, file = "xb.csv", quote = F)
x = as.matrix(read.csv("xb.csv"))[, 2:456]


##############using 2022M1 to 2022M7's stocks' price to start building model
x = as.matrix(read.csv("xb.csv"))[, 2:456]
for(i in 1:(nrow(x) - 1)){
  x[i, ] = x[i + 1, ]/x[i, ] - 1
}
x = x[171:313, ]  #using 2022M1 to 2022M7's stocks' price 

xt = t(x)
A = xt %*% x

#make index into growth rate
y = as.matrix(read.csv("S27.csv", header = F))
for(i in 1:(nrow(y) - 1)){
  y[i, ] = y[i + 1, ]/y[i, ] - 1
}
y = as.matrix(y[171:313, ])  #using 2022M1 to 2022M7's stocks' price 

#algorithm
A1 = A
A2 = A
for(i in 1:nrow(A)){
  for(j in 1:ncol(A)){
    if(A[i, j] < 0){
      A1[i, j] = 0
      A2[i, j] = -A[i, j]
    }else {
      A2[i, j] = 0
    }
  }
}


#Find suitable variable coefficients and lambda by looping
setnum = 10
lambda1 = 0
lambda2 = 10000
pp = 0.015

for(i in 1:5000){
  beta = matrix(3, ncol(x), 1)
  one = matrix(1, ncol(x), 1)
  lambda = (lambda1 + lambda2)/2
  
  for(ii in 1:3){
    a = A1 %*% beta
    b = lambda*one - 2*xt %*% y
    c = A2 %*% beta
    beta = ((-b + sqrt(b*b + 4*a*c))/(2*a))*beta
  }
  
  #choose lambda by dichotomy
  lb = length(beta[beta > pp])
  if(lb > setnum){
    lambda1 = lambda
  }else if(lb < setnum){
    lambda2 = lambda
  }else{
    break
  }
}

#examine which stocks are selected
colnames(x)[which(beta > pp)]


#look at the variable coefficients
betagood = as.matrix(beta[beta > pp])
betagood

#Extract selected stock prices
su = which(beta > pp)
xgood = matrix(1, 143, setnum)
for(i in 1:setnum){
  xgood[, i] = x[, su[i]]
}

#####see how good the model is
#get the prediction
yhat1 = xgood %*% betagood

#residuals
ycha = y - xgood %*% betagood

#####nonnegative-lasso + OLS
#+OLS
betaols = as.matrix((lm(ycha ~ 0 + xgood))$coefficients)

#get the prediction
yhat = xgood %*% betagood + xgood %*% betaols

#####plot to see how it shows
#read the real index
y0 = as.matrix(read.csv("S27.csv", header = F))

#what we predict
y02 = matrix(1, 144, 1)
y02[1, ] = y0[171, ]
for(i in 2:nrow(y02)){
  y02[i, ] = (yhat1[i - 1, ] + 1)*y02[i - 1, ]
}
#y02

#what we predict（+OLS）
y01 = matrix(1, 144, 1)
y01[1, ] = y0[171, ]
for(i in 2:nrow(y01)){
  y01[i, ] = (yhat[i - 1, ] + 1)*y01[i - 1, ]
}
#y01

#####plot
#ready to polt
y01_ = cbind(data.frame(rep("Multiplicative+OLS", 144)), as.data.frame(y01), matrix(1:144, 144, 1))
colnames(y01_) = c("object", "price", "date")
y0_ = cbind(data.frame(rep("real",144)), as.data.frame(y0[171:314, ]), matrix(1:144, 144, 1))
colnames(y0_) = c("object", "price", "date")
y02_ = cbind(data.frame(rep("Multiplicativ", 144)), as.data.frame(y02), matrix(1:144, 144, 1))
colnames(y02_) = c("object", "price", "date")
h4_1 = as.data.frame(rbind(y01_, y0_))
h4_2 = as.data.frame(rbind(y02_, y0_))
h4_3 = as.data.frame(rbind(y02_, y01_, y0_))
#y00 = as.data.frame(cbind(y01, y0[171:314, ]))
#write.csv(y00, file = "realpred1", quote = F)

#plot
p1 = ggplot(h4_1, aes(x = date, y = price, group = object)) +
  geom_line(aes(color = object))+
  geom_point(aes(color = object)) +
  expand_limits(y = 300)
p1


##########using 2022M8 to 2022M9's stocks' price to do the validation
x = as.matrix(read.csv("xb.csv"))[, 2:456]
for(i in 1:(nrow(x) - 1)){
  x[i, ] = x[i + 1, ]/x[i, ] - 1
}
x = x[314:357, ]  #using 2022M8 to 2022M9's stocks' price 

xt = t(x)
A = xt %*% x

#make index into growth rate
y = as.matrix(read.csv("S27.csv", header = F))
for(i in 1:(nrow(y) - 1)){
  y[i, ] = y[i + 1, ]/y[i, ] - 1
}
y = as.matrix(y[314:357, ])  #using 2022M1 to 2022M7's stocks' price 

#algorithm
A1 = A
A2 = A
for(i in 1:nrow(A)){
  for(j in 1:ncol(A)){
    if(A[i, j] < 0){
      A1[i, j] = 0
      A2[i, j] = -A[i, j]
    }else {
      A2[i, j] = 0
    }
  }
}


#Find suitable variable coefficients and lambda by looping
setnum = 10
lambda1 = 0
lambda2 = 10000
pp = 0.015

for(i in 1:5000){
  beta = matrix(3, ncol(x), 1)
  one = matrix(1, ncol(x), 1)
  lambda = (lambda1 + lambda2)/2
  
  for(ii in 1:3){
    a = A1 %*% beta
    b = lambda*one - 2*xt %*% y
    c = A2 %*% beta
    beta = ((-b + sqrt(b*b + 4*a*c))/(2*a))*beta
  }
  
  #choose lambda by dichotomy
  lb = length(beta[beta > pp])
  if(lb > setnum){
    lambda1 = lambda
  }else if(lb < setnum){
    lambda2 = lambda
  }else{
    break
  }
}

#examine which stocks are selected
colnames(x)[which(beta > pp)]


#look at the variable coefficients
betagood = as.matrix(beta[beta > pp])
betagood

#Extract selected stock prices
su = which(beta > pp)
xgood = matrix(1, 44, setnum)
for(i in 1:setnum){
  xgood[, i] = x[, su[i]]
}


#####see how good the model is
#get the prediction
yhat1 = xgood %*% betagood

#residuals
ycha = y - xgood %*% betagood

#####nonnegative-lasso + OLS
betaols = as.matrix((lm(ycha ~ 0 + xgood))$coefficients)

#get the prediction
yhat = xgood %*% betagood + xgood %*% betaols

#####plot to see how it shows
#read the real index
y0 = as.matrix(read.csv("S27.csv",header=F))

#what we predict
y02 = matrix(1, 45, 1)
y02[1, ] = y0[314, ]
for(i in 2:nrow(y02)){
  y02[i, ] = (yhat1[i - 1, ] + 1)*y02[i - 1, ]
}
#y02

#what we predict（+OLS）
y01 = matrix(1, 45, 1)
y01[1, ] = y0[314, ]
for(i in 2:nrow(y01)){
  y01[i, ] = (yhat[i - 1, ] + 1)*y01[i - 1, ]
}
#y01

#####plot
#ready to polt
y01_ = cbind(data.frame(rep("Multiplicative+OLS", 45)), as.data.frame(y01), matrix(1:45, 45, 1))
colnames(y01_) = c("object", "price", "date")
y0_ = cbind(data.frame(rep("real",45)), as.data.frame(y0[314:358, ]), matrix(1:45, 45, 1))
colnames(y0_) = c("object", "price", "date")
y02_ = cbind(data.frame(rep("Multiplicativ", 45)), as.data.frame(y02), matrix(1:45, 45, 1))
colnames(y02_) = c("object", "price", "date")
h4_1 = as.data.frame(rbind(y01_, y0_))
h4_2 = as.data.frame(rbind(y02_, y0_))
h4_3 = as.data.frame(rbind(y02_, y01_, y0_))
#y00 = as.data.frame(cbind(y01, y0[314:358, ]))
#write.csv(y00,file = "realpredg3b", quote = F)

#plot
p2 = ggplot(h4_1, aes(x = date, y = price, group = object)) +
  geom_line(aes(color = object))+
  geom_point(aes(color = object)) +
  expand_limits(y = 300)
p2


