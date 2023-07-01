############estimate the risk#############
#####sort data
price = read.csv("E:/5205/lasso_.csv", header = T)
lassoprice = price[, 2]
lstmprice = price[, 3]


#####distribution
#normal Q-Q plot
qqnorm(lassoprice, col = 4, main = "Normal Q-Q Plot Price")
qqline(lassoprice, lwd = 2)

#skewed-t Q-Q plot
library("fGarch")
fit = sstdFit(lassoprice, hessian = T)
fit
par(mfrow = c(1, 1))
para = fit$estimate
xgrid = seq(0, max(lassoprice) + 5,length.out = 100)
plot(density(lassoprice), main = "Male Earnings", ylim = c(0, 0.1), col = 4, lwd = 2)
lines(xgrid, dsstd(xgrid, mean = para[1], sd = para[2],  nu = para[3], xi = para[4]), col = 6, type = "l", lty = 5, lwd = 2)
legend("topright", c("KDE", "Skewed-t"), lty = c(1, 5), col = c(4, 6))

par(mfrow=c(1, 1))
n = length(lassoprice)
q.grid = (1:n) / (n+1)
qqplot(lassoprice, qsstd(q.grid, mean = para[1], sd = para[2], nu = para[3],xi=para[4]), col = 4, xlab = "Sample Quantiles", ylab = "Theoretical Quantiles", main = "Skewed-t Q-Q Plot Price" )
abline(a = 0, b = 1, lwd = 2)

#sort data
relasso = diff(lassoprice)/lag(lassoprice)[-1]
relstm = diff(lstmprice)/lag(lstmprice)[-1]
rerfc = price[, 4][1:44]
y01_ = cbind(data.frame(rep("lasso", 44)), as.data.frame(relasso), matrix(1:44, 44, 1))
colnames(y01_) = c("object", "return_ratio", "time")
y0_ = cbind(data.frame(rep("lstm",44)), as.data.frame(relstm), matrix(1:44, 44, 1))
colnames(y0_) = c("object", "return_ratio", "time")
y02_ = cbind(data.frame(rep("rfc", 44)), as.data.frame(rerfc), matrix(1:44, 44, 1))
colnames(y02_) = c("object", "return_ratio", "time")
h4_3 = as.data.frame(rbind(y02_, y01_, y0_))

#plot
p = ggplot(h4_3, aes(x = time, y = return_ratio, group = object)) +
  geom_line(aes(color = object))+
  geom_point(aes(color = object)) 
p


#####ES and VaR
#normal distribution
S = 5000
alpha = 0.05
mulasso = mean(relasso)
sdlasso = sd(relasso)
Finvlasso = qnorm(alpha, mean = mulasso, sd = sdlasso)
denlasso = dnorm(qnorm(alpha))
VaRlasso = -S * Finvlasso
VaRlasso
ESlasso = S * (-mulasso + sdlasso*denlasso/alpha)
ESlasso

mulstm = mean(relstm)
sdlstm = sd(relstm)
Finvlstm = qnorm(alpha, mean = mulstm, sd = sdlstm)
denlstm = dnorm(qnorm(alpha))
VaRlstm = -S * Finvlstm
VaRlstm
ESlstm = S * (-mulstm + sdlstm*denlstm/alpha)
ESlstm

murfc = mean(rerfc)
sdrfc = sd(rerfc)
Finvrfc = qnorm(alpha, mean = murfc,sd = sdrfc)
denrfc = dnorm(qnorm(alpha))
VaRrfc = -S * Finvrfc
VaRrfc
ESrfc = S * (-murfc + sdrfc*denrfc/alpha)
ESrfc

##### skewed t-distribution
library(MASS)

reslasso = fitdistr(relasso,'t')
mulasso = reslasso$estimate['m']
lambdalasso = reslasso$estimate['s']
nulasso = reslasso$estimate['df']
qt(alpha, df = nulasso)
dt(qt(alpha, df = nulasso), df = nulasso)
Finvlasso = mulasso + lambdalasso * qt(alpha, df = nulasso)
VaRlasso = -S * Finvlasso
options(digits = 4)
VaRlasso
denlasso = dt(qt(alpha, df = nulasso), df = nulasso)
ESlasso = S * (-mulasso + lambdalasso*(denlasso/alpha) * (nulasso + qt(alpha, df = nulasso)^2)/(nulasso - 1))
ESlasso

reslstm = fitdistr(relstm,'t')
mulstm = reslstm$estimate['m']
lambdalstm = reslstm$estimate['s']
nulstm = reslstm$estimate['df']
qt(alpha, df = nulstm)
dt(qt(alpha, df = nulstm), df = nulstm)
Finvlstm = mulstm + lambdalstm * qt(alpha, df = nulstm)
VaRlstm = -S * Finvlstm
options(digits = 4)
VaRlstm
denlstm = dt(qt(alpha, df = nulstm), df = nulstm)
ESlstm = S * (-mulstm + lambdalstm*(denlstm/alpha) * (nulstm + qt(alpha, df = nulstm)^2)/(nulstm - 1))
ESlstm

resrfc = fitdistr(rerfc,'t')
murfc = resrfc$estimate['m']
lambdarfc = resrfc$estimate['s']
nurfc = resrfc$estimate['df']
qt(alpha, df = nurfc)
dt(qt(alpha, df = nurfc), df = nurfc)
Finvrfc = murfc + lambdarfc * qt(alpha, df = nurfc)
VaRrfc = -S * Finvrfc
options(digits = 4)
VaRrfc
denrfc = dt(qt(alpha, df = nurfc), df = nurfc)
ESrfc = S * (-murfc + lambdarfc*(denrfc/alpha)*(nurfc + qt(alpha, df = nurfc)^2)/(nurfc - 1))
ESrfc

