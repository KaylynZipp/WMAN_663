
#Quant Project 

#Loading Packages
library(multcomp)

#Checking Over My Data
data<-smb_gen_data
data<-data.na.omit<-data[-c(43,44,45,46), ]
head(smb_gen_data)
str(data)

#Checking Normality
shapiro.test(smb_gen_data$total_plast)
shapiro.test(smb_gen_data$X500)
shapiro.test(smb_gen_data$X300)
shapiro.test(smb_gen_data$X212)
shapiro.test(smb_gen_data$X106)
shapiro.test(smb_gen_data$X20)

#General Understanding of the Population based on location

#*******Sex~Pool***********#

sex.pool<-glm(sex~pool, family = binomial, data = data)
summary(sex.pool)
x <- matrix(c(0, -1, 1), nrow = 1)
cntr <- glht(model = sex.pool, linfct = x)
summary(cntr)

#*******Weight~Pool***********#
weight.pool<-glm(weight~pool,family = Gamma, data = data)
summary(weight.pool)
x <- matrix(c(0, -1, 1), nrow = 1)
cntr <- glht(model = weight.pool, linfct = x)
summary(cntr)

#*******Length~Pool***********#
length.pool<-glm(length~pool,family = Gamma, data = data)
summary(length.pool)
x <- matrix(c(0, -1, 1), nrow = 1)
cntr <- glht(model = weight.pool, linfct = x)
summary(cntr)



# Are there differences between sites in counts of microplastic in Smallmouth Bass?

#**Total Count of Suspected Microplastic
fit.totalplast.pool<-glm(total_plast ~ pool, family = poisson, data =data)
summary(fit.totalplast.pool)
x <- matrix(c(0, -1, 1), nrow = 1)
cntr <- glht(model = fit.totalplast.pool, linfct = x)
summary(cntr)

pool<-factor(c('op', 'mn', 'pm'))

nd <- data.frame(
  pool = pool
)
prd <- predict.glm(object = fit.totalplast.pool, newdata = nd, type = 'link',
                   
                   se.fit = T)

low <- exp(prd$fit - qnorm(0.975) * prd$se.fit)
high <- exp(prd$fit + qnorm(0.975) * prd$se.fit)

plot(y = exp(prd$fit), x = nd$pool, xlab = 'Pool',
     ylab = 'Expected Count of Total Suspected Microplastics', cex.axis = 1.5, cex.lab = 1.5,
     ylim = c(min(low), max(high)), type = 'l')

points(x = nd$pool, y = low, lty = 2)
points(x = nd$pool, y = high, lty = 2)

#**500

fit.500.pool<-glm(X500 ~ pool, family = poisson, data =data)
summary(fit.500.pool)
x <- matrix(c(0, -1, 1), nrow = 1)
cntr <- glht(model = fit.500.pool, linfct = x)
summary(cntr)

exp(-0.41988)

pool<-factor(c('op', 'mn', 'pm'))

nd <- data.frame(
  pool = pool
)
prd <- predict.glm(object = fit.500.pool, newdata = nd, type = 'link',
                   
                   se.fit = T)

low <- exp(prd$fit - qnorm(0.975) * prd$se.fit)
high <- exp(prd$fit + qnorm(0.975) * prd$se.fit)

plot(y = exp(prd$fit), x = nd$pool, xlab = 'Pool',
     ylab = 'Expected count of microplastics greater than 500μm', cex.axis = 1, cex.lab = 1,
     ylim = c(min(low), max(high)), type = 'l')

points(x = nd$pool, y = low, lty = 2)
points(x = nd$pool, y = high, lty = 2)

#**300
fit.300.pool<-glm(X300 ~ pool, family = poisson, data =data)
summary(fit.300.pool)
exp(0.46349)
x <- matrix(c(0, -1, 1), nrow = 1)
cntr <- glht(model = fit.300.pool, linfct = x)
summary(cntr)
exp(0.2860)

nd <- data.frame(
  pool = pool
)
prd <- predict.glm(object = fit.300.pool, newdata = nd, type = 'link',
                   
                   se.fit = T)

low <- exp(prd$fit - qnorm(0.975) * prd$se.fit)
high <- exp(prd$fit + qnorm(0.975) * prd$se.fit)

plot(y = exp(prd$fit), x = nd$pool, xlab = 'Pool',
     ylab = 'Expected count of microplastics greater than 300μm but less than 500μm', cex.axis = 1, cex.lab = .6,
     ylim = c(min(low), max(high)), type = 'l')

points(x = nd$pool, y = low, lty = 2)
points(x = nd$pool, y = high, lty = 2)

#**212
fit.212.pool<-glm(X212 ~ pool, family = poisson, data =data)
summary(fit.212.pool)
exp(0.24130)
x <- matrix(c(0, -1, 1), nrow = 1)
cntr <- glht(model = fit.212.pool, linfct = x)
summary(cntr)

nd <- data.frame(
  pool = pool
)
prd <- predict.glm(object = fit.212.pool, newdata = nd, type = 'link',
                   
                   se.fit = T)

low <- exp(prd$fit - qnorm(0.975) * prd$se.fit)
high <- exp(prd$fit + qnorm(0.975) * prd$se.fit)

plot(y = exp(prd$fit), x = nd$pool, xlab = 'Pool',
     ylab = 'Expected count of microplastics greater than 106μm but less than 212μm', cex.axis = 1, cex.lab = .6,
     ylim = c(min(low), max(high)), type = 'l')

points(x = nd$pool, y = low, lty = 2)
points(x = nd$pool, y = high, lty = 2)

#**106
fit.106.pool<-glm(X106 ~ pool, family = poisson, data =data)
summary(fit.106.pool)
x <- matrix(c(0, -1, 1), nrow = 1)
cntr <- glht(model = fit.106.pool, linfct = x)
summary(cntr)
exp(0.2679)


nd <- data.frame(
  pool = pool
)
prd <- predict.glm(object = fit.106.pool, newdata = nd, type = 'link',
                   
                   se.fit = T)

low <- exp(prd$fit - qnorm(0.975) * prd$se.fit)
high <- exp(prd$fit + qnorm(0.975) * prd$se.fit)

plot(y = exp(prd$fit), x = nd$pool, xlab = 'Pool',
     ylab = 'Expected count of microplastics greater than 106μm but less than 212μm', cex.axis = 1, cex.lab = .6,
     ylim = c(min(low), max(high)), type = 'l')

points(x = nd$pool, y = low, lty = 2)
points(x = nd$pool, y = high, lty = 2)

#**20
fit.20.pool<-glm(X20 ~ pool, family = poisson, data =data)
summary(fit.20.pool)
exp(-0.58060)
x <- matrix(c(0, -1, 1), nrow = 1)
cntr <- glht(model = fit.20.pool, linfct = x)
summary(cntr)
exp(0.4638)

nd <- data.frame(
  pool = pool
)
prd <- predict.glm(object = fit.106.pool, newdata = nd, type = 'link',
                   
                   se.fit = T)

low <- exp(prd$fit - qnorm(0.975) * prd$se.fit)
high <- exp(prd$fit + qnorm(0.975) * prd$se.fit)

plot(y = exp(prd$fit), x = nd$pool, xlab = 'Pool',
     ylab = 'Expected count of suspected microplastics greater than 106μm but less than 212μm', cex.axis = 1, cex.lab = .6,
     ylim = c(min(low), max(high)), type = 'l')

points(x = nd$pool, y = low, lty = 2)
points(x = nd$pool, y = high, lty = 2)


#Are there significant relationships between 
#the accumulation of total microplastics based on sex, length, weight? 

data.na.omit<-data[-c(12), ]
fit.totalplast.sex<-glm(total_plast ~ sex, family = poisson, data =data.na.omit)
summary(fit.totalplast.sex)
exp (-0.34394)
sex<-factor(c('f', 'm'))

nd <- data.frame(
  sex = sex
)
prd <- predict.glm(object = fit.totalplast.sex, newdata = nd, type = 'link',
                   
                   se.fit = T)

low <- exp(prd$fit - qnorm(0.975) * prd$se.fit)
high <- exp(prd$fit + qnorm(0.975) * prd$se.fit)

plot(y = exp(prd$fit), x = nd$sex, xlab = 'Sex',
     ylab = 'Expected count of total suspected microplastics', cex.axis = 1, cex.lab = .6,
     ylim = c(min(low), max(high)), type = 'l')

points(x = nd$sex, y = low, lty = 2)
points(x = nd$sex, y = high, lty = 2)

#Length & total suspected plastics

fit.totalplast.length<-glm(total_plast ~ length, family = poisson, data =data)
summary(fit.totalplast.length)

#Weight & total suspected plastics

#Removing random blank columns
data<-data[-c(43:46), ]
fit.totalplast.weight<-glm(total_plast ~ weight, family = poisson, data =data)
summary(fit.totalplast.weight)

min<-min(data$weight)
max<-max(data$weight)
weight<-seq(from = min, to = max, 100 )

nd <- data.frame(
  weight = weight
)
prd <- predict.glm(object = fit.totalplast.weight, newdata = nd, type = 'link',
                   
                   se.fit = T)

low <- exp(prd$fit - qnorm(0.975) * prd$se.fit)
high <- exp(prd$fit + qnorm(0.975) * prd$se.fit)

plot(y = exp(prd$fit), x = nd$weight, xlab = 'Weight',
     ylab = 'Expected count of total suspected microplastics', cex.axis = 1, cex.lab = .6,
     ylim = c(min(low), max(high)), type = 'l')

lines(x = nd$weight, y = low, lty = 2)
lines(x = nd$weight, y = high, lty = 2)

# Weight * Length and total suspected plastics

fit.totalplast.weight.length<-glm(total_plast ~ weight*length, family = poisson, data =data)
summary(fit.totalplast.weight.length)

b<-coef(fit.totalplast.weight.length)

for(i in 1:100){
  for(j in 1:100){
    y[i, j] <- b[1] + b[2] * Weight[i] + b[3] * Length[j] +b[4]*Length[j]*Weight[i]
  }
}
?persp
persp(x = Weight, y = Length, z = y, theta =  -35, cex.lab = .6, zlab = "Total Suspected Microplastics")






