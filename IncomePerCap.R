# Your project should use this data to create good models for median household income and 
# income per capita in NYC and the surrounding area and comment on the di erences and similarities between the two models.
library(leaps)
library(glmnet)
library(pls)
library(tidyr)
library(ggplot2)
census = read.csv('nyc_census.csv')
dim(census) # 2167 36
# remove na
census  = na.omit(census)
summary(census)
census.full =  census

# drop censustract
census = census[2:36] 
dim(census)  # 2095 35

# is factor ? county/borough
county = census[,1]
is.factor(county)
table(county)
borough = census[,2]
is.factor(borough)
table(borough)

# remove borough
census = census[,!(colnames(census) %in% c('Borough'))]

# linear dependency:sex / race / job types / commute types / employment types
# drop the one column with the largest mean value within each group
# sex: Women / race:White / job types: Professional /commute types:Transit / employment types: PrivateWork
data = census[, !(colnames(census) %in% c('Women','White','Professional','Transit','PrivateWork'))]
dim(data)  # 2095   29


# INCOME and INCOME PER CAP have right-skewed distributions, consider log transformation
par(mfrow=c(1,2))
hist(census$Income)
hist(log(census$Income))

hist(census$IncomePerCap)
hist(log(census$IncomePerCap))


# split train and test set: 20% test, 80% train
set.seed(1)
samp.size = floor(0.8*nrow(data))
train.ind = sample(seq(nrow(data)), size = samp.size)
census.train = data[train.ind,]
census.test = data[-train.ind,]


# Run best subset selection
regfit.full = regsubsets(IncomePerCap~.,data=census.train, nvmax = 28)
reg.summary=summary(regfit.full)
names(reg.summary)

par(mfrow=c(2,2))
plot(reg.summary$rss, xlab = "num variables.",ylab = "Residual Sum of Square",type="l")

plot(reg.summary$adjr2, xlab = "num variables.",ylab = "Adj R2",type="l")
maxpoint<-which.max(reg.summary$adjr2) 
points(maxpoint,reg.summary$adjr2[maxpoint], col = "red",cex=1, pch = 10) 

plot(reg.summary$cp ,xlab="Number of Variables ",ylab="Cp", type='l')
minpoint2 <- which.min(reg.summary$cp ) 
points(minpoint2,reg.summary$cp [minpoint2],col="red",cex=1,pch=10)

plot(reg.summary$bic ,xlab="Number of Variables ",ylab="BIC",type='l')
minpoint3 <- which.min(reg.summary$bic ) 
points(minpoint3,reg.summary$bic[minpoint3],col="red",cex=1,pch=10)


plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")


# cross-validation approach -- best subset
set.seed(1)
k = 10
fold <- sample(1:k,nrow(census.train), replace = TRUE)
errors <- matrix(NA,k,28,dimnames = list(NULL,paste(1:28)))
for (i in 1:k){
  fit.best <- regsubsets(IncomePerCap ~.,data = census.train[fold!=i,],nvmax = 28)
  test.mat<-model.matrix(IncomePerCap~.,census.train[fold==i,])
  for (j in 1:28){
    coefi <- coef(fit.best,j)
    pred <- test.mat[,names(coefi)]%*%coefi
    errors[i,j] = mean((census.train$IncomePerCap[fold==i] - pred)^2)
  }
}
error.mean = apply(errors,2,mean)
error.mean
which.min(error.mean)  #28
min(error.mean)  #36384461
par(mfrow=c(1,1))
plot(error.mean,type = 'b')
coef(regfit.full,28)




# run forward selection:
set.seed(1)
regfit.fwd = regsubsets(IncomePerCap ~.,data = census.train, nvmax = 28, method = 'forward')
reg.summary = summary(regfit.fwd)

par(mfrow=c(2,2))
plot(reg.summary$rss, xlab = "num variables.",ylab = "Residual Sum of Square",type="l")
plot(reg.summary$adjr2, xlab = "num variables.",ylab = "Adj R2",type="l")
maxpoint<-which.max(reg.summary$adjr2) 
points(maxpoint,reg.summary$adjr2[maxpoint], col = "red",cex=1, pch = 10) 

plot(reg.summary$cp ,xlab="Number of Variables ",ylab="Cp", type='l')
minpoint2 <- which.min(reg.summary$cp ) 
points(minpoint2,reg.summary$cp [minpoint2],col="red",cex=1,pch=10)

plot(reg.summary$bic ,xlab="Number of Variables ",ylab="BIC",type='l')
minpoint3 <- which.min(reg.summary$bic ) 
points(minpoint3,reg.summary$bic[minpoint3],col="red",cex=1,pch=10)

par(mfrow=c(1,2))
plot(regfit.fwd,scale="adjr2")
plot(regfit.fwd,scale="bic")

# cross validation approach -- forward
set.seed(1)
k = 10
fold <- sample(1:k,nrow(census.train), replace = TRUE)
errors <- matrix(NA,k,28,dimnames = list(NULL,paste(1:28)))
for (i in 1:k){
  fit.best <- regsubsets(IncomePerCap ~.,data = census.train[fold!=i,],nvmax = 28,method = 'forward')
  test.mat<-model.matrix(IncomePerCap~.,census.train[fold==i,])
  for (j in 1:28){
    coefi <- coef(fit.best,j)
    pred <- test.mat[,names(coefi)]%*%coefi
    errors[i,j] = mean((census.train$IncomePerCap[fold==i] - pred)^2)
  }
}
error.mean = apply(errors,2,mean)
error.mean
which.min(error.mean) # 28
min(error.mean)  #36384461
par(mfrow=c(1,1))
plot(error.mean,type = 'b')
coef(regfit.fwd,28)


# run backward selection:
regfit.bwd = regsubsets(IncomePerCap ~.,data = census.train, nvmax = 28, method = 'backward')
reg.summary = summary(regfit.bwd)
par(mfrow=c(2,2))
plot(reg.summary$rss, xlab = "num variables.",ylab = "Residual Sum of Square",type="l")
plot(reg.summary$adjr2, xlab = "num variables.",ylab = "Adj R2",type="l")
maxpoint<-which.max(reg.summary$adjr2) 
points(maxpoint,reg.summary$adjr2[maxpoint], col = "red",cex=1, pch = 10) 

plot(reg.summary$cp ,xlab="Number of Variables ",ylab="Cp", type='l')
minpoint2 <- which.min(reg.summary$cp ) 
points(minpoint2,reg.summary$cp [minpoint2],col="red",cex=1,pch=10)

plot(reg.summary$bic ,xlab="Number of Variables ",ylab="BIC",type='l')
minpoint3 <- which.min(reg.summary$bic ) 
points(minpoint3,reg.summary$bic[minpoint3],col="red",cex=1,pch=10)

par(mfrow=c(1,2))
plot(regfit.fwd,scale="adjr2")
plot(regfit.fwd,scale="bic")


# cross validation approach -- forward
set.seed(1)
k = 10
fold <- sample(1:k,nrow(census.train), replace = TRUE)
errors <- matrix(NA,k,28,dimnames = list(NULL,paste(1:28)))
for (i in 1:k){
  fit.best <- regsubsets(IncomePerCap ~.,data = census.train[fold!=i,],nvmax = 28,method = 'backward')
  test.mat<-model.matrix(IncomePerCap~.,census.train[fold==i,])
  for (j in 1:28){
    coefi <- coef(fit.best,j)
    pred <- test.mat[,names(coefi)]%*%coefi
    errors[i,j] = mean((census.train$IncomePerCap[fold==i] - pred)^2)
  }
}
error.mean = apply(errors,2,mean)
error.mean
which.min(error.mean) # 28
min(error.mean)  # 36384461
par(mfrow=c(1,1))
plot(error.mean,type = 'b')
# refit
coef(regfit.bwd,28)


#lasso
trainx<-as.matrix(census.train[,c(1:10,12:29)])
trainy<-as.matrix(census.train[,11])
testx<-as.matrix(census.test[,c(1:10,12:29)])
testy<-as.matrix(census.test[,11])
fullx<-as.matrix(data[,c(1:10,12:29)])
fully<-as.matrix(data[,11])

grid=10^seq(10,-2,length=100)    #Grid of lambda values
trainx=model.matrix(IncomePerCap~.,census.train)[,-1]
trainy=census.train$IncomePerCap
testx=model.matrix(IncomePerCap~.,census.test)[,-1]
testy=census.test$IncomePerCap

fullx=model.matrix(IncomePerCap~.,data)[,-1]
fully=data$IncomePerCap

set.seed(1)
fit.lasso <- glmnet(trainx,trainy,alpha = 1,lambda=grid)
plot(fit.lasso)
cv.lasso = cv.glmnet(trainx,trainy,alpha = 1)
plot(cv.lasso)
bestlam = cv.lasso$lambda.min
bestlam   #51.76935
pred.lasso = predict(fit.lasso, s = bestlam,newx = testx )
mean((pred.lasso-testy)^2)  #34123207

#full model
final.lasso = glmnet(fullx,fully,alpha = 1,lambda = grid)
coef.lasso = predict(final.lasso,type = "coefficients", s = bestlam)
coef.lasso
sum(coef.lasso == 0 )

# plot of income vs. every predictor using training set
data$County = as.numeric(data$County)
data %>%
  gather(County, TotalPop, Men, Hispanic, Black, Native, Asian, Citizen, Income, IncomeErr,
         IncomePerCapErr, Poverty, ChildPoverty, Service, Office, Construction,
         Production, Drive, Carpool, Walk, OtherTransp, MeanCommute, Employed, PublicWork, FamilyWork, 
         -IncomePerCap, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = IncomePerCap)) +
  geom_point(colour = "blue", alpha = 0.2, size=0.1) +
  facet_wrap(~ var, scales = "free") +
  theme_bw()

# histogram plot of every predictor using full data set
data %>%
  gather(County, TotalPop, Men, Hispanic, Black, Native, Asian, Citizen, Income, IncomeErr,
         IncomePerCapErr, Poverty, ChildPoverty, Service, Office, Construction,
         Production, Drive, Carpool, Walk, OtherTransp, MeanCommute, Employed, PublicWork, FamilyWork, 
         -IncomePerCap, key = "var", value = "value") %>%
  ggplot(aes(value)) +
  geom_histogram() +
  facet_wrap(~ var, scales = "free") +
  theme_bw()

lm.1 = lm(IncomePerCap ~ County + TotalPop + Men + Hispanic + Black + Native + Asian + Citizen + 
            Income + IncomeErr + IncomePerCapErr + Poverty + ChildPoverty + 
            Service + Office + Construction + Production + Drive + Carpool + Walk + OtherTransp +
            MeanCommute + Employed + PublicWork + FamilyWork, census.train)
summary(lm.1)

lm.2 = lm(IncomePerCap ~ County + Men + Hispanic + Black + Native + Asian + Citizen + 
            Income + IncomeErr + IncomePerCapErr + Poverty + ChildPoverty + 
            Office + Drive + Carpool + OtherTransp +
            Employed, census.train)
summary(lm.2)

lm.3 = lm(log(IncomePerCap) ~ County + Men + Hispanic + Black + Native + Asian + Citizen + 
            Income + IncomeErr + IncomePerCapErr + Poverty + ChildPoverty + 
            Office + Drive + Carpool + OtherTransp +
            Employed, census.train)
summary(lm.3)

# log some of the variables that are right skewed not involve zero
lm.4 = lm(log(IncomePerCap) ~ County + log(TotalPop) + Men + Hispanic + Black + Native + Asian + log(Citizen) + 
            log(Income) + log(IncomeErr) + log(IncomePerCapErr) + Poverty + ChildPoverty + 
            Service + Office + Construction + Production + Drive + Carpool + Walk + OtherTransp +
            MeanCommute + Employed + PublicWork + FamilyWork, census.train)
summary(lm.4)

# sqrt some of the variables that are right skewed
lm.5 = lm(log(IncomePerCap) ~ County + log(TotalPop) + log(Men) + sqrt(Hispanic) + sqrt(Black) + sqrt(Native) + Asian + log(Citizen) + 
            log(Income) + log(IncomeErr) + log(IncomePerCapErr) + log(Poverty) + sqrt(ChildPoverty) + 
            Service + Office + sqrt(Construction) + Production + sqrt(Drive) + sqrt(Carpool) + sqrt(Walk) + sqrt(OtherTransp) +
            MeanCommute + sqrt(Employed) + PublicWork + FamilyWork, census.train)
summary(lm.5)

# Add interatcion term Black Men and Office Drive
lm.6 = lm(log(IncomePerCap) ~ County + log(TotalPop) + Men + Hispanic + Black + Native + Asian + log(Citizen) + 
            log(Income) + log(IncomeErr) + log(IncomePerCapErr) + Poverty + ChildPoverty + 
            Service + Office + Construction + Production + Drive + Carpool + Walk + OtherTransp +
            MeanCommute + Employed + PublicWork + FamilyWork + Men*Black + Office*Drive, census.train)
summary(lm.6)

# Now use the test set to compare the different models
sqrt(mean((census.test$IncomePerCap - predict.lm(lm.1, census.test)) ^ 2))
sqrt(mean((census.test$IncomePerCap - predict.lm(lm.2, census.test)) ^ 2))
sqrt(mean((census.test$IncomePerCap - exp(predict.lm(lm.3, census.test))) ^ 2))
sqrt(mean((census.test$IncomePerCap - exp(predict.lm(lm.4, census.test))) ^ 2))
sqrt(mean((census.test$IncomePerCap - exp(predict.lm(lm.5, census.test))) ^ 2)) #getting inf error
sqrt(mean((census.test$IncomePerCap - exp(predict.lm(lm.6, census.test))) ^ 2)) #best


cor(data$Men,data$Black)
cor(data$Office,data$Drive)
par(mfrow=c(2,2))
plot(lm.6)



