(Visualization)
# Read in Data
nyc <- read.csv ("nyc_census.csv", header=TRUE)
loc <- read.csv ("census_tract_loc.csv", header=TRUE)
summary(nyc)
options(scipen = 999)
loc$BlockCode <- substr(loc$BlockCode, 0, 11)

# Remove NA values
nyc_census <- na.omit(nyc)
summary(nyc_census)
merged <- merge(nyc_census, loc, by.x=("CensusTract"), by.y=("BlockCode"),all.x = TRUE)
summary(merged)
library(ggplot2)
library(ggmap)
nyc_base <- ggmap::get_map("New York City", zoom = 10)
ggmap(nyc_base)
ggmap(nyc_base) + geom_point(aes(x=Longitude, y=Latitude, 
                                 colour=Income), data=merged) + 
  scale_colour_gradient(low = "pink", high = "red")

ggmap(nyc_base) + geom_point(aes(x=Longitude, y=Latitude, 
                                 colour=IncomePerCap), data=merged) + 
  scale_colour_gradient(low = "yellow", high = "violet")

boxplot(Income~County,data = nyc_census,
        main="Boxplot of Income in Different Counties",
        xlab="County",ylab="Income")

par(mfrow = c(2,1))
boxplot(nyc_census$Men~nyc_census$County)
boxplot(nyc_census$Women~nyc_census$County)

par(mfrow = c(2,1))
boxplot(nyc_census$Hispanic~nyc_census$County,ylab="Hispanic")
boxplot(nyc_census$White~nyc_census$County,ylab="White")
boxplot(nyc_census$Black~nyc_census$County,ylab="Black")
boxplot(nyc_census$Asian~nyc_census$County,ylab="Asian")
boxplot(nyc_census$Native~nyc_census$County,ylab="Native")

par(mfrow = c(2,1))
boxplot(nyc_census$Income~nyc_census$County,ylab="Income")
boxplot(nyc_census$Poverty~nyc_census$County,ylab="Poverty")

par(mfrow = c(1,1))
boxplot(nyc_census$Professional~nyc_census$County,ylab="Professional")
boxplot(nyc_census$Service~nyc_census$County,ylab="Service")
boxplot(nyc_census$Office~nyc_census$County,ylab="Office")
boxplot(nyc_census$Construction~nyc_census$County,ylab="Construction")
boxplot(nyc_census$Production~nyc_census$County,ylab="Production")

par(mfrow = c(2,1))
boxplot(nyc_census$Drive~nyc_census$County,ylab="Drive")
boxplot(nyc_census$Transit~nyc_census$County,ylab="Transit")
boxplot(nyc_census$Carpool~nyc_census$County,ylab="Carpool")
boxplot(nyc_census$Walk~nyc_census$County,ylab="Walk")

# Model for predicting household median income 
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
# plot each predictor against the outcome variable, Income

data %>%
  gather(-Income, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = Income)) +
  geom_point(colour = "blue", alpha = 0.2, size=0.1) +
  facet_wrap(~ var, scales = "free") +
  theme_bw()

# INCOM and INCOME PER CAP have right-skewed distributions, consider log transformation
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
regfit.full = regsubsets(Income~.,data=census.train, nvmax = 28)
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
  fit.best <- regsubsets(Income ~.,data = census.train[fold!=i,],nvmax = 28)
  test.mat<-model.matrix(Income~.,census.train[fold==i,])
  for (j in 1:28){
    coefi <- coef(fit.best,j)
    pred <- test.mat[,names(coefi)]%*%coefi
    errors[i,j] = mean((census.train$Income[fold==i] - pred)^2)
  }
}
error.mean = apply(errors,2,mean)
error.mean
which.min(error.mean)  #20
min(error.mean)  #104235704
par(mfrow=c(1,1))
plot(error.mean,type = 'b')
coef(regfit.full,20)

# run forward selection:
set.seed(1)
regfit.fwd = regsubsets(Income ~.,data = census.train, nvmax = 28, method = 'forward')
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
  fit.best <- regsubsets(Income ~.,data = census.train[fold!=i,],nvmax = 28,method = 'forward')
  test.mat<-model.matrix(Income~.,census.train[fold==i,])
  for (j in 1:28){
    coefi <- coef(fit.best,j)
    pred <- test.mat[,names(coefi)]%*%coefi
    errors[i,j] = mean((census.train$Income[fold==i] - pred)^2)
  }
}
error.mean = apply(errors,2,mean)
error.mean
which.min(error.mean) # 20
min(error.mean)  #104904990
par(mfrow=c(1,1))
plot(error.mean,type = 'b')
coef(regfit.fwd,20)

# run backward selection:
regfit.bwd = regsubsets(Income ~.,data = census.train, nvmax = 28, method = 'backward')
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

# cross validation approach -- backward
set.seed(1)
k = 10
fold <- sample(1:k,nrow(census.train), replace = TRUE)
errors <- matrix(NA,k,28,dimnames = list(NULL,paste(1:28)))
for (i in 1:k){
  fit.best <- regsubsets(Income ~.,data = census.train[fold!=i,],nvmax = 28,method = 'backward')
  test.mat<-model.matrix(Income~.,census.train[fold==i,])
  for (j in 1:28){
    coefi <- coef(fit.best,j)
    pred <- test.mat[,names(coefi)]%*%coefi
    errors[i,j] = mean((census.train$Income[fold==i] - pred)^2)
  }
}
error.mean = apply(errors,2,mean)
error.mean
which.min(error.mean) # 20
min(error.mean)  # 104235704
par(mfrow=c(1,1))
plot(error.mean,type = 'b')
# refit
coef(regfit.bwd,20)

#lasso
trainx<-as.matrix(census.train[,c(1:8,10:29)])
trainy<-as.matrix(census.train[,9])
testx<-as.matrix(census.test[,c(1:8,10:29)])
testy<-as.matrix(census.test[,9])
fullx<-as.matrix(data[,c(1:8,10:29)])
fully<-as.matrix(data[,9])

grid=10^seq(10,-2,length=100)    #Grid of lambda values
trainx=model.matrix(Income~.,census.train)[,-1]
trainy=census.train$Income
testx=model.matrix(Income~.,census.test)[,-1]
testy=census.test$Income

fullx=model.matrix(Income~.,data)[,-1]
fully=data$Income

set.seed(1)
fit.lasso <- glmnet(trainx,trainy,alpha = 1,lambda=grid)
plot(fit.lasso)
cv.lasso = cv.glmnet(trainx,trainy,alpha = 1)
plot(cv.lasso)
bestlam = cv.lasso$lambda.min
bestlam   #23.88439
pred.lasso = predict(fit.lasso, s = bestlam,newx = testx )
mean((pred.lasso-testy)^2)  #94331291
#full model
final.lasso = glmnet(fullx,fully,alpha = 1,lambda = grid)
coef.lasso = predict(final.lasso,type = "coefficients", s = bestlam)
coef.lasso
sum(coef.lasso == 0 )

#PCR
set.seed(1)
pcr.fit = pcr(Income~.,data = census.train, scale = T,validation ="CV")
summary(pcr.fit)
par(mfrow=c(1,1))
validationplot(pcr.fit,val.type='MSEP')

pcr.pred=predict(pcr.fit,testx,ncomp=31) 
mean((pcr.pred-testy)^2)

#plsr
set.seed(1)
plsr.fit = plsr(Income~.,data = census.train, scale = T,validation ="CV")
summary(plsr.fit)
par(mfrow=c(1,1))
validationplot(pcr.fit,val.type='MSEP')

pcr.pred=predict(pcr.fit,testx,ncomp=31) 
mean((pcr.pred-testy)^2)

# plot of income vs. every predictor using training set

census.train %>%
  gather(County, Men, Hispanic, Black, Asian, Citizen, IncomeErr, IncomePerCap,
         IncomePerCapErr, Poverty, ChildPoverty, Service, Office, Construction,
         Production, Drive, Carpool, WorkAtHome, MeanCommute, Employed, -Income, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = Income)) +
  geom_point(colour = "blue", alpha = 0.2, size=0.1) +
  facet_wrap(~ var, scales = "free") +
  theme_bw()

# histogram plot of every predictor using training set

census.train %>%
  gather(County, Men, Hispanic, Black, Asian, Citizen, IncomeErr, IncomePerCap,
         IncomePerCapErr, Poverty, ChildPoverty, Service, Office, Construction,
         Production, Drive, Carpool, WorkAtHome, MeanCommute, Employed, -Income, key = "var", value = "value") %>%
  ggplot(aes(value)) +
  geom_histogram(fill='blue') +
  facet_wrap(~ var, scales = "free") +
  theme_bw()

# histogram plot of every predictor using full data set

data %>%
  gather(County, Men, Hispanic, Black, Asian, Citizen, IncomeErr, IncomePerCap,
         IncomePerCapErr, Poverty, ChildPoverty, Service, Office, Construction,
         Production, Drive, Carpool, WorkAtHome, MeanCommute, Employed, -Income, key = "var", value = "value") %>%
  ggplot(aes(value)) +
  geom_histogram() +
  facet_wrap(~ var, scales = "free") +
  theme_bw()

# fit first linear model
best.subset.lm1 = lm(Income ~ County + Men + Hispanic + Black + Asian
                     + Citizen + IncomeErr + IncomePerCap + IncomePerCapErr 
                     + Poverty + ChildPoverty + Service + Office + Construction
                     + Production + Drive + WorkAtHome, 
                     data=census.train)
summary(best.subset.lm1)
par(mfrow=c(2,2))
plot(best.subset.lm1)
census.train1 = census.train
census.train1$residuals1 = residuals(best.subset.lm1)
census.train1$County = as.numeric(census.train1$County)

census.train1 %>%
  gather(County, Men, Hispanic, Black, Asian,
         Citizen, IncomeErr, IncomePerCap, IncomePerCapErr, 
         Poverty, ChildPoverty, Service, Office, Construction,
         Production, Drive, WorkAtHome, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = residuals1)) +
  geom_point(colour = "blue", alpha = 0.2, size=0.1) +
  facet_wrap(~ var, scales = "free") +
  theme_bw()

# plot 
census.train1 %>%
  gather(County, Men, Hispanic, Black, Asian,
         Citizen, IncomeErr, IncomePerCap, IncomePerCapErr, 
         Poverty, ChildPoverty, Service, Office, Construction,
         Production, Drive, WorkAtHome, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = Income)) +
  geom_point(colour = "blue", alpha = 0.2, size=0.1) + 
  facet_wrap(~ var, scales = "free") +
  theme_bw()

census.train1 %>%
  gather(County, Men, Hispanic, Black, Asian, Citizen, IncomeErr, IncomePerCap,
         IncomePerCapErr, Poverty, ChildPoverty, Service, Office, Construction,
         Production, Drive, WorkAtHome, -Income, key = "var", value = "value") %>%
  ggplot(aes(value)) +
  geom_histogram(fill='blue') +
  facet_wrap(~ var, scales = "free") +
  theme_bw()

lev = hatvalues(best.subset.lm1)
census.train[order(lev,decreasing=TRUE)[1:10]]
sort(lev,decreasing=TRUE)[1:10]

# fit second linear model
best.subset.lm2 = lm(log(Income) ~ County + Men + Hispanic + Black + Asian
                     + Citizen + IncomeErr + IncomePerCap + IncomePerCapErr 
                     + Poverty + ChildPoverty + Service + Office + Construction
                     + Production + Drive + WorkAtHome,
                     data=census.train)

summary(best.subset.lm2)
par(mfrow=c(2,2))
plot(best.subset.lm2)
census.train2 = census.train
census.train2$residuals2 = residuals(best.subset.lm2)
census.train2$County = as.numeric(census.train2$County)

plot_scat_income = ggplot(data=census.train2, aes(Income)) + 
  geom_histogram(fill='blue') + xlab("Income") + 
  ylab("Count")
print(plot_scat_income)
ggsave("model_2_hist.png", plot=plot_scat_income, width=5, height=5)

plot_scat_income_t = ggplot(data=census.train2, aes(log(Income))) + 
  geom_histogram(fill='blue') + xlab("log(Income)") + 
  ylab("Count")
print(plot_scat_income_t)
ggsave("model_2_hist_t.png", plot=plot_scat_income_t, width=5, height=5)

census.train %>%
  gather(County, Men, Hispanic, Black, Asian,
         Citizen, IncomeErr, IncomePerCap, IncomePerCapErr, 
         Poverty, ChildPoverty, Service, Office, Construction, 
         Production, Drive, WorkAtHome,
         key = "var", value = "value") %>%
  ggplot(aes(x = value, y = residuals2)) +
  geom_point(colour = "blue", alpha = 0.2, size=0.1) +
  facet_wrap(~ var, scales = "free") +
  theme_bw()

# plot of each variable after log transformation of outcome variable
census.train %>%
  gather(County, Men, Hispanic, Black, Asian,
         Citizen, IncomeErr, IncomePerCap, IncomePerCapErr, 
         Poverty, ChildPoverty, Service, Office, Construction,
         Production, Drive, WorkAtHome,
         key = "var", value = "value") %>%
  ggplot(aes(x = value, y = log(Income))) +
  geom_point(colour = "blue", alpha = 0.2, size=0.1) +
  facet_wrap(~ var, scales = "free") +
  theme_bw()

# fit third linear model
best.subset.lm3 = lm(log(Income) ~ County + Men + Hispanic + Black + I(Black^2) + Asian
                     + Citizen + IncomeErr + log(IncomePerCap) + log(IncomePerCapErr) 
                     + Poverty + ChildPoverty + Service + Office + Construction
                     + Production + Drive + WorkAtHome,
                     data=census.train)

summary(best.subset.lm3)
par(mfrow=c(2,2))
plot(best.subset.lm3)
census.train3 = census.train
census.train3$County = as.numeric(census.train3$County)
census.train3$residuals3 = residuals(best.subset.lm3)
census.train3$Black = I(census.train$Black^2)
census.train3$IncomePerCap = log(census.train$IncomePerCap)
census.train3$IncomePerCapErr = log(census.train$IncomePerCapErr)

census.train3 %>%
  gather(County, Men, Hispanic, Black, Asian,
         Citizen, IncomeErr, IncomePerCap, IncomePerCapErr, 
         Poverty, ChildPoverty, Service, Office, Construction,
         Production, Drive, WorkAtHome, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = log(Income))) +
  geom_point(colour = "blue", alpha = 0.2, size=0.1) +
  facet_wrap(~ var, scales = "free") +
  theme_bw()

plot_scat_income1 = ggplot(data=census.train, aes(x=IncomePerCap,y=Income)) + 
  geom_point(colour = "blue", alpha = 0.2, size=0.1)  + xlab("IncomePerCap") + 
  ylab("Income")
print(plot_scat_income1)
ggsave("model_3_hist1.png", plot=plot_scat_income1, width=5, height=5)

plot_scat_income1_t = ggplot(data=census.train, aes(x=log(IncomePerCap),y=Income)) + 
  geom_point(colour = "blue", alpha = 0.2, size=0.1)  + xlab("log(IncomePerCap)") + 
  ylab("Income")
print(plot_scat_income1_t)
ggsave("model_3_hist1_t.png", plot=plot_scat_income1_t, width=5, height=5)

plot_scat_income2 = ggplot(data=census.train, aes(x=Black,y=Income)) + 
  geom_point(colour = "blue", alpha = 0.2, size=0.1) + xlab("Black") + 
  ylab("Income")
print(plot_scat_income2)
ggsave("model_3_hist2.png", plot=plot_scat_income2, width=5, height=5)

plot_scat_income2_t = ggplot(data=census.train, aes(x=I(Black^2),y=Income)) + 
  geom_point(colour = "blue", alpha = 0.2, size=0.1)  + xlab("I(Black^2)") + 
  ylab("Income")
print(plot_scat_income2_t)
ggsave("model_3_hist2_t.png", plot=plot_scat_income2_t, width=5, height=5)

# fit fourth linear model
best.subset.lm4 = lm(log(Income) ~ County + sqrt(Men) + Hispanic + Black + I(Black^2) + sqrt(Asian)
                     + sqrt(Citizen) + sqrt(IncomeErr) + log(IncomePerCap) + log(IncomePerCapErr) 
                     + sqrt(Poverty) + sqrt(ChildPoverty) + Service + Office + sqrt(Construction)
                     + Production + sqrt(Drive) + sqrt(WorkAtHome), data=census.train)

summary(best.subset.lm4)
par(mfrow=c(2,2))
plot(best.subset.lm4)
census.train4 = census.train
census.train4$County = as.numeric(census.train$County)
census.train4$residuals4 = residuals(best.subset.lm4)

census.train4$Men = sqrt(census.train$Men)
census.train4$Black = census.train$Black^2
census.train4$Asian = sqrt(census.train$Asian)
census.train4$Citizen = sqrt(census.train$Citizen)
census.train4$IncomeErr = sqrt(census.train$IncomeErr)
census.train4$IncomePerCap = log(census.train$IncomePerCap)
census.train4$IncomePerCapErr = log(census.train$IncomePerCapErr)
census.train4$Poverty = sqrt(census.train$Poverty)
census.train4$ChildPoverty = sqrt(census.train$ChildPoverty)
census.train4$Construction = sqrt(census.train$Construction)
census.train4$Drive= sqrt(census.train$Drive)
census.train4$WorkAtHome = sqrt(census.train$WorkAtHome)

census.train4 %>%
  gather(County, Men, Hispanic, Black, Asian,
         Citizen, IncomeErr, IncomePerCap, IncomePerCapErr, 
         Poverty, ChildPoverty, Service, Office, Construction,
         Production, Drive, WorkAtHome, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = residuals4)) +
  geom_point(colour = "blue", alpha = 0.2, size=0.1) +
  facet_wrap(~ var, scales = "free") +
  theme_bw()

census.train4 %>%
  gather(County, Men, Hispanic, Black, Asian,
         Citizen, IncomeErr, IncomePerCap, IncomePerCapErr, 
         Poverty, ChildPoverty, Service, Office, Construction,
         Production, Drive, WorkAtHome, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = Income)) +
  geom_point(colour = "blue", alpha = 0.2, size=0.1) +
  facet_wrap(~ var, scales = "free") +
  theme_bw()

plot_scat_income1 = ggplot(data=census.train, aes(x=ChildPoverty,y=Income)) + 
  geom_point(colour = "blue", alpha = 0.2, size=0.1)  + xlab("ChildPoverty") + 
  ylab("Income")
print(plot_scat_income1)
ggsave("model_4_hist1.png", plot=plot_scat_income1, width=5, height=5)

plot_scat_income1_t = ggplot(data=census.train, aes(x=sqrt(ChildPoverty),y=Income)) + 
  geom_point(colour = "blue", alpha = 0.2, size=0.1)  + xlab("sqrt(ChildPoverty)") + 
  ylab("Income")
print(plot_scat_income1_t)
ggsave("model_4_hist1_t.png", plot=plot_scat_income1_t, width=5, height=5)

# fit fifth linear model
library(corrplot)
df_cor = subset(census.train, select = -c(Income,IncomePerCap) )
df_cor$County = as.numeric(df_cor$County)
cor(df_cor)
corrplot(df_cor, method = "circle")

cor(data$Office,data$Drive)
cor(data$Office,data$Black)

best.subset.lm5 = lm(log(Income) ~ County + sqrt(Men) + Hispanic + I(Black^2) + sqrt(Asian)
                     + sqrt(Citizen) + sqrt(IncomeErr) + log(IncomePerCap) + log(IncomePerCapErr) 
                     + sqrt(Poverty) + sqrt(ChildPoverty) + Service + Office + sqrt(Construction)
                     + Production + sqrt(Drive) + sqrt(WorkAtHome)
                     + Office*Black + sqrt(Men)*sqrt(Drive) + sqrt(ChildPoverty)*sqrt(Citizen),
                     data=census.train)

summary(best.subset.lm5)
par(mfrow=c(2,2))
plot(best.subset.lm5)

# calculate rmse
lm1.mse = sqrt(mean(((census.test$Income)-predict.lm(best.subset.lm1,census.test))^2))
print(lm1.mse)

lm2.mse = sqrt(mean(((census.test$Income)-exp(predict.lm(best.subset.lm2,census.test)))^2))
print(lm2.mse)

lm3.mse = sqrt(mean(((census.test$Income)-exp(predict.lm(best.subset.lm3,census.test)))^2))
print(lm3.mse)

lm4.mse = sqrt(mean(((census.test$Income)-exp(predict.lm(best.subset.lm4,census.test)))^2))
print(lm4.mse)

lm5.mse = sqrt(mean(((census.test$Income)-exp(predict.lm(best.subset.lm5,census.test)))^2))
print(lm5.mse)

# create a good model for income per capita in NYC and the surrounding area and comment on the differences and similarities between the two models.
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

plot_scat_income = ggplot(data=census, aes(IncomePerCap)) + 
  geom_histogram(fill='blue') + xlab("IncomePerCap") + 
  ylab("Count")
print(plot_scat_income)

plot_scat_income_t = ggplot(data=census, aes(log(IncomePerCap))) + 
  geom_histogram(fill='blue') + xlab("log(IncomePerCap)") + 
  ylab("Count")
print(plot_scat_income_t)

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

# cross validation approach -- backward
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
  gather(County, Men, Hispanic, Black, Native, Asian, Citizen, Income, IncomeErr,
         IncomePerCapErr, ChildPoverty, Service, Office, Construction,
         Production, Drive, Carpool, OtherTransp, MeanCommute, Employed, 
         PublicWork, SelfEmployed, FamilyWork,Unemployment, 
         -IncomePerCap, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = IncomePerCap)) +
  geom_point(colour = "blue", alpha = 0.2, size=0.1) +
  facet_wrap(~ var, scales = "free") +
  theme_bw()

# histogram plot of every predictor using full data set
data %>%
  gather(County, Men, Hispanic, Black, Native, Asian, Citizen, Income, IncomeErr,
         IncomePerCapErr, ChildPoverty, Service, Office, Construction,
         Production, Drive, Carpool, OtherTransp, MeanCommute, Employed, 
         PublicWork, SelfEmployed, FamilyWork,Unemployment,  
         -IncomePerCap, key = "var", value = "value") %>%
  ggplot(aes(value)) +
  geom_histogram(fill='blue') +
  facet_wrap(~ var, scales = "free") +
  theme_bw()

lm.1 = lm(IncomePerCap ~ County + Men + Hispanic + Black + Native + Asian + Citizen + 
            Income + IncomeErr + IncomePerCapErr  + ChildPoverty + 
            Service + Office + Construction + Production + Drive + Carpool  + OtherTransp + 
            MeanCommute + Employed + PublicWork + SelfEmployed + FamilyWork + 
            Unemployment, census.train)
summary(lm.1)

lm.2 = lm(IncomePerCap ~ County + Men + Hispanic + Black + Native + Asian + Citizen + 
            Income + IncomeErr + IncomePerCapErr + 
            Office + Drive + Carpool + OtherTransp + 
            Employed, census.train)
summary(lm.2)

lm.3 = lm(log(IncomePerCap) ~ County + Men + Hispanic + Black + Native + Asian + Citizen + 
            Income + IncomeErr + IncomePerCapErr  + ChildPoverty + 
            Service + Office + Construction + Production + Drive + Carpool  + OtherTransp + 
            MeanCommute + Employed + PublicWork + SelfEmployed + FamilyWork + 
            Unemployment, census.train)
summary(lm.3)

# sqrt some of the variables that are right skewed
lm.4 = lm(log(IncomePerCap) ~ County + log(Men) + sqrt(Hispanic) + sqrt(Black) + sqrt(Native) 
          + Asian + log(Citizen) + 
            log(Income) + log(IncomeErr) + log(IncomePerCapErr) + sqrt(ChildPoverty) + 
            Service + Office + sqrt(Construction) + Production + sqrt(Drive) + sqrt(Carpool)  
          + sqrt(OtherTransp) + MeanCommute +
            sqrt(Employed) + PublicWork + sqrt(SelfEmployed) + FamilyWork + sqrt(Unemployment), census.train)
summary(lm.4)

# Add interatcion term Black Men and Office Drive
lm.5 = lm(log(IncomePerCap) ~ County + log(Men) + sqrt(Hispanic) + sqrt(Black) + sqrt(Native) 
          + Asian + log(Citizen) + 
            log(Income) + log(IncomeErr) + log(IncomePerCapErr) + sqrt(ChildPoverty) + 
            Service + Office + sqrt(Construction) + Production + sqrt(Drive) + sqrt(Carpool)  
          + sqrt(OtherTransp) + MeanCommute +
            sqrt(Employed) + PublicWork + sqrt(SelfEmployed) + FamilyWork + sqrt(Unemployment)
          + log(Men)*sqrt(Black) + Office*sqrt(Drive), census.train)
summary(lm.5)

# Now use the test set to compare the different models
sqrt(mean((census.test$IncomePerCap - predict.lm(lm.1, census.test)) ^ 2))
sqrt(mean((census.test$IncomePerCap - predict.lm(lm.2, census.test)) ^ 2))
sqrt(mean((census.test$IncomePerCap - exp(predict.lm(lm.3, census.test))) ^ 2))
sqrt(mean((census.test$IncomePerCap - exp(predict.lm(lm.4, census.test))) ^ 2)) #getting inf error
sqrt(mean((census.test$IncomePerCap - exp(predict.lm(lm.5, census.test))) ^ 2)) #best

cor(data$Men,data$Black)
cor(data$Office,data$Drive)
par(mfrow=c(2,2))
plot(lm.6)

data %>%
  gather(County, Men, Hispanic, Black, Native, Asian, Citizen, Income, IncomeErr,
         IncomePerCapErr, ChildPoverty, Service, Office, Construction,
         Production, Drive, Carpool, OtherTransp, MeanCommute, Employed, 
         PublicWork, SelfEmployed, FamilyWork,Unemployment,
         key = "var", value = "value") %>%
  ggplot(aes(x = value, y = IncomePerCap)) +
  geom_point(colour = "blue", alpha = 0.2, size=0.1) +
  facet_wrap(~ var, scales = "free") +
  theme_bw()

# plot of each variable after log transformation of outcome variable
data %>%
  gather(County, Men, Hispanic, Black, Native, Asian, Citizen, Income, IncomeErr,
         IncomePerCapErr, ChildPoverty, Service, Office, Construction,
         Production, Drive, Carpool, OtherTransp, MeanCommute, Employed, 
         PublicWork, SelfEmployed, FamilyWork,Unemployment,
         key = "var", value = "value") %>%
  ggplot(aes(x = value, y = log(IncomePerCap))) +
  geom_point(colour = "blue", alpha = 0.2, size=0.1) +
  facet_wrap(~ var, scales = "free") +
  theme_bw()
