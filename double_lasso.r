library(glmnet)
#####################################
# ANALYSIS #
#####################################
rm(list=ls())

rain <- read.csv("E:/New folder (2)/rain.csv")
rice <- read.csv("E:/New folder (2)/rice.csv")
cotton <- read.csv("E:/New folder (2)/cotton.csv")
work <- read.csv("E:/New folder (2)/work.csv")
borewells <- read.csv("E:/New folder (2)/borewells.csv")
water_table <- read.csv("E:/New folder (2)/water_table.csv")
###############################################
work_pond=work[1:37,2:22+1]
work_soakpits=work[38:74,2:22+1]
work_clean_trench=work[75:111,2:22+1]
work_restore_revolute=work[112:148,2:22+1]
work_clear_waterways=work[149:185,2:22+1]
work_plantation=work[186:222,2:22+1]
##############################################
rain_2=rain[,1:21+1]
rain_2=as.matrix(rain_2)
rain_2=as.vector(rain_2)

rice_2=rice[,1:21+1]
rice_2=as.matrix(rice_2)
rice_2=as.vector(rice_2)

cotton_2=cotton[,1:21+1]
cotton_2=as.matrix(cotton_2)
cotton_2=as.vector(cotton_2)

borewells_2=borewells[,1:21+1]
borewells_2=as.matrix(borewells_2)
borewells_2=as.vector(borewells_2)

work_pond_2=as.matrix(work_pond)
work_pond_2=as.vector(work_pond_2)

work_soakpits_2=as.matrix(work_soakpits)
work_soakpits_2=as.vector(work_soakpits_2)

work_clean_trench_2=as.matrix(work_clean_trench)
work_clean_trench_2=as.vector(work_clean_trench_2)

work_restore_revolute_2=as.matrix(work_restore_revolute)
work_restore_revolute_2=as.vector(work_restore_revolute_2)

work_clear_waterways_2=as.matrix(work_clear_waterways)
work_clear_waterways_2=as.vector(work_clear_waterways_2)

work_plantation_2=as.matrix(work_plantation)
work_plantation_2=as.vector(work_plantation_2)

water_table_2=water_table[,1:21+1]
water_table_2=as.matrix(water_table_2)
water_table_2=as.vector(water_table_2)
################################################
data=cbind(rain_2, rice_2, cotton_2, borewells_2, work_pond_2, work_soakpits_2,work_clean_trench_2,work_restore_revolute_2,work_clear_waterways_2,work_plantation_2,water_table_2)
data=data.frame(data)

testvar <-as.matrix(subset(data[,1:10]))
water_level<-as.matrix(data$water_table_2)
#-------------------------------------------------------------
# STEP 1: select variables that predict outcomes
n=nrow(testvar)
p=ncol(testvar)
sda = sd(residuals(lm(water_level ~ rain_2, data=data)))
lambda1 = sda*(1.1/sqrt(n))* qnorm(1 - (.1/log(n))/(2*p))
k = 1
while(k < 15){
  fitY = glmnet(testvar, water_level, lambda=lambda1)
  ba = coef(fitY, s = lambda1)
  err= water_level-predict(fitY,testvar)
  sda = sd(err)
  lambda1 = sda*(1.1/sqrt(n))* qnorm(1 - (.1/log(n))/(2*p))
  k=k+1
}
ba
# STEP 2: select variables that predict treatment
sd1=sd(rain_2)
lambda1 = sd1*(1.1/sqrt(n))* qnorm(1 - (.1/log(n))/(2*p))
#summary(lambda1)
k = 1
while(k < 15){
  fitT1 = glmnet(testvar, rain_2, lambda=lambda1)
  ba = coef(fitT1, s = lambda1)
  err = rain_2-predict(fitT1,testvar)
  sda = sd(err)
  lambda1 = sda*(1.1/sqrt(n))* qnorm(1 - (.1/log(n))/(2*p))
  k=k+1
}
ba
# STEP 3: linear regression with both sets of variables
# Regression without X predictors
use1 = union(which(abs(fitY$beta)>.0001),which(abs(fitT1$beta)>.0001))
X = cbind(rain_2,testvar)
fitr <- lm(water_level ~ X[,use1], data=data)
summary(fitr) # show results
#----------------------------------------------------------------
sd1=sd(rice_2)
lambda1 = sd1*(1.1/sqrt(n))* qnorm(1 - (.1/log(n))/(2*p))
#summary(lambda1)
k = 1
while(k < 15){
  fitT2 = glmnet(testvar, rice_2, lambda=lambda1)
  ba = coef(fitT2, s = lambda1)
  err = rice_2-predict(fitT2,testvar)
  sda = sd(err)
  lambda1 = sda*(1.1/sqrt(n))* qnorm(1 - (.1/log(n))/(2*p))
  k=k+1
}
ba
#------------------------------------------------------------------
sd1=sd(cotton_2)
lambda1 = sd1*(1.1/sqrt(n))* qnorm(1 - (.1/log(n))/(2*p))
#summary(lambda1)
k = 1
while(k < 15){
  fitT3 = glmnet(testvar, cotton_2, lambda=lambda1)
  ba = coef(fitT3, s = lambda1)
  err = cotton_2-predict(fitT3,testvar)
  sda = sd(err)
  lambda1 = sda*(1.1/sqrt(n))* qnorm(1 - (.1/log(n))/(2*p))
  k=k+1
}
ba
#------------------------------------------------------------------
sd1=sd(borewells_2)
lambda1 = sd1*(1.1/sqrt(n))* qnorm(1 - (.1/log(n))/(2*p))
#summary(lambda1)
k = 1
while(k < 15){
  fitT4 = glmnet(testvar, borewells_2, lambda=lambda1)
  ba = coef(fitT4, s = lambda1)
  err = borewells_2-predict(fitT4,testvar)
  sda = sd(err)
  lambda1 = sda*(1.1/sqrt(n))* qnorm(1 - (.1/log(n))/(2*p))
  k=k+1
}
ba
#------------------------------------------------------------------
sd1=sd(work_pond_2)
lambda1 = sd1*(1.1/sqrt(n))* qnorm(1 - (.1/log(n))/(2*p))
#summary(lambda1)
k = 1
while(k < 15){
  fitT5 = glmnet(testvar, work_pond_2, lambda=lambda1)
  ba = coef(fitT5, s = lambda1)
  err = work_pond_2-predict(fitT5,testvar)
  sda = sd(err)
  lambda1 = sda*(1.1/sqrt(n))* qnorm(1 - (.1/log(n))/(2*p))
  k=k+1
}
ba
#------------------------------------------------------------------
sd1=sd(work_soakpits_2)
lambda1 = sd1*(1.1/sqrt(n))* qnorm(1 - (.1/log(n))/(2*p))
#summary(lambda1)
k = 1
while(k < 15){
  fitT6 = glmnet(testvar, work_soakpits_2, lambda=lambda1)
  ba = coef(fitT6, s = lambda1)
  err = work_soakpits_2-predict(fitT6,testvar)
  sda = sd(err)
  lambda1 = sda*(1.1/sqrt(n))* qnorm(1 - (.1/log(n))/(2*p))
  k=k+1
}
ba
#------------------------------------------------------------------
sd1=sd(work_clean_trench_2)
lambda1 = sd1*(1.1/sqrt(n))* qnorm(1 - (.1/log(n))/(2*p))
#summary(lambda1)
k = 1
while(k < 15){
  fitT7 = glmnet(testvar, work_clean_trench_2, lambda=lambda1)
  ba = coef(fitT7, s = lambda1)
  err = work_clean_trench_2-predict(fitT7,testvar)
  sda = sd(err)
  lambda1 = sda*(1.1/sqrt(n))* qnorm(1 - (.1/log(n))/(2*p))
  k=k+1
}
ba
#------------------------------------------------------------------
sd1=sd(work_restore_revolute_2)
lambda1 = sd1*(1.1/sqrt(n))* qnorm(1 - (.1/log(n))/(2*p))
#summary(lambda1)
k = 1
while(k < 15){
  fitT8 = glmnet(testvar, work_restore_revolute_2, lambda=lambda1)
  ba = coef(fitT8, s = lambda1)
  err = work_restore_revolute_2-predict(fitT8,testvar)
  sda = sd(err)
  lambda1 = sda*(1.1/sqrt(n))* qnorm(1 - (.1/log(n))/(2*p))
  k=k+1
}
ba
#------------------------------------------------------------------
sd1=sd(work_clear_waterways_2)
lambda1 = sd1*(1.1/sqrt(n))* qnorm(1 - (.1/log(n))/(2*p))
#summary(lambda1)
k = 1
while(k < 15){
  fitT9 = glmnet(testvar, work_clear_waterways_2, lambda=lambda1)
  ba = coef(fitT9, s = lambda1)
  err = work_clear_waterways_2-predict(fitT9,testvar)
  sda = sd(err)
  lambda1 = sda*(1.1/sqrt(n))* qnorm(1 - (.1/log(n))/(2*p))
  k=k+1
}
ba
#------------------------------------------------------------------
sd1=sd(work_plantation_2)
lambda1 = sd1*(1.1/sqrt(n))* qnorm(1 - (.1/log(n))/(2*p))
#summary(lambda1)
k = 1
while(k < 15){
  fitT10 = glmnet(testvar, work_plantation_2, lambda=lambda1)
  ba = coef(fitT10, s = lambda1)
  err = work_plantation_2-predict(fitT10,testvar)
  sda = sd(err)
  lambda1 = sda*(1.1/sqrt(n))* qnorm(1 - (.1/log(n))/(2*p))
  k=k+1
}
ba
#------------------------------------------------------------------
use2 = union(which(abs(fitT2$beta)>.0001),which(abs(fitT3$beta) > .0001))
use3 = union(which(abs(fitT4$beta)>.0001),which(abs(fitT5$beta) > .0001))
use4 = union(which(abs(fitT6$beta)>.0001),which(abs(fitT7$beta) > .0001))
use5 = union(which(abs(fitT8$beta)>.0001),which(abs(fitT9$beta) > .0001))
use6 = which(abs(fitT10$beta)>.0001)
use12 = union (use1, use2)
use34 = union (use3, use4)
use56 = union (use5, use6)
use1234 = union (use12, use34)
use = union (use1234, use56)
X = cbind(rain_2, rice_2, cotton_2, borewells_2, work_pond_2, work_soakpits_2,work_clean_trench_2,work_restore_revolute_2,work_clear_waterways_2,work_plantation_2, testvar)
summary(use)
fitr <- lm(water_level ~ X[,use], data=data)
summary(fitr) # show results
