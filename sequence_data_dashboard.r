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
# split the work by shape
work_pond=work[1:37,2:22+1]
work_soakpits=work[38:74,2:22+1]
work_clean_trench=work[75:111,2:22+1]
work_restore_revolute=work[112:148,2:22+1]
work_clear_waterways=work[149:185,2:22+1]
work_plantation=work[186:222,2:22+1]
##############################################
# convert the all matrices related to year and district to vector
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

#independent_ <- as.matrix(subset(data,select = c(rain_2, rice_2, cotton_2, borewells_2, work_pond_2, work_soakpits_2,work_clean_trench_2,work_restore_revolute_2,work_clear_waterways_2,work_plantation_2)))
#fitr <- lm(water_table_2 ~ independent_, data=data)
#summary(fitr) # show results

testvar <-as.matrix(subset(data[,1:10]))
water_level<-as.matrix(data$water_table_2)

cv_model <- cv.glmnet(testvar, water_level, alpha = 1)
best_lambda <- cv_model$lambda.min
best_lambda
plot(cv_model)
best_model <- glmnet(testvar, water_level, alpha = 1, lambda = best_lambda)
coef(best_model)
