library(tidyverse)

folder <- dirname(rstudioapi::getSourceEditorContext()$path)
parentFolder <- dirname(folder)
diabetes_3 <- read.csv(paste0(parentFolder
                              ,"/Data/diabetes_012_health_indicators_BRFSS2015.csv")
                       , stringsAsFactors = TRUE)

diabetes<-
  diabetes_3[sample(nrow(diabetes_3), 2536),]

predictors_1 <- colnames(diabetes)[-5]
predictors_2 <- colnames(diabetes)[-16]
predictors_3 <- colnames(diabetes)[-17]

# Muestreo
set.seed(1)
index.train <- sample(1:nrow(diabetes)
                      ,0.7*nrow(diabetes)
                      ,replace = FALSE)

# Modelo 1 con variable target BWI
train.data_3 <- diabetes[index.train,c(predictors_1,"BMI"),drop=F]
test.data_3<- diabetes[-index.train,c(predictors_1,"BMI"),drop=F]

ins_model_1 <-lm(BMI ~ ., data = train.data)
ins_model_1

train.output_1 <- predict(ins_model_1, newdata = test.data)
summary(ins_model_1)

# Error cuadratico medio 1
MSE_1<-
  data.frame(predicted = train.output_1
             ,actual = test.data$BMI
             ,MSE_1 =((train.output_1 - test.data$BMI)^2)/nrow(test.data))

MSE_1$RMSE_1 <- sqrt(MSE_1$MSE_1)

# Modelo 2 con variable target MentHlth

train.data_3 <- diabetes[index.train,c(predictors_2,"MentHlth"),drop=F]
test.data_3 <- diabetes[-index.train,c(predictors_2,"MentHlth"),drop=F]

ins_model_2 <-lm(MentHlth ~ ., data = train.data)
ins_model_2

train.output_2 <- predict(ins_model_2, newdata = test.data)
summary(ins_model_2)

# Error cuadratico medio 2
MSE_2<-
  data.frame(predicted = train.output_2
             ,actual = test.data$MentHlth
             ,MSE_2 =((train.output_2 - test.data$MentHlth)^2)/nrow(test.data))

MSE_2$RMSE_2 <- sqrt(MSE_2$MSE_2)


# Modelo 3 con variable target PhysHlth

train.data_3 <- diabetes[index.train,c(predictors_2,"PhysHlth"),drop=F]
test.data_3 <- diabetes[-index.train,c(predictors_2,"PhysHlth"),drop=F]

ins_model_3 <-lm(PhysHlth ~ ., data = train.data)
ins_model_3

train.output_3 <- predict(ins_model_3, newdata = test.data)
summary(ins_model_3)

# Error cuadratico medio 3
MSE_3<-
  data.frame(predicted = train.output_3
             ,actual = test.data$PhysHlth
             ,MSE_3 =((train.output_3 - test.data$PhysHlth)^2)/nrow(test.data))

MSE_3$RMSE_3 <- sqrt(MSE_3$MSE_3)
