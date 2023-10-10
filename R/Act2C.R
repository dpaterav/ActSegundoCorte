
library(tidyverse)
library(dplyr)
library(caret)
library(class)
library(gmodels)

folder <- dirname(rstudioapi::getSourceEditorContext()$path)
parentFolder <- dirname(folder)

DIABETES <-
  read_csv(paste0(parentFolder
                  ,"/data/diabetes_012_health_indicators_BRFSS2015.csv"))

DIABETES_DF <- diabetes_3

# 1 = Tiene diabetes, 0 = No tiene diabetes

DIABETES_DF$Diabetes_012 <- ifelse(DIABETES_DF$Diabetes_012 >= 1,1,0)

# Muestreo estratificado aleatorio #
set.seed(1)
DIAB_DF <- DIABETES_DF %>%
  group_by(Diabetes_012) %>%
  sample_n(1268, replace = FALSE) %>%
  ungroup()

# Análisis exploratorio de datos #

hist(DIABETES_DF$Diabetes_012
     ,main = "Histograma presencia de la enfermedad",
     xlab = "Presencia de la enfermedad", ylab = "Frecuencia"
     ,col= "red")
hist(DIAB_DF$Diabetes_012
     ,main = "Histograma presencia de la enfermedad",
     xlab = "Presencia de la enfermedad", ylab = "Frecuencia"
     ,col= "red")

plot(DIABETES_DF[4:6])
plot(DIABETES_DF[4:6], pch=21
     , bg= c("red", "green")[unclass(DIABETES_DF$Diabetes_012)])


plot(DIAB_DF[4:6])
plot(DIAB_DF[4:6], pch=21
     , bg= c("red", "green")[unclass(DIAB_DF$Diabetes_012)])

# MODELO KNN DIABETES_012#

# Modelo con 21 variables , cross validation= 10 #

DIAB_DF$Diabetes_012 <- as.factor(DIAB_DF$Diabetes_012)

set.seed(1)
sample.index <- sample(1:nrow(DIAB_DF)
                       ,nrow(DIAB_DF)*0.7
                       ,replace = F)

predictors <- c("HighBP","HighChol","CholCheck","BMI", "Smoker","Stroke","HeartDiseaseorAttack","PhysActivity"
                ,"Fruits","Veggies","HvyAlcoholConsump", "AnyHealthcare","NoDocbcCost","GenHlth","MentHlth","PhysHlth","DiffWalk"
                ,"Sex","Age","Education","Income")

## Datos de entrenamiento y prueba
train.data <- DIAB_DF[sample.index,c(predictors,"Diabetes_012"),drop=F]
test.data <- DIAB_DF[-sample.index,c(predictors,"Diabetes_012"),drop=F]

## Primer modelo knn
ctrl <- trainControl(method="cv",  number=10)
knnFit <- train(Diabetes_012 ~ HighBP + HighChol + CholCheck +BMI + Smoker + Stroke + HeartDiseaseorAttack + PhysActivity + Fruits
                + Veggies + HvyAlcoholConsump + AnyHealthcare + NoDocbcCost + GenHlth + MentHlth + PhysHlth + DiffWalk + Sex + Age + Education + Income
                , data = train.data
                , method = "knn", trControl = ctrl
                , preProcess = c("range") #c("center", "scale")
                , tuneLength = 25)

knnFit
plot(knnFit)

# Predicciones

knnPredict <- predict(knnFit, newdata = test.data)
knnPredict
confusionMatrix(knnPredict, test.data$Diabetes_012)

# Modelo con 16 variables  predictoras , cross validation = 5 #

DIAB_DF$Diabetes_012 <- as.factor(DIAB_DF$Diabetes_012)

set.seed(1)
sample.index1 <- sample(1:nrow(DIAB_DF)
                        ,nrow(DIAB_DF)*0.7
                        ,replace = F)

predictors1 <- c("HighBP","HighChol","CholCheck","BMI", "Smoker","Stroke","HeartDiseaseorAttack","PhysActivity"
                 ,"Fruits","Veggies","HvyAlcoholConsump", "AnyHealthcare","NoDocbcCost","PhysHlth"
                 ,"Sex","Age")

# Datos de entrenamiento y prueba
train.data1 <- DIAB_DF[sample.index1,c(predictors1,"Diabetes_012"),drop=F]
test.data1 <- DIAB_DF[-sample.index1,c(predictors1,"Diabetes_012"),drop=F]

# Primer modelo knn
ctrl1 <- trainControl(method="cv",  number = 5)
knnFit1 <- train(Diabetes_012 ~ HighBP + HighChol + CholCheck +BMI + Smoker + Stroke + HeartDiseaseorAttack + PhysActivity + Fruits
                 + Veggies + HvyAlcoholConsump + AnyHealthcare + NoDocbcCost + PhysHlth + Sex + Age
                 , data = train.data1
                 , method = "knn", trControl = ctrl1
                 , preProcess = c("range") #c("center", "scale")
                 , tuneLength = 25)

knnFit1
plot(knnFit1)

#Predicciones

knnPredict1 <- predict(knnFit1, newdata = test.data1)
knnPredict1
confusionMatrix(knnPredict1, test.data1$Diabetes_012)

# Modelo con 11 variables  predictoras , cross validation = 5 #

DIAB_DF$Diabetes_012 <- as.factor(DIAB_DF$Diabetes_012)

set.seed(1)
sample.index2 <- sample(1:nrow(DIAB_DF)
                        ,nrow(DIAB_DF)*0.7
                        ,replace = F)

predictors2 <- c("HighBP","HighChol","BMI","HeartDiseaseorAttack","PhysActivity"
                 ,"Fruits","Veggies", "AnyHealthcare","PhysHlth"
                 ,"Sex","Age")

# Datos de entrenamiento y prueba
train.data2 <- DIAB_DF[sample.index2,c(predictors2,"Diabetes_012"),drop=F]
test.data2 <- DIAB_DF[-sample.index2,c(predictors2,"Diabetes_012"),drop=F]

# Primer modelo knn
ctrl2 <- trainControl(method="cv",  number = 3)
knnFit2 <- train(Diabetes_012 ~ HighBP + HighChol  +BMI   + HeartDiseaseorAttack + PhysActivity + Fruits
                 + Veggies +  AnyHealthcare  + PhysHlth + Sex + Age
                 , data = train.data2
                 , method = "knn", trControl = ctrl
                 , preProcess = c("range") #c("center", "scale")
                 , tuneLength = 25)

knnFit2
plot(knnFit2)

# Predicciones

knnPredict2 <- predict(knnFit2, newdata = test.data2)
knnPredict2
confusionMatrix(knnPredict2, test.data2$Diabetes_012)

# Modelo knn Heart Diseaseor attack #

Heart_attack_DF <- DIABETES
Heart_attack_DF$HeartDiseaseorAttack <- as.factor(Heart_attack_DF$HeartDiseaseorAttack)

set.seed(1)
H_A_DF <- Heart_attack_DF %>%
  group_by(HeartDiseaseorAttack) %>%
  sample_n(1268, replace = FALSE) %>%
  ungroup()

# Modelo con 21 variables predictoras #
set.seed(1)
sample.index3 <- sample(1:nrow(H_A_DF)
                        ,nrow(H_A_DF)*0.7
                        ,replace = F)

predictors3 <- c("Diabetes_012","HighBP","HighChol","CholCheck","BMI", "Smoker","Stroke","PhysActivity"
                 ,"Fruits","Veggies","HvyAlcoholConsump", "AnyHealthcare","NoDocbcCost","GenHlth","MentHlth","PhysHlth","DiffWalk"
                 ,"Sex","Age","Education","Income")

# Datos de entrenamiento y prueba
train.data3 <- H_A_DF[sample.index3,c(predictors3,"HeartDiseaseorAttack"),drop=F]
test.data3 <- H_A_DF[-sample.index3,c(predictors3,"HeartDiseaseorAttack"),drop=F]

# Primer modelo knn
ctrl3 <- trainControl(method="cv",  number = 10)
knnFit3 <- train(HeartDiseaseorAttack ~.
                 , data = train.data3
                 , method = "knn", trControl = ctrl3
                 , preProcess = c("range") #c("center", "scale")
                 , tuneLength = 25)

knnFit3
plot(knnFit3)

# Predicciones

knnPredict3 <- predict(knnFit3, newdata = test.data3)
knnPredict3
confusionMatrix(knnPredict3, test.data3$HeartDiseaseorAttack)

# Modelo con 16 variable #


set.seed(1)
sample.index4 <- sample(1:nrow(H_A_DF)
                        ,nrow(H_A_DF)*0.7
                        ,replace = F)

predictors4 <- c("Diabetes_012","HighBP","HighChol","BMI", "Smoker","Stroke","PhysActivity"
                 ,"Fruits","Veggies","HvyAlcoholConsump", "AnyHealthcare","GenHlth","PhysHlth","DiffWalk"
                 ,"Sex","Age")

# Datos de entrenamiento y prueba
train.data4 <- H_A_DF[sample.index4,c(predictors4,"HeartDiseaseorAttack"),drop=F]
test.data4 <- H_A_DF[-sample.index4,c(predictors4,"HeartDiseaseorAttack"),drop=F]

# Primer modelo knn
ctrl4 <- trainControl(method="cv",  number = 5)
knnFit4 <- train(HeartDiseaseorAttack ~ Diabetes_012 + HighBP + HighChol + BMI + Smoker + Stroke + PhysActivity
                 + Fruits + Veggies + HvyAlcoholConsump + AnyHealthcare + GenHlth + PhysHlth + DiffWalk
                 + Sex + Age
                 , data = train.data4
                 , method = "knn", trControl = ctrl4
                 , preProcess = c("range") #c("center", "scale")
                 , tuneLength = 25)

knnFit4
plot(knnFit4)

# Predicciones

knnPredict4 <- predict(knnFit4, newdata = test.data4)
knnPredict4
confusionMatrix(knnPredict4, test.data4$HeartDiseaseorAttack)


# Modelo con 11 variable #


set.seed(1)
sample.index5 <- sample(1:nrow(H_A_DF)
                        ,nrow(H_A_DF)*0.7
                        ,replace = F)

predictors5 <- c("HighBP","HighChol","BMI", "Smoker","PhysActivity"
                 ,"Veggies","HvyAlcoholConsump","PhysHlth","DiffWalk"
                 ,"Sex","Age")

# Datos de entrenamiento y prueba
train.data5 <- H_A_DF[sample.index5,c(predictors5,"HeartDiseaseorAttack"),drop=F]
test.data5 <- H_A_DF[-sample.index5,c(predictors5,"HeartDiseaseorAttack"),drop=F]

# Primer modelo knn
ctrl5 <- trainControl(method="cv",  number = 3)
knnFit5 <- train(HeartDiseaseorAttack ~  HighBP + HighChol + BMI + Smoker + PhysActivity
                 + Veggies + HvyAlcoholConsump  + PhysHlth + DiffWalk
                 + Sex + Age
                 , data = train.data5
                 , method = "knn", trControl = ctrl5
                 , preProcess = c("range") #c("center", "scale")
                 , tuneLength = 25)

knnFit5
plot(knnFit5)

# Predicciones

knnPredict5 <- predict(knnFit5, newdata = test.data5)
knnPredict5
confusionMatrix(knnPredict5, test.data5$HeartDiseaseorAttack)


# Modelo knn sex #

SEX <- DIABETES
SEX$Sex <- as.factor(SEX$Sex)

set.seed(1)
SeX <- SEX %>%
  group_by(Sex) %>%
  sample_n(1268, replace = FALSE) %>%
  ungroup()

# Modelo con 21 variables predictoras #
set.seed(1)
sample.index6 <- sample(1:nrow(SeX)
                        ,nrow(SeX)*0.7
                        ,replace = F)

predictors6 <- c("Diabetes_012","HighBP","HighChol","CholCheck","BMI", "Smoker","Stroke","PhysActivity"
                 ,"Fruits","Veggies","HvyAlcoholConsump", "AnyHealthcare","NoDocbcCost","GenHlth","MentHlth","PhysHlth","DiffWalk"
                 ,"HeartDiseaseorAttack","Age","Education","Income")

# Datos de entrenamiento y prueba
train.data6 <- SeX[sample.index6,c(predictors6,"Sex"),drop=F]
test.data6 <- SeX[-sample.index6,c(predictors6,"Sex"),drop=F]

# Primer modelo knn
ctrl6 <- trainControl(method="cv",  number = 10)
knnFit6 <- train(Sex ~.
                 , data = train.data6
                 , method = "knn", trControl = ctrl6
                 , preProcess = c("range") #c("center", "scale")
                 , tuneLength = 25)

knnFit6
plot(knnFit6)

# Predicciones

knnPredict6 <- predict(knnFit6, newdata = test.data6)
knnPredict6
confusionMatrix(knnPredict6, test.data6$Sex)

# modelo con 16 variables predictoras #
set.seed(1)
sample.index7 <- sample(1:nrow(SeX)
                        ,nrow(SeX)*0.7
                        ,replace = F)

predictors7 <- c("Diabetes_012","HighBP","HighChol","BMI", "Smoker","PhysActivity"
                 ,"Fruits","Veggies","HvyAlcoholConsump","MentHlth","PhysHlth","DiffWalk"
                 ,"HeartDiseaseorAttack","Age","Education","Income")

# Datos de entrenamiento y prueba
train.data7 <- SeX[sample.index7,c(predictors7,"Sex"),drop=F]
test.data7 <- SeX[-sample.index7,c(predictors7,"Sex"),drop=F]

# Primer modelo knn
ctrl7 <- trainControl(method="cv",  number = 5)
knnFit7 <- train(Sex ~ Diabetes_012 + HighBP + HighChol + BMI + Smoker + PhysActivity
                 + Fruits + Veggies + HvyAlcoholConsump + MentHlth + PhysHlth + DiffWalk
                 + HeartDiseaseorAttack + Age + Education + Income
                 , data = train.data7
                 , method = "knn", trControl = ctrl7
                 , preProcess = c("range") #c("center", "scale")
                 , tuneLength = 25)

knnFit7
plot(knnFit7)

# Predicciones

knnPredict7 <- predict(knnFit7, newdata = test.data7)
knnPredict7
confusionMatrix(knnPredict7, test.data7$Sex)


# modelo con 11 variables predictoras #
set.seed(1)
sample.index8 <- sample(1:nrow(SeX)
                        ,nrow(SeX)*0.7
                        ,replace = F)

predictors8 <- c("Diabetes_012","BMI", "Smoker","PhysActivity"
                 ,"HvyAlcoholConsump","MentHlth","DiffWalk"
                 ,"HeartDiseaseorAttack","Age","Education","Income")

# Datos de entrenamiento y prueba
train.data8 <- SeX[sample.index8,c(predictors8,"Sex"),drop=F]
test.data8 <- SeX[-sample.index8,c(predictors8,"Sex"),drop=F]

# Primer modelo knn
ctrl8 <- trainControl(method="cv",  number = 3)
knnFit8 <- train(Sex ~ Diabetes_012 + BMI + Smoker + PhysActivity
                 + HvyAlcoholConsump + MentHlth  + DiffWalk
                 + HeartDiseaseorAttack + Age + Education + Income
                 , data = train.data8
                 , method = "knn", trControl = ctrl8
                 , preProcess = c("range") #c("center", "scale")
                 , tuneLength = 25)

knnFit8
plot(knnFit8)

# Predicciones

knnPredict8 <- predict(knnFit8, newdata = test.data8)
knnPredict8
confusionMatrix(knnPredict8, test.data8$Sex)

