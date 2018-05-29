library(readr) 
library(dplyr)
library(class)
library(caret)
library(AppliedPredictiveModeling)

data(iris)
glimpse(iris)

featurePlot(x = iris %>% dplyr::select(-Species), 
            y = iris$Species, 
            plot = "ellipse",
            auto.key = list(columns = 3))

index <- sample(1:nrow(iris),nrow(iris)*0.7)
train <- iris[index,]
test <- iris[-index,]

preProcValues <- preProcess(train, method = c("center", "scale"))
train <- predict(preProcValues, train)
test <- predict(preProcValues, test)

pred <- knn(train %>% dplyr::select(-Species)  ,
            test%>% dplyr::select(-Species),
            train$Species,k=1)

table(origianl=test$Species,pred)
