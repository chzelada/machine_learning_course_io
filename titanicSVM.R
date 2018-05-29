library(e1071)
library(dplyr)
titanic <- read_csv("~/Documents/machine_learning_course_io/titanic.csv")

names(titanic)

titanic<-
titanic %>% select(-PassengerId,-Cabin,-Ticket,-Name)


summary(titanic)

titanic$Survived <- factor(titanic$Survived)
titanic$Pclass <- factor(titanic$Pclass)
titanic$Sex <- factor(titanic$Sex)
titanic$Embarked <- factor(titanic$Embarked)

summary(titanic)

titanic$Age[is.na(titanic$Age)] <- median(titanic$Age,na.rm = TRUE)

summary(titanic)

titanic$Embarked[is.na(titanic$Embarked)]<-"S"

summary(titanic)


train_index <- sample(1:nrow(titanic),size = 0.7*nrow(titanic))
train <- titanic[train_index,]
test <- titanic[-train_index,]


summary(train)
summary(test)


svm_model <- svm(Survived~. , data = train)

pred_train <- predict(svm_model, train)
table(pred_train,train$Survived)

pred_test<-predict(svm_model, test)
table(pred_test,test$Survived)




