library(e1071)
library(dplyr)

str(iris)
svm_model <- svm(Species~.,data = iris,kernel="radial")
print(svm_model)
pred <- predict(svm_model,iris)
table(pred,iris$Species)
