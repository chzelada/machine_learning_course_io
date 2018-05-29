library(e1071)
library(dplyr)

str(iris)
svm_model <- svm(Species~.,data = iris,kernel="sigmoid")
print(svm_model)
pred <- predict(svm_model,iris)
table(pred,iris$Species)

svm_model$SV





x1<-rnorm(60,4,2)
y1<-rnorm(60,8,3)
x2<-rnorm(60,8,2)
y2<-rnorm(60,2,3)

df1 <- data.frame(x=x1,y=y1,class="16")
df2 <- data.frame(x=x2,y=y2,class="18")


df <- rbind(df1,df2)

df$class<-factor(df$class)


plot(df$x,df$y,col=df$class)

x <- seq(0,15,by = 0.1)
y <- seq(0,15,by = 0.1)
test<-expand.grid(x=x,y=y)
svm_model <- svm(class~.,data = df,kernel="linear")
pred<-predict(svm_model,test)
plot(test$x,test$y,col=pred,pch=20)
points(df$x,df$y,col="white",pch=as.numeric(df$class))



