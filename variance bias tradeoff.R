library(dplyr) 

mtcars_data<- mtcars %>% dplyr::select(disp,mpg)
 

disp_x <- 70:475

train_fit_1 <- lm(mpg~disp, data=mtcars_data) 
train_fit_2 <- lm(mpg~disp+I(disp^2), data=mtcars_data) 
train_fit_3 <- lm(mpg~disp+I(disp^2)+I(disp^3), data=mtcars_data) 
train_fit_12 <- lm(mpg~poly(disp,15), data=mtcars_data) 

mpg_y <- predict(train_fit_1,data.frame(disp=disp_x))
plot(mtcars_data)
points(disp_x,mpg_y,type="l")
mpg_y <- predict(train_fit_2,data.frame(disp=disp_x))
points(disp_x,mpg_y,type="l",col='blue')
mpg_y <- predict(train_fit_3,data.frame(disp=disp_x))
points(disp_x,mpg_y,type="l",col='red')
mpg_y <- predict(train_fit_12,data.frame(disp=disp_x))
points(disp_x,mpg_y,type="l",col='green')

sse_vec<-c()
for(i in 1:19){
  fit_lm<-lm(mpg~poly(disp,i), data=mtcars_data) 
  sse <- sum(fit_lm$residuals^2)
  sse_vec <- c(sse_vec,sse) 
}

row.names(mtcars_data)<-NULL

cv<-function(){
  sse_vec_mean<-c()
  mtcars_data <- mtcars_data[sample(1:32),]
  mtcars_data$cv <- 1:4
  for(i in 1:10){
    sse_vec_cv<-c()
    for(j in 1:4){
      train <- mtcars_data %>% filter(cv!=j)
      test <- mtcars_data %>% filter(cv==j)
      fit_lm<-lm(mpg~poly(disp,i), data=train ) 
      yhat <- predict(fit_lm,test %>% dplyr::select(disp) )
      sse<- sum((yhat-test$mpg)^2)
      sse_vec_cv <- c(sse_vec_cv,sse)
    }
    sse_vec_mean <- c(sse_vec_mean,mean(sse_vec_cv) )
  }
  return(sse_vec_mean)
}

data<-cv()
data<-rbind(data,cv(),cv(),cv(),cv(),cv(),cv(),cv())




plot(1:19,sse_vec,type = 'l', col = 'red')
points(1:10,colMeans(data),col='blue',type='l')
points(1:10,cv(),col='blue',type='p')

train_fit_5 <- lm(mpg~poly(disp,3), data=mtcars_data)
plot(mtcars_data %>% dplyr::select(disp,mpg))
mpg_y <- predict(train_fit_5,data.frame(disp=disp_x))
points(disp_x,mpg_y,type="l",col='red')
summary(train_fit_5)









