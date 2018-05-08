library(dplyr) 
 
mtcars_data<- mtcars %>% dplyr::select(disp,wt,mpg)

cv<-function(){
  sse_vec_mean<-c()
  mtcars_data <- mtcars_data[sample(1:32),]
  mtcars_data$cv <- 1:4
  for(i in 1:6){
    sse_vec_cv<-c()
    for(j in 1:4){
      train <- mtcars_data %>% filter(cv!=j)
      test <- mtcars_data %>% filter(cv==j)
      fit_lm<-lm(mpg~poly(disp,i)+poly(wt,i), data=train ) 
      yhat <- predict(fit_lm,test %>% dplyr::select(disp,wt) )
      sse<- sum((yhat-test$mpg)^2)
      sse_vec_cv <- c(sse_vec_cv,sse)
    }
    sse_vec_mean <- c(sse_vec_mean, round(mean(sse_vec_cv)) )
  }
  return(sse_vec_mean)
}

debug(cv)
cv()

data<-cv()
data<-rbind(data,cv(),cv(),cv(),cv(),cv(),cv(),cv())

colMeans(data)


fit_lm<-lm(mpg~poly(disp,2)+poly(wt,2), data=mtcars_data )
summary(fit_lm)
plot(mtcars)
