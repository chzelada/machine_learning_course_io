#### https://rpubs.com/dvorakt/143162
library(readr)
library(dplyr)
library(class)
library(caret)
library(AppliedPredictiveModeling) 

loan_data <- read_csv("loan_data.csv")

glimpse(loan_data)

nzv <- nearZeroVar(loan_data)
loan_data <- loan_data[,-nzv]

unique(loan_data$loan_status)

loan_data <- loan_data %>% filter(!is.na(loan_status))

good_status<-c("Current", "Fully Paid", "Does not meet the credit policy.  Status:Fully Paid" )

loan_data <- loan_data %>% 
  mutate(good = case_when(
    loan_status %in% good_status ~ "good",
    TRUE ~ "bad"
  ))

table(loan_data$good)


## FICO scores

loan_data <- loan_data %>% mutate(fico = (fico_range_high+ fico_range_low)/2)

loan_data$good <- factor(loan_data$good) 
transparentTheme(set = TRUE, trans = 0.3, pchSize = 0.5)
featurePlot(x = loan_data %>% dplyr::select(dti,fico), 
            y = loan_data$good, 
            plot = "pairs",
            auto.key = list(columns = 2))

featurePlot(x = loan_data %>% dplyr::select(dti,fico), 
            y = loan_data$good, 
            plot = "density",
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")),
            auto.key = list(columns = 2))

model_data <- loan_data %>% dplyr::select(dti,fico,good)

index <- sample(seq_along(model_data$dti),nrow(model_data)*0.75)
train <- model_data[index,]
test <- model_data[-index,]

preProcValues <- preProcess(train, method = c("center", "scale"))
train <- predict(preProcValues, train)
test <- predict(preProcValues, test)



pred <- knn(train %>% dplyr::select(fico,dti)  ,
            test%>% dplyr::select(fico,dti),
            train$good,k=5)

transparentTheme(set = TRUE, trans = 0.3, pchSize = 0.5)
featurePlot(x = test %>% dplyr::select(dti,fico), 
            y = pred, 
            plot = "pairs",
            auto.key = list(columns = 2))


table(test$good,pred)
