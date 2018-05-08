library(caret)
## Dummy Variables

library(earth)
data(etitanic)
dummies <- dummyVars(survived ~ ., data = etitanic)
head(predict(dummies, newdata = etitanic))
new_data <- predict(dummies, newdata = etitanic)


## zero or near-zero variance

data(mdrr)
table(mdrrDescr$nR11)
nzv <- nearZeroVar(mdrrDescr, saveMetrics= TRUE)
sum(nzv$zeroVar)
sum(nzv$nzv)
nzv$freqRatio
nzv[!nzv$nzv,][1:10,]

nzv <- nearZeroVar(mdrrDescr)
clean_mdrrDescr <- mdrrDescr[,-nzv]



## Correlated Predictors

descrCor <-  cor(clean_mdrrDescr)
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .999)
summary(descrCor[upper.tri(descrCor)])

descrCor <-  cor(clean_mdrrDescr)
highlyCorDescr <- findCorrelation(descrCor, cutoff = .75)
clean_mdrrDescr <- clean_mdrrDescr[,-highlyCorDescr]

## Center and Scale

set.seed(96)
inTrain <- sample(seq(along = mdrrClass), length(mdrrClass)/2)

training <- clean_mdrrDescr[inTrain,]
test <- clean_mdrrDescr[-inTrain,]


preProcValues <- preProcess(training, method = c("center", "scale"))

trainTransformed <- predict(preProcValues, training)
testTransformed <- predict(preProcValues, test)


