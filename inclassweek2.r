summary(train)
train$Age[is.na(train$Age) == TRUE] <- mean(train$Age,na.rm=TRUE)
library(rpart)
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")
plot(fit)
printcp(fit)
plotcp(fit)
summary(fit)
plot(fit, uniform = TRUE, main = "Titanic Tree")
text(fit, use.n=TRUE, all=TRUE)
post(fit, file = " ", title = "Titanic Decesion Tree")
library(caret)
set.seed(998)
inTraining <- createDataPartition(train$Survived, p=.75, list=FALSE)
training <- train[inTraining,]
testing <- train[-inTraining,]

fitControl <- trainControl(method = "cv", number=10, repeats= 1,verbose=TRUE)
set.seed(825)
library(gbm)
gbmFit1 <- train(Survived ~ Pclass+Sex+Age, data=train, method= "gbm", trControl = fitControl
                 ,verbose= FALSE)
gbmFit1
gbmGrid <- expand.grid(interaction.depth = c(2,3,4), n.trees=(1:5)*200, 
                       shrinkage= c(.1,.05,.01))
gbmFit2 <- train(Survived ~ Pclass+Sex+Age, data=train, method= "gbm", trControl = fitControl,
                 verbose= FALSE, tuneGrid=gbmGrid)
gbmFit2
trellis.par.set(caretTheme())
plot(gbmFit2)

# kaggle submit
predict(gbmFit2, newdata = head(test), type = "prob")
p.hats <- predict.gbmFit2(train.gbmFit2, newdata = testData, type = "response")

survival <- vector()
for(i in 1:length(p.hats)) {
  if(p.hats[i] > .5) {
    survival[i] <- 1
  } else {
    survival[i] <- 0
  }
}
Prediction <- predict(gbmFit2, test, type = "prob")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)
