prop.table(table(train$site_id, train$booking_bool),1)
test$booking_bool <- 0
test$booking_bool[test$prop_starrating > 3.5 ] <- 1 #if the hotel star rating is above 4 say they booked it
aggregate(booking_bool ~ prop_starrating + prop_review_score, data=train, FUN=sum) #looking at stats
aggregate(booking_bool ~ date_time + prop_review_score, data=train, FUN=sum) #looking at stats
aggregate(booking_bool ~ prop_starrating + prop_review_score, data=train, FUN=length) #looking at stats
aggregate(booking_bool ~ prop_starrating + prop_review_score, data=train, FUN=function(x) {sum(x)/length(x)})
aggregate(booking_bool ~ visitor_hist_adr_usd + prop_review_score, data=train, FUN=function(x) {sum(x)/length(x)})
aggregate(booking_bool ~ prop_brand_bool + promotion_flag + srch_saturday_night_bool, data=train, FUN=function(x) {sum(x)/length(x)})

test$booking_bool[test$prop_starrating == 2 & test$prop_review_score > 3 ] <- 1
test$booking_bool[test$prop_starrating == 1 & test$prop_review_score > 4 ] <- 1

test$booking_bool[test$prop_review_score > 3.5 ] <- 1
test$booking_bool[test$prop_brand_bool == 1 & test$promotion_flag == 1 & test$srch_saturday_night_bool == 1 ] <- 1
test$booking_bool[test$prop_starrating < 2.5 ] <- 0

fit <- rpart(booking_bool ~ prop_starrating + prop_review_score + prop_brand_bool + promotion_flag, data=train, method="class")
plot(fit)

fancyRpartPlot(fit)
text(fit)

fit <- randomForest(as.factor(Survived) ~ prop_starrating + prop_review_score + prop_brand_bool+ promotion_flag, data=train, importance=TRUE, ntree=2000)

library(caret)
set.seed(998)
inTraining <- createDataPartition(train$booking_bool, p=.75, list=FALSE)
training <- train[inTraining,]
testing <- train[-inTraining,]

fitControl <- trainControl(method = "cv", number=10, repeats= 1,verbose=TRUE)
set.seed(825)
library(gbm)
gbmFit1 <- train(booking_bool ~ prop_review_score+prop_starrating, data=train, method= "gbm", trControl = fitControl
                 ,verbose= FALSE)
gbmFit1
gbmGrid <- expand.grid(interaction.depth = c(2,3,4), n.trees=(1:5)*200, 
                       shrinkage= c(.1,.05,.01))
gbmFit2 <- train(booking_bool ~ prop_review_score+prop_starrating, data=train, method= "gbm", trControl = fitControl,
                 verbose= FALSE, tuneGrid=gbmGrid)
gbmFit2
trellis.par.set(caretTheme())
plot(gbmFit2)
Prediction <- predict(gbmFit2, test, type = "prob")
summary(predict)

test$srch_prop_id  <- paste(test$srch_id,test$prop_id, sep = "-") # creating the srch-prop_id
submit <- data.frame("srch-prop_id" = test$srch_prop_id, booking_bool = test$booking_bool)
write.csv(submit, file = "Test_Sumbission7", row.names = FALSE)

Prediction <- predict(fit, test, type = "class")
submit <- data.frame("srch-prop_id" = test$srch_prop_id, booking_bool = Prediction)
write.csv(submit, file = "Test_Sumbission4", row.names = FALSE)
