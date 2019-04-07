library(jtools)
library(ggstance)
library(caret)
smp_size <- floor(0.75 * nrow(MIP))
set.seed(123)
train_ind <- sample(seq_len(nrow(MIP)), size = smp_size)

train <- MIP[train_ind, ]
test <- MIP[-train_ind, ]

train$MIP_Candidate <- gsub("MIP Candidate", 1, train$MIP_Candidate)
train$MIP_Candidate <- gsub("Not_MIP", 0, train$MIP_Candidate)
test$MIP_Candidate <- gsub("MIP Candidate", 1, test$MIP_Candidate)
test$MIP_Candidate <- gsub("Not_MIP", 0, test$MIP_Candidate)
train$MIP_Candidate <- as.factor(train$MIP_Candidate)
test$MIP_Candidate <- as.factor(test$MIP_Candidate)
AwardLog <- glm(MIP_Candidate ~ dif_PPG + dif_WS + dif_OWS + dif_TSperc +dif_PER + dif_MPG + dif_GS + dif_FTr + dif_BPM + dif_VORP + PPG + VORP + PER + WS + MPG + FTr + 
                  G + GS + Year + BPM + TSperc + AST + Age + dif_FGperc + dif_AST + dif_BLK + dif_ThreePperc + dif_OBPM + dif_USGperc, data = train, family = binomial)
predictTest <- predict(AwardLog, type = "response", newdata = test, type = "prob")
testtable <- table(test$MIP_Candidate, predict(AwardLog, newdata = test, type= "response"))
anova(AwardLog)

#Removing some insignificant values
AwardLog2 <- glm(MIP_Candidate ~ dif_PPG  + PPG + VORP + PER + MPG  + 
                   G + BPM + WS + dif_OBPM + dif_VORP, data = train, family = binomial)
summary(AwardLog2)
predictTest2 <- predict(AwardLog2, type = "response", newdata = test, type = "prob")
testtable2 <- table(test$MIP_Candidate, predict(AwardLog2, newdata = test, type= "response"))


Coefficients 
Award.coef <- coef(summary(AwardLog))
Award.coef[, "Estimate"] <- exp(coef(AwardLog))
Award.coef

#Classification rate

accuracy <- sum(diag(testtable))/length(test)
accuracy


#Checking AUC
library(ROCR)
ROCRMIP <- prediction(predictTest, test$MIP_Candidate)
as.numeric(performance(ROCRMIP, "auc")@y.values)#   97.27% is the AUC


#Random Forest Model
library(randomForest)

MIPforest <- randomForest(MIP_Candidate ~ dif_PPG + dif_WS + dif_OWS + dif_TSperc +dif_PER + dif_MPG + dif_GS +
                            dif_FTr + dif_BPM + dif_VORP + PPG + VORP + PER + WS + MPG + FTr + 
                            G + GS + Year + BPM + TSperc + AST + Age + dif_FGperc + dif_AST + dif_BLK + dif_ThreePperc +
                            dif_OBPM + dif_USGperc, data = train, importance = TRUE)
MIPforest2 <- randomForest(MIP_Candidate ~ dif_PPG + dif_WS + dif_OWS + dif_TSperc +dif_PER + dif_MPG + dif_GS +
                            dif_FTr + dif_BPM + dif_VORP + PPG + VORP + PER + WS + MPG + FTr + 
                            G + GS + Year + BPM + TSperc + AST + Age + dif_FGperc + dif_AST + dif_BLK + dif_ThreePperc +
                            dif_OBPM + dif_USGperc, data = train, importance = TRUE)
  #predicting on training set, 100% accuracy as expected
pred1 <- predict(MIPforest2, train)
confusionMatrix(pred1, train$MIP_Candidate)
pred2 <- predict(MIPforest2, test)
confusionMatrix(pred2, test$MIP_Candidate)
plot(MIPforest2)
  #Tune the model
tuneRF(train[])
  #predicting on testing set, not great! Worse than the logistic regression model. May need some fine tuning
predTest <- predict(MIPforest2, test, type = "class")
table(predTest, test$MIP_Candidate)
  #Showing importance for the model of the variables, first two columns show mean raw importance score. Google the others, its complicated., Google it all really...
importance(MIPforest2)
varImpPlot(MIPforest2)
a=c()
i=5
for (i in 3:8) {
  model3 <- randomForest(MIP_Candidate ~ dif_PPG + dif_WS + dif_OWS + dif_TSperc +dif_PER + dif_MPG + dif_GS +
                           dif_FTr + dif_BPM + dif_VORP + PPG + VORP + PER + WS + MPG + FTr + 
                           G + GS + Year + BPM + TSperc + AST + Age + dif_FGperc + dif_AST + dif_BLK + dif_ThreePperc +
                           dif_OBPM + dif_USGperc, data = train, ntree = 500, mtry = i, importance = TRUE)
  predTrain5 <- predict(MIPforest2, train, type = "class")
  a[i-2] = mean(predTrain5 == train$MIP_Candidate)
}

a

plot(3:8,a)
