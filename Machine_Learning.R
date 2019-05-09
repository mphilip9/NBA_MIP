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


#Graphing some individual variables to display significance

model_difPPG <- glm(MIP_Candidate ~ dif_PPG, data = train2, family = binomial)
range(train$dif_PPG)
xdifPPG <-seq (-15, 18, 0.01)
yMIP <- predict(model_difPPG, list(dif_PPG = xdifPPG), type = "response")
train2$MIP_Candidate <- as.numeric(train2$MIP_Candidate)
plot(train$dif_PPG, train$MIP_Candidate, pch = 16, xlab = "Difference in PPG", ylab = "MIP_Candidacy")
lines(xdifPPG, yMIP, col = "red", lwd = 3)

model_difPPG <- glm(MIP_Candidate ~ dif_PPG, data = train, family = binomial)
test$MIP_Candidate <- predict(model_difPPG, newdata=test, type="response")
plot(MIP_Candidate~dif_PPG, data=train, col="red4")
lines(MIP_Candidate~dif_PPG, test, col="green4", lwd=2)

#Remove NA from variables! This is interfering with ability to properly compare models as some may be misrepresented as more accurate



#Model Creation
AwardLog <- glm(MIP_Candidate ~ dif_PPG + dif_WS + dif_OWS + dif_TSperc +dif_PER + dif_MPG +
                  dif_GS + dif_FTr + dif_BPM + dif_VORP + PPG + VORP + PER + WS + MPG + FTr + 
                  G + GS + Year + BPM + TSperc + AST + Age + dif_FGperc + dif_AST + dif_BLK +
                  dif_ThreePperc + dif_OBPM + dif_USGperc, data = train, family = binomial)
predictTest <- predict(AwardLog, newdata = test, type = "response")
testtable <- table(test$MIP_Candidate, predictTest > 0.5)
anova(AwardLog)

#Removing some insignificant values

summary(AwardLog2)
predictTest2 <- predict(AwardLog2, type = "response", newdata = test)
testtable2 <- table(test$MIP_Candidate, predict(AwardLog2, newdata = test, type= "response"))

#Model based on coefficients
library(ROCR)
library(pROC)
AwardLog2 <- glm(MIP_Candidate ~ dif_PPG  + dif_VORP + MPG + dif_OWS, data = train, family = binomial)
predictTest2 <- predict(AwardLog2, type = "response", newdata = test)
testtable2 <- table(test$MIP_Candidate, predictTest2 > 0.5)
ROCRMIP2 <- prediction(predictTest2, test$MIP_Candidate)
as.numeric(performance(ROCRMIP2, "auc")@y.values)
optim.thresh(test$MIP_Candidate, predictTest2)

#Model based solely on dif_PPG
AwardLog3 <- glm(MIP_Candidate ~ dif_PPG, data = train, family = binomial)
predictTest3 <- predict(AwardLog3, type = "response", newdata = test)
testtable3 <- table(test$MIP_Candidate, predictTest3 > 0.5)
ROCRMIP3 <- prediction(predictTest3, test$MIP_Candidate)
as.numeric(performance(ROCRMIP3, "auc")@y.values)


#Making table to represent probabilities/predictions from model to match with player names
test['MIP probability'] <- predictTest
MIPsample <- test %>% filter(`MIP probability` > 0.5) %>% select(MIP_Candidate, Player, Year, `MIP probability`, dif_PPG, dif_MPG, WS, dif_VORP, MPG, VORP, PER, Age)
MIPsample <- data.frame(MIPsample)
MIPsample %>% arrange(desc(`MIP probability`))
kable(MIPsample)#Use this function in Rmarkdown, labels etc. can be added as arguments. It works
#Confusion Matrix
library(SDMTools)
optim.thresh(test$MIP_Candidate, predictTest)#checking for optimal threshold value

fourfoldplot(testtable)#nicer visualization for the confusion matrix


#Coefficients 
Award.coef <- coef(summary(AwardLog))
Award.coef[, "Estimate"] <- exp(coef(AwardLog))
Award.coef

#Classification rate

accuracy <- sum(diag(testtable))/length(test)
accuracy


#Checking AUC for AwardLog
library(ROCR)
ROCRMIP <- prediction(predictTest, test$MIP_Candidate)
roccurve <- roc(test$MIP_Candidate ~ predictTest)
plot(roccurve)#plot of the ROC curve
as.numeric(performance(ROCRMIP, "auc")@y.values)#   97.27% is the AUC


#Random Forest Model
library(randomForest)

MIPforest <- randomForest(MIP_Candidate ~ dif_PPG + dif_WS + dif_OWS + dif_TSperc +dif_PER + dif_MPG +
                  dif_GS + dif_FTr + dif_BPM + dif_VORP + PPG + VORP + PER + WS + MPG + FTr + 
                  G + GS + Year + BPM + TSperc + AST + Age + dif_FGperc + dif_AST + dif_BLK +
                  dif_ThreePperc + dif_OBPM + dif_USGperc, data = train, importance = TRUE, na.action = na.omit)
MIPforest2 <- randomForest(MIP_Candidate ~ dif_WS + dif_OWS + dif_TSperc + dif_VORP + dif_PER + dif_BPM, data = train, importance = TRUE, na.action = na.omit)
MIPforest3 <- randomForest(MIP_Candidate ~ dif_PPG + dif_WS, data = train, importance = TRUE, na.action = na.omit)
  #predicting on training set, 100% accuracy as expected
pred1 <- predict(MIPforest, train)
confusionMatrix(pred1, train$MIP_Candidate)
pred2 <- predict(MIPforest2, test)
confusionMatrix(pred2, test$MIP_Candidate)
plot(MIPforest2)

  
  #Tune the model
tuneRF(train[])
  #predicting on testing set, not great! Worse than the logistic regression model. May need some fine tuning
predTest <- predict(MIPforest, test, type = "class")
forestTable <- table(predTest, test$MIP_Candidate)

  #Plotting ROC and AUC metric
library(pROC)
PredictionsWithProbs <- predict(MIPforest, test, type = 'prob')
auc <- auc(test$MIP_Candidate, PredictionsWithProbs[, 2])
plot(roc(test$MIP_Candidate, PredictionsWithProbs[, 2]))

  #Plotting second model ROC and AUC
PredictionsWithProbs3 <- predict(MIPforest2, test, type = 'prob')
auc <- auc(test$MIP_Candidate, PredictionsWithProbs3[, 2])
plot(roc(test$MIP_Candidate, PredictionsWithProbs3[, 2]))

  #Third model
PredictionsWithProbs4 <- predict(MIPforest3, test, type = 'prob')
auc <- auc(test$MIP_Candidate, PredictionsWithProbs4[, 2])
plot(roc(test$MIP_Candidate, PredictionsWithProbs4[, 2]))


  #Showing importance for the model of the variables, first two columns show mean raw importance score. Google the others, its complicated. Google it all really...
importance(MIPforest2)
varImpPlot(MIPforest)

  #Displaying probabilities from random forest model
test['forest probability'] <- PredictionsWithProbs


  #Using randomForestexplainer to assess variable importance
library(randomForestExplainer)
min_depth_distribution(MIPforest)




#Saving data for import to Rmarkdown
save(roccurve, file = "ROC.RData")
save(testtable, file = "testtable.RData")
save(MIPforest, file = "MIPforest.RData")
save(forestTable, file = "forestTable.RData")
save(MIPsample, file = "MIPsample.RData")


