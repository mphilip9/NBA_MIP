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
                  G + GS + Year + BPM + TSperc + AST + dif_FGperc + dif_AST + dif_BLK + dif_ThreePperc + dif_OBPM + dif_USGperc, data = train, family = binomial)
#Removing some insignificant values
AwardLog2 <- glm(MIP_Candidate ~ dif_PPG  + PPG + VORP + PER + MPG  + 
                  G + BPM + TSperc, data = train, family = binomial)
summary(AwardLog2)
predictTest <- predict(AwardLog, type = "response", newdata = test, type = "prob")
testtable <- table(test$MIP_Candidate, predict(AwardLog, newdata = test, type= "response"))
anova(AwardLog)

Coefficients 
Award.coef <- coef(summary(AwardLog))
Award.coef[, "Estimate"] <- exp(coef(AwardLog))
Award.coef

#Classification rate

accuracy <- sum(diag(testtable))/length(test)
accuracy
