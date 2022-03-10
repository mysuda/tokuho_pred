# Packages
library(tidyverse)
library(epiDisplay)
library(car)
library(pROC)
library(ROCR)
library(caret)
library(randomForest)
library(logistf)
library(ggplot2)
library(PRROC)
library(readr)
library(dplyr, warn.conflicts = FALSE)
library(wooldridge)
library(rmarkdown)

# 30-40 years man
## dataset
male30_40_all <- na.omit(male_40sai_30sai) 
set.seed(42)
male30_40_train <- sample_frac(male30_40_all,size=0.8)
male30_40_test <- anti_join(male30_40_all,male30_40_train)
table(male30_40_train$Mets.num.x)
table(male30_40_test$Mets.num.x)

male30_40_train$Mets.num.x <- as.factor(male30_40_train$Mets.num.x) 
male30_40_test$Mets.num.x <- as.factor(male30_40_test$Mets.num.x) 
male30_40_all$Mets.num.x <- as.factor(male30_40_all$Mets.num.x) 

## Logistic regression
logit30_40male <- glm(Mets.num.x ~ BMI.y + fukui.y + sbp.y + dbp.y + TG.y + HDL.y + LDL.y + AST_GOT.y + ALT_GPT.y + GGT_GTP.y + BS.y + UA.y + HB.y + RBC.y + HT.y + WBC.y + taijyuzoukanum.y + asanasi.num.y + syokujisokudo.num.y + nerumaesyokuji..num.y + eiyoubaransu.num.y + alcoholhindo.num.y + undou.num.y + hokou.num.y + hokousokudo.num.y + suiminkyuyou.num.y + tabaco.num.y + kaizenisi.num.y , data=male30_40_train,family=binomial(logit))
summary(logit30_40male)
exp(confint(logit30_40male))
logistic.display(logit30_40male, simplified = T)
vif(logit30_40male)

## Calculation of SPRC
cf30_40male <- coef(logit30_40male)  
male30_40_train_0 <- data.frame(lapply(male30_40_train, as.numeric)) 
sds <- sapply(male30_40_train_0, sd) 
mus <- sapply(male30_40_train_0, mean)
cf_std30_40male <- cf30_40male * c(1, c(sds[names(cf30_40male)][-1]))
cf_std30_40male[1] <- cf[1] + sum(cf30_40male[-1] * mus[names(cf30_40male)][-1])
as.data.frame(cf_std30_40male)

## Random Forest
male30_40_train$Mets.num.x <- as.factor(male30_40_train$Mets.num.x)
ctrl <- trainControl(method="repeatedcv",number=10,repeats=10) 
set.seed(43)
rf30_40male <- randomForest(Mets.num.x ~ BMI.y + fukui.y + sbp.y + dbp.y + TG.y + HDL.y + LDL.y + AST_GOT.y + ALT_GPT.y + GGT_GTP.y + BS.y + UA.y + HB.y + RBC.y + HT.y + WBC.y + taijyuzoukanum.y + asanasi.num.y + syokujisokudo.num.y + nerumaesyokuji..num.y + eiyoubaransu.num.y + alcoholhindo.num.y + undou.num.y + hokou.num.y + hokousokudo.num.y + suiminkyuyou.num.y + tabaco.num.y + kaizenisi.num.y , data=male30_40_train,proximity=T, ntree=1000,trControl=ctrl)
rf30_40male_all <- randomForest(Mets.num.x ~ BMI.y + fukui.y + sbp.y + dbp.y + TG.y + HDL.y + LDL.y + AST_GOT.y + ALT_GPT.y + GGT_GTP.y + BS.y + UA.y + HB.y + RBC.y + HT.y + WBC.y + taijyuzoukanum.y + asanasi.num.y + syokujisokudo.num.y + nerumaesyokuji..num.y + eiyoubaransu.num.y + alcoholhindo.num.y + undou.num.y + hokou.num.y + hokousokudo.num.y + suiminkyuyou.num.y + tabaco.num.y + kaizenisi.num.y , data=male30_40_all,proximity=T,importance=T, ntree=1000,trControl=ctrl)
?randomForest
shapes = c(1,16)
shapes <- shapes[as.numeric(male30_40_all$Mets.num.x)]
MDSplot(rf30_40male_all, male30_40_all$Mets.num.x)

TF30_40male_all <- rf30_40male_all$y == rf30_40male_all$predicted 
TF30_40male_all.n <- as.numeric(TF30_40male_all)
MDS.result30_40male_all<-MDSplot(rf30_40male_all, male30_40_all$Mets.num.x)
plot(MDS.result30_40male_all$points, pch = as.numeric(male30_40_all$Mets.num.x), col = TF30_40male_all.n + 2)
plot(MDS.result30_40male_all$points, pch = as.numeric(male30_40_all$Mets.num.x))
plot(MDS.result30_40msle_all$points, pch = c(16,1)) 
MDSplot(rf30_40male_all, male30_40_all$Mets.num.x, palette = c(4, 2)) 

varImpPlot(rf30_40male_all)
varImpPlot(rf30_40male_all,n.var = 10,type=1) 
varImp(rf30_40male_all)
importance(rf30_40male_all,type=1)

vi <- data.frame(variable_importance_30_40male)  
ggplot(data=vi)+
  geom_bar(mapping = aes(x = reorder(variables,variable_importance), y=variable_importance), stat="identity") +
  coord_flip() +
  xlab("Variables")+
  ylab("Variable Importance")


###cforest
library(partykit)
male30_40_train$Mets.num.x <- as.factor(male30_40_train$Mets.num.x)
ctrl <- trainControl(method="repeatedcv",number=10,repeats=10)
set.seed(43)
crf30_40male <- cforest(Mets.num.x ~ BMI.y + fukui.y + sbp.y + dbp.y + TG.y + HDL.y + LDL.y + AST_GOT.y + ALT_GPT.y + GGT_GTP.y + BS.y + UA.y + HB.y + RBC.y + HT.y + WBC.y + taijyuzoukanum.y + asanasi.num.y + syokujisokudo.num.y + nerumaesyokuji..num.y + eiyoubaransu.num.y + alcoholhindo.num.y + undou.num.y + hokou.num.y + hokousokudo.num.y + suiminkyuyou.num.y + tabaco.num.y + kaizenisi.num.y , data=male30_40_train, ntree=1000)
varimp(crf30_40male)

###cforest (conditional = T)
library(partykit)
male30_40_train$Mets.num.x <- as.factor(male30_40_train$Mets.num.x)
ctrl <- trainControl(method="repeatedcv",number=10,repeats=10)
set.seed(43)
crf30_40male <- cforest(Mets.num.x ~ BMI.y + fukui.y + sbp.y + dbp.y + TG.y + HDL.y + LDL.y + AST_GOT.y + ALT_GPT.y + GGT_GTP.y + BS.y + UA.y + HB.y + RBC.y + HT.y + WBC.y + taijyuzoukanum.y + asanasi.num.y + syokujisokudo.num.y + nerumaesyokuji..num.y + eiyoubaransu.num.y + alcoholhindo.num.y + undou.num.y + hokou.num.y + hokousokudo.num.y + suiminkyuyou.num.y + tabaco.num.y + kaizenisi.num.y , data=male30_40_train, ntree=1000)
varimp(crf30_40male,conditional = T)

## ROC Curve
### Logistic regression
logit30_40_male.prob <- predict(logit30_40male,probability=T,male30_40_test)
pred.logit30_40_male <- prediction(logit30_40_male.prob,male30_40_test$Mets.num.x)
rocObj_logit30_40_male <- performance(pred.logit30_40_male,measure="tpr", x.measure="fpr")
prObj_logit30_40_male <- performance(pred.logit30_40_male,measure="prec", x.measure="rec")
aucObj_logit30_40_male <- performance(pred.logit30_40_male,measure="auc")
aucprObj_logit30_40_male <- performance(pred.logit30_40_male,measure="aucpr")
aucObj_logit30_40male <- aucObj_logit30_40_male@y.values[[1]]
aucprObj_logit30_40male <- aucprObj_logit30_40_male@y.values[[1]]
aucObj_logit30_40male
aucprObj_logit30_40male

### Random Forest
rf30_40_male.prob <- predict(rf30_40male,type="prob",male30_40_test)
pred.rf30_40_male <- prediction(rf30_40_male.prob[,2],male30_40_test$Mets.num.x)
rocObj_rf30_40_male <- performance(pred.rf30_40_male,measure="tpr", x.measure="fpr")
prObj_rf30_40_male <- performance(pred.rf30_40_male,measure="prec", x.measure="rec")
aucObj_rf30_40_male <- performance(pred.rf30_40_male,measure="auc")
aucprObj_rf30_40_male <- performance(pred.rf30_40_male,measure="aucpr")
aucObj_rf30_40male <- aucObj_rf30_40_male@y.values[[1]]
aucprObj_rf30_40male <- aucprObj_rf30_40_male@y.values[[1]]
aucObj_rf30_40male
aucprObj_rf30_40male

### cForest (conditinal = T) 
crft30_40_male.prob <- predict(crf30_40male,type="prob",male30_40_test)
pred.crft30_40_male <- prediction(crft30_40_male.prob[,2],male30_40_test$Mets.num.x)
rocObj_crft30_40_male <- performance(pred.crft30_40_male,measure="tpr", x.measure="fpr")
prObj_crft30_40_male <- performance(pred.crft30_40_male,measure="prec", x.measure="rec")
aucObj_crft30_40_male <- performance(pred.crft30_40_male,measure="auc")
aucprObj_crft30_40_male <- performance(pred.crft30_40_male,measure="aucpr")
aucObj_crft30_40male <- aucObj_crft30_40_male@y.values[[1]]
aucprObj_crft30_40male <- aucprObj_crft30_40_male@y.values[[1]]
aucObj_crft30_40male
aucprObj_crft30_40male

## ROC drawing 
plot(rocObj_logit30_40_male,main="30_40male", col="red")
par(new=T)
y <- function(x)
  plot(y,0,1,xlab="",ylab="")
legend("bottomright",legend=c("Logistic Regression","Random Forest","cForest"),col=c("red","blue","green"),lty=c(1))
par(new=T)
plot(rocObj_rf30_40_male, col="blue")
par(new=T)
plot(rocObj_crft30_40_male, col="green")

# AUC test
library(pROC)
rf_roc <- roc(male30_40_test$Mets.num.x,rf30_40_male.prob[,2])
mr_roc <- roc(male30_40_test$Mets.num.x,logit30_40_male.prob)
roc.test(rf_roc,mr_roc)

rf_pr <- roc(male30_40_test$Mets.num.x,rf30_40_male.prob[,2])
mr_pr <- roc(male30_40_test$Mets.num.x,logit30_40_male.prob)
roc.test(rf_roc,mr_roc)

# ROC Table for searching optimal point
fn_ROC_Table <- function(pred, obs){
  
  pred1 <- prediction(predictions=pred, labels=obs)
  perf1 <- performance(pred1, measure="tpr", x.measure="fpr")
  
  ROCtable <- data.frame(
    Cutoff=unlist(pred1@cutoffs), 
    TP=unlist(pred1@tp), FP=unlist(pred1@fp),
    FN=unlist(pred1@fn), TN=unlist(pred1@tn),
    TPR=unlist(perf1@y.values), FPR=unlist(perf1@x.values),
    diff_TPR_FPR=unlist(perf1@y.values)-unlist(perf1@x.values),
    MissClasR=(unlist(pred1@fp)+unlist(pred1@fn))/
      (unlist(pred1@tp)+unlist(pred1@fp)+unlist(pred1@fn)+unlist(pred1@tn))
  )
  
  return(ROCtable)
}

ROCtable_p1 <- fn_ROC_Table(rf30_40_male.prob[,2],male30_40_test$Mets.num.x) 
for(i in 1:nrow(ROCtable_p1)){
  mx <- max(ROCtable_p1$diff_TPR_FPR) # max value of TPR-FPR
  if(ROCtable_p1$diff_TPR_FPR[i]==mx){ print(ROCtable_p1[i,]) }
}

ROCtable_p2 <- fn_ROC_Table(logit30_40_male.prob,male30_40_test$Mets.num.x) 
for(i in 1:nrow(ROCtable_p2)){
  mx <- max(ROCtable_p2$diff_TPR_FPR) # max value of TPR-FPR
  if(ROCtable_p2$diff_TPR_FPR[i]==mx){ print(ROCtable_p2[i,]) }
}


# 30-40 years women
## dataset
female30_40_all <- na.omit(female_40sai_30sai) 
female30_40_all$Mets.num.x <- as.factor(female30_40_all$Mets.num.x)
set.seed(44)
female30_40_train <- sample_frac(female30_40_all,size=0.5)
female30_40_test <- anti_join(female30_40_all,female30_40_train)
table(female30_40_train$Mets.num.x)
table(female30_40_test$Mets.num.x)
female30_40_train$Mets.num.x <- as.factor(female30_40_train$Mets.num.x) 
female30_40_test$Mets.num.x <- as.factor(female30_40_test$Mets.num.x)
female30_40_all$Mets.num.x <- as.factor(female30_40_all$Mets.num.x) 

## SMOTE (not doing in this study)
library(DMwR)
female30_40_train_smote <- SMOTE(Mets.num.x ~ ., data = data.frame(female30_40_train),k=2,perc.over=2400,perc.under=1584)
female30_40_test_smote <- SMOTE(Mets.num.x ~ ., data = data.frame(female30_40_test),k=2,perc.over=800,perc.under=1575)
table(female30_40_train_smote$Mets.num.x)
table(female30_40_test_smote$Mets.num.x)

## Logistic Regression
logit30_40female <- glm(Mets.num.x ~ BMI.y + fukui.y + sbp.y + dbp.y + TG.y + HDL.y + LDL.y + AST_GOT.y + ALT_GPT.y + GGT_GTP.y + BS.y + UA.y + HB.y + RBC.y + HT.y + WBC.y + taijyuzoukanum.y + asanasi.num.y + syokujisokudo.num.y + nerumaesyokuji..num.y + eiyoubaransu.num.y + alcoholhindo.num.y + undou.num.y + hokou.num.y + hokousokudo.num.y + suiminkyuyou.num.y + tabaco.num.y + kaizenisi.num.y, data=female30_40_train, family=binomial(logit))
summary(logit30_40female)
logistic.display(logit30_40female, simplified = T)
vif(logit30_40female)

### Firth 
install.packages("brglm")
library(brglm)
brglm_model<- brglm(Mets.num.x ~ BMI.y + fukui.y + sbp.y + dbp.y + TG.y + HDL.y + LDL.y + AST_GOT.y + ALT_GPT.y + GGT_GTP.y + BS.y + UA.y + HB.y + RBC.y + HT.y + WBC.y + taijyuzoukanum.y + asanasi.num.y + syokujisokudo.num.y + nerumaesyokuji..num.y + eiyoubaransu.num.y + alcoholhindo.num.y + undou.num.y + hokou.num.y + hokousokudo.num.y + suiminkyuyou.num.y + tabaco.num.y + kaizenisi.num.y, data=female30_40_train , family = "binomial")
summary(brglm_model)
vif(brglm_model)

## Calculation of SPRC
cf30_40female <- coef(logit30_40female)  
female30_40_train_0 <- data.frame(lapply(female30_40_train, as.numeric))
sds <- sapply(female30_40_train_0, sd) 
mus <- sapply(female30_40_train_0, mean)
cf_std30_40female <- cf30_40female * c(1, c(sds[names(cf30_40female)][-1]))
cf_std30_40female[1] <- cf[1] + sum(cf30_40female[-1] * mus[names(cf30_40female)][-1])
as.data.frame(cf_std30_40female)

## SPRC for Firth
cf30_40female <- coef(brglm_model)  
female30_40_train_0 <- data.frame(lapply(female30_40_train, as.numeric)) 
sds <- sapply(female30_40_train_0, sd) 
mus <- sapply(female30_40_train_0, mean)
cf_std30_40female <- cf30_40female * c(1, c(sds[names(cf30_40female)][-1]))
cf_std30_40female[1] <- cf[1] + sum(cf30_40female[-1] * mus[names(cf30_40female)][-1])
as.data.frame(cf_std30_40female)

## Random Forest
ctrl <- trainControl(method="repeatedcv",number=10,repeats=10)
set.seed(43)
rf30_40female <- randomForest(Mets.num.x ~ BMI.y + fukui.y + sbp.y + dbp.y + TG.y + HDL.y + LDL.y + AST_GOT.y + ALT_GPT.y + GGT_GTP.y + BS.y + UA.y + HB.y + RBC.y + HT.y + WBC.y + taijyuzoukanum.y + asanasi.num.y + syokujisokudo.num.y + nerumaesyokuji..num.y + eiyoubaransu.num.y + alcoholhindo.num.y + undou.num.y + hokou.num.y + hokousokudo.num.y + suiminkyuyou.num.y + tabaco.num.y + kaizenisi.num.y , data=female30_40_train,proximity=T,ntree=1000,trControl=ctrl)
rf30_40female_all <- randomForest(Mets.num.x ~ BMI.y + fukui.y + sbp.y + dbp.y + TG.y + HDL.y + LDL.y + AST_GOT.y + ALT_GPT.y + GGT_GTP.y + BS.y + UA.y + HB.y + RBC.y + HT.y + WBC.y + taijyuzoukanum.y + asanasi.num.y + syokujisokudo.num.y + nerumaesyokuji..num.y + eiyoubaransu.num.y + alcoholhindo.num.y + undou.num.y + hokou.num.y + hokousokudo.num.y + suiminkyuyou.num.y + tabaco.num.y + kaizenisi.num.y , data=female30_40_all,proximity=T,ntree=1000,trControl=ctrl)
MDSplot(rf30_40female_all, female30_40_all$Mets.num.x)

TF30_40female_all <- rf30_40female_all$y == rf30_40female_all$predicted 
TF30_40female_all.n <- as.numeric(TF30_40female_all)
MDS.result30_40female_all<-MDSplot(rf30_40female_all, female30_40_all$Mets.num.x)
plot(MDS.result30_40female_all$points, pch = as.numeric(female30_40_all$Mets.num.x), col = TF30_40female_all.n + 2) 
plot(MDS.result30_40female_all$points, pch = c(16,1))  
MDSplot(rf30_40female_all, female30_40_all$Mets.num.x, palette = c(4, 2)) #4青　2赤

varImpPlot(rf30_40female_all)
varImpPlot(rf30_40female_all,n.var = 10) 
varImp(rf30_40female_all)

vi <- data.frame(variable_importance_30_40female)  
ggplot(data=vi)+
  geom_bar(mapping = aes(x = reorder(variables,variable_importance), y=variable_importance), stat="identity") +
  coord_flip() +
  xlab("Variables")+
  ylab("Variable Importance")

### cforest
library(partykit)
female30_40_train$Mets.num.x <- as.factor(female30_40_train$Mets.num.x)
ctrl <- trainControl(method="repeatedcv",number=10,repeats=10)
set.seed(43)
crf30_40female <- cforest(Mets.num.x ~ BMI.y + fukui.y + sbp.y + dbp.y + TG.y + HDL.y + LDL.y + AST_GOT.y + ALT_GPT.y + GGT_GTP.y + BS.y + UA.y + HB.y + RBC.y + HT.y + WBC.y + taijyuzoukanum.y + asanasi.num.y + syokujisokudo.num.y + nerumaesyokuji..num.y + eiyoubaransu.num.y + alcoholhindo.num.y + undou.num.y + hokou.num.y + hokousokudo.num.y + suiminkyuyou.num.y + tabaco.num.y + kaizenisi.num.y , data=female30_40_train, ntree=1000)
varimp(crf30_40female)

### cforest (conditional = T)
library(partykit)
female30_40_train$Mets.num.x <- as.factor(female30_40_train$Mets.num.x)
ctrl <- trainControl(method="repeatedcv",number=10,repeats=10)
set.seed(43)
crf30_40female <- cforest(Mets.num.x ~ BMI.y + fukui.y + sbp.y + dbp.y + TG.y + HDL.y + LDL.y + AST_GOT.y + ALT_GPT.y + GGT_GTP.y + BS.y + UA.y + HB.y + RBC.y + HT.y + WBC.y + taijyuzoukanum.y + asanasi.num.y + syokujisokudo.num.y + nerumaesyokuji..num.y + eiyoubaransu.num.y + alcoholhindo.num.y + undou.num.y + hokou.num.y + hokousokudo.num.y + suiminkyuyou.num.y + tabaco.num.y + kaizenisi.num.y , data=female30_40_train, ntree=1000)
varimp(crf30_40female,conditional = T)

##　ROC curve
## Logistic Regression
logit30_40_female.prob <- predict(logit30_40female,probability=T,female30_40_test)
pred.logit30_40_female <- prediction(logit30_40_female.prob,female30_40_test$Mets.num.x)
rocObj_logit30_40_female <- performance(pred.logit30_40_female,measure="tpr", x.measure="fpr")
prObj_logit30_40_female <- performance(pred.logit30_40_female,measure="prec", x.measure="rec")
aucObj_logit30_40_female <- performance(pred.logit30_40_female,measure="auc")
aucprObj_logit30_40_female <- performance(pred.logit30_40_female,measure="aucpr")
aucObj_logit30_40 <- aucObj_logit30_40_female@y.values[[1]]
aucprObj_logit30_40female <- aucprObj_logit30_40_female@y.values[[1]]
aucObj_logit30_40
aucprObj_logit30_40female


## Logistic Regression with Firth
logit30_40_female.prob <- predict(brglm_model,probability=T,female30_40_test)
pred.logit30_40_female <- prediction(logit30_40_female.prob,female30_40_test$Mets.num.x)
rocObj_logit30_40_female <- performance(pred.logit30_40_female,measure="tpr", x.measure="fpr")
prObj_logit30_40_female <- performance(pred.logit30_40_female,measure="prec", x.measure="rec")
aucObj_logit30_40_female <- performance(pred.logit30_40_female,measure="auc")
aucObj_logit30_40 <- aucObj_logit30_40_female@y.values[[1]]
aucprObj_logit30_40female <- aucprObj_logit30_40_female@y.values[[1]]
aucObj_logit30_40
aucprObj_logit30_40female


## Random Forest
rf30_40_female.prob <- predict(rf30_40female,type="prob",female30_40_test)
pred.rf30_40_female <- prediction(rf30_40_female.prob[,2],female30_40_test$Mets.num.x)
rocObj_rf30_40_female <- performance(pred.rf30_40_female,measure="tpr", x.measure="fpr")
prObj_rf30_40_female <- performance(pred.rf30_40_female,measure="prec", x.measure="rec")
aucObj_rf30_40_female <- performance(pred.rf30_40_female,measure="auc")
aucprObj_rf30_40_female <- performance(pred.rf30_40_female,measure="aucpr")
aucObj_rf30_40female <- aucObj_rf30_40_female@y.values[[1]]
aucprObj_rf30_40female <- aucprObj_rf30_40_female@y.values[[1]]
aucObj_rf30_40female
aucprObj_rf30_40female

## ROC drawing
plot(rocObj_logit30_40_female,main="30_40female", col="red")
par(new=T)
y <- function(x)
  plot(y,0,1,xlab="",ylab="")
legend("bottomright",legend=c("Logistic female Regression","Random Forest"),col=c("red","blue"),lty=c(1))
par(new=T)
plot(rocObj_rf30_40_female, col="blue")

## PR drawing
plot(prObj_logit30_40_female,main="30_40female", col="blue",ylim=c(0,1))
par(new=T)
y <- function(x)
  plot(y,0,1,xlab="",ylab="")
legend("topright",legend=c("Random Forest","Logistic Regression"),col=c("red","blue"),lty=c(1))
par(new=T)
plot(prObj_rf30_40_female, col="red",ylim=c(0,1))

## AUC test
library(pROC)
rf_roc <- roc(female30_40_test$Mets.num.x,rf30_40_female.prob[,2])
mr_roc <- roc(female30_40_test$Mets.num.x,logit30_40_female.prob)
roc.test(rf_roc,mr_roc)

## ROC Table for searching optimal point
fn_ROC_Table <- function(pred, obs){
  
  pred1 <- prediction(predictions=pred, labels=obs)
  perf1 <- performance(pred1, measure="tpr", x.measure="fpr")
  
  ROCtable <- data.frame(
    Cutoff=unlist(pred1@cutoffs), 
    TP=unlist(pred1@tp), FP=unlist(pred1@fp),
    FN=unlist(pred1@fn), TN=unlist(pred1@tn),
    TPR=unlist(perf1@y.values), FPR=unlist(perf1@x.values),
    diff_TPR_FPR=unlist(perf1@y.values)-unlist(perf1@x.values),
    MissClasR=(unlist(pred1@fp)+unlist(pred1@fn))/
      (unlist(pred1@tp)+unlist(pred1@fp)+unlist(pred1@fn)+unlist(pred1@tn))
  )
  
  return(ROCtable)
}

ROCtable_p1 <- fn_ROC_Table(rf30_40_female.prob[,2],female30_40_test$Mets.num.x)
for(i in 1:nrow(ROCtable_p1)){
  mx <- max(ROCtable_p1$diff_TPR_FPR) # max value of TPR-FPR
  if(ROCtable_p1$diff_TPR_FPR[i]==mx){ print(ROCtable_p1[i,]) }
}

ROCtable_p2 <- fn_ROC_Table(logit30_40_female.prob,female30_40_test$Mets.num.x) 
for(i in 1:nrow(ROCtable_p2)){
  mx <- max(ROCtable_p2$diff_TPR_FPR) # max value of TPR-FPR
  if(ROCtable_p2$diff_TPR_FPR[i]==mx){ print(ROCtable_p2[i,]) }
}

# 35-40 year man
## dataset
male35_40_all <- na.omit(male_40sai_35sai) 
set.seed(42)
male35_40_train <- sample_frac(male35_40_all,size=0.8)
male35_40_test <- anti_join(male35_40_all,male35_40_train)

male35_40_train$Mets.num.x <- as.factor(male35_40_train$Mets.num.x) 
male35_40_test$Mets.num.x <- as.factor(male35_40_test$Mets.num.x) 
male35_40_all$Mets.num.x <- as.factor(male35_40_all$Mets.num.x)

table(male35_40_all$Mets.num.x)

## Logistic Regression
logit35_40male <- glm(Mets.num.x ~ BMI.y + fukui.y + sbp.y + dbp.y + TG.y + HDL.y + LDL.y + AST_GOT.y + ALT_GPT.y + GGT_GTP.y + BS.y + UA.y + HB.y + RBC.y + HT.y + WBC.y + taijyuzoukanum.y + asanasi.num.y + syokujisokudo.num.y + nerumaesyokuji..num.y + eiyoubaransu.num.y + alcoholhindo.num.y + undou.num.y + hokou.num.y + hokousokudo.num.y + suiminkyuyou.num.y + tabaco.num.y + kaizenisi.num.y , data=male35_40_train,family=binomial(logit))
summary(logit35_40male)
exp(confint(logit35_40male))
logistic.display(logit35_40male, simplified = T)
vif(logit35_40male)

## Calculation of SPRC
cf35_40male <- coef(logit35_40male)  
male35_40_train_0 <- data.frame(lapply(male35_40_train, as.numeric)) 
sds <- sapply(male35_40_train_0, sd) 
mus <- sapply(male35_40_train_0, mean)
cf_std35_40male <- cf35_40male * c(1, c(sds[names(cf35_40male)][-1]))
cf_std35_40male[1] <- cf[1] + sum(cf35_40male[-1] * mus[names(cf35_40male)][-1])
as.data.frame(cf_std35_40male)

## Random Forest
male35_40_train$Mets.num.x <- as.factor(male35_40_train$Mets.num.x)
ctrl <- trainControl(method="repeatedcv",number=10,repeats=10)
set.seed(43)
rf35_40male <- randomForest(Mets.num.x ~ BMI.y + fukui.y + sbp.y + dbp.y + TG.y + HDL.y + LDL.y + AST_GOT.y + ALT_GPT.y + GGT_GTP.y + BS.y + UA.y + HB.y + RBC.y + HT.y + WBC.y + taijyuzoukanum.y + asanasi.num.y + syokujisokudo.num.y + nerumaesyokuji..num.y + eiyoubaransu.num.y + alcoholhindo.num.y + undou.num.y + hokou.num.y + hokousokudo.num.y + suiminkyuyou.num.y + tabaco.num.y + kaizenisi.num.y , data=male35_40_train,proximity=T, ntree=1000,trControl=ctrl)
rf35_40male_all <- randomForest(Mets.num.x ~ BMI.y + fukui.y + sbp.y + dbp.y + TG.y + HDL.y + LDL.y + AST_GOT.y + ALT_GPT.y + GGT_GTP.y + BS.y + UA.y + HB.y + RBC.y + HT.y + WBC.y + taijyuzoukanum.y + asanasi.num.y + syokujisokudo.num.y + nerumaesyokuji..num.y + eiyoubaransu.num.y + alcoholhindo.num.y + undou.num.y + hokou.num.y + hokousokudo.num.y + suiminkyuyou.num.y + tabaco.num.y + kaizenisi.num.y , data=male35_40_all,proximity=T, ntree=1000,trControl=ctrl)
MDSplot(rf35_40male_all, male35_40_all$Mets.num.x)
TF35_40male_all <- rf35_40male_all$y == rf35_40male_all$predicted 
TF35_40male_all.n <- as.numeric(TF35_40male_all)
MDS.result35_40male_all<-MDSplot(rf35_40male_all, male35_40_all$Mets.num.x)
plot(MDS.result35_40male_all$points, pch = as.numeric(male35_40_all$Mets.num.x), col = TF35_40male_all.n + 2)
plot(MDS.result35_40male_all$points, pch = c(16,1))  
MDSplot(rf35_40male_all, male35_40_all$Mets.num.x, palette = c(4, 2)) 

varImpPlot(rf35_40_all)
varImpPlot(rf35_40male,n.var = 10)
varImp(rf35_40male_all)

vi <- data.frame(variable_importance_35_40male) 
ggplot(data=vi)+
  geom_bar(mapping = aes(x = reorder(variables,variable_importance), y=variable_importance), stat="identity") +
  coord_flip() +
  xlab("Variables")+
  ylab("Variable Importance")

### cforest
library(partykit)
male35_40_train$Mets.num.x <- as.factor(male35_40_train$Mets.num.x)
ctrl <- trainControl(method="repeatedcv",number=10,repeats=10)
set.seed(43)
crf35_40male <- cforest(Mets.num.x ~ BMI.y + fukui.y + sbp.y + dbp.y + TG.y + HDL.y + LDL.y + AST_GOT.y + ALT_GPT.y + GGT_GTP.y + BS.y + UA.y + HB.y + RBC.y + HT.y + WBC.y + taijyuzoukanum.y + asanasi.num.y + syokujisokudo.num.y + nerumaesyokuji..num.y + eiyoubaransu.num.y + alcoholhindo.num.y + undou.num.y + hokou.num.y + hokousokudo.num.y + suiminkyuyou.num.y + tabaco.num.y + kaizenisi.num.y , data=male35_40_train, ntree=1000)
varimp(crf35_40male)

### cforest (conditional = T)
library(partykit)
female35_40_train$Mets.num.x <- as.factor(female35_40_train$Mets.num.x)
ctrl <- trainControl(method="repeatedcv",number=10,repeats=10)
set.seed(43)
crf35_40female <- cforest(Mets.num.x ~ BMI.y + fukui.y + sbp.y + dbp.y + TG.y + HDL.y + LDL.y + AST_GOT.y + ALT_GPT.y + GGT_GTP.y + BS.y + UA.y + HB.y + RBC.y + HT.y + WBC.y + taijyuzoukanum.y + asanasi.num.y + syokujisokudo.num.y + nerumaesyokuji..num.y + eiyoubaransu.num.y + alcoholhindo.num.y + undou.num.y + hokou.num.y + hokousokudo.num.y + suiminkyuyou.num.y + tabaco.num.y + kaizenisi.num.y , data=female35_40_train, ntree=1000)
varimp(crf35_40female,conditional = T)


## ROC curve
## Logistic Regression
logit35_40_male.prob <- predict(logit35_40male,probability=T,male35_40_test)
pred.logit35_40_male <- prediction(logit35_40_male.prob,male35_40_test$Mets.num.x)
rocObj_logit35_40_male <- performance(pred.logit35_40_male,measure="tpr", x.measure="fpr")
prObj_logit35_40_male <- performance(pred.logit35_40_male,measure="prec", x.measure="rec")
aucObj_logit35_40_male <- performance(pred.logit35_40_male,measure="auc")
aucprObj_logit35_40_male <- performance(pred.logit35_40_male,measure="aucpr")
aucObj_logit35_40male <- aucObj_logit35_40_male@y.values[[1]]
aucprObj_logit35_40male <- aucprObj_logit35_40_male@y.values[[1]]
aucObj_logit35_40male
aucprObj_logit35_40male


## Random Forest
rf35_40_male.prob <- predict(rf35_40male,type="prob",male35_40_test)
pred.rf35_40_male <- prediction(rf35_40_male.prob[,2],male35_40_test$Mets.num.x)
rocObj_rf35_40_male <- performance(pred.rf35_40_male,measure="tpr", x.measure="fpr")
prObj_rf35_40_male <- performance(pred.rf35_40_male,measure="prec", x.measure="rec")
aucObj_rf35_40_male <- performance(pred.rf35_40_male,measure="auc")
aucprObj_rf35_40_male <- performance(pred.rf35_40_male,measure="aucpr")
aucObj_rf35_40male <- aucObj_rf35_40_male@y.values[[1]]
aucprObj_rf35_40male <- aucprObj_rf35_40_male@y.values[[1]]
aucObj_rf35_40male
aucprObj_rf35_40male


## ROC drawing
plot(rocObj_logit35_40_male,main="35_40male", col="red")
par(new=T)
y <- function(x)
  plot(y,0,1,xlab="",ylab="")
legend("bottomright",legend=c("Logistic Regression","Random Forest"),col=c("red","blue"),lty=c(1))
par(new=T)
plot(rocObj_rf35_40_male, col="blue")

## PR drawing
plot(prObj_logit35_40_male,main="35_40male", col="blue")
par(new=T)
y <- function(x)
  plot(y,0,1,xlab="",ylab="")
legend("topright",legend=c("Random Forest","Logistic Regression"),col=c("red","blue"),lty=c(1))
par(new=T)
plot(prObj_rf35_40_male, col="red")

# AUC test
library(pROC)
rf_roc <- roc(male35_40_test$Mets.num.x,rf35_40_male.prob[,2])
mr_roc <- roc(male35_40_test$Mets.num.x,logit35_40_male.prob)
roc.test(rf_roc,mr_roc)

# ROC Table
fn_ROC_Table <- function(pred, obs){
  
  pred1 <- prediction(predictions=pred, labels=obs)
  perf1 <- performance(pred1, measure="tpr", x.measure="fpr")
  
  ROCtable <- data.frame(
    Cutoff=unlist(pred1@cutoffs), 
    TP=unlist(pred1@tp), FP=unlist(pred1@fp),
    FN=unlist(pred1@fn), TN=unlist(pred1@tn),
    TPR=unlist(perf1@y.values), FPR=unlist(perf1@x.values),
    diff_TPR_FPR=unlist(perf1@y.values)-unlist(perf1@x.values),
    MissClasR=(unlist(pred1@fp)+unlist(pred1@fn))/
      (unlist(pred1@tp)+unlist(pred1@fp)+unlist(pred1@fn)+unlist(pred1@tn))
  )
  
  return(ROCtable)
}

ROCtable_p1 <- fn_ROC_Table(rf35_40_male.prob[,2],male35_40_test$Mets.num.x) 
for(i in 1:nrow(ROCtable_p1)){
  mx <- max(ROCtable_p1$diff_TPR_FPR) # max value of TPR-FPR
  if(ROCtable_p1$diff_TPR_FPR[i]==mx){ print(ROCtable_p1[i,]) }
}

ROCtable_p2 <- fn_ROC_Table(logit35_40_male.prob,male35_40_test$Mets.num.x)
for(i in 1:nrow(ROCtable_p2)){
  mx <- max(ROCtable_p2$diff_TPR_FPR) # max value of TPR-FPR
  if(ROCtable_p2$diff_TPR_FPR[i]==mx){ print(ROCtable_p2[i,]) }
}


# 35-40 women
## dataset 
female35_40_all <- na.omit(female_40sai_35sai) 
set.seed(420)
female35_40_train <- sample_frac(female35_40_all,size=0.5)
female35_40_test <- anti_join(female35_40_all,female35_40_train)
table(female35_40_train$Mets.num.x)
table(female35_40_test$Mets.num.x)

female35_40_train$Mets.num.x <- as.factor(female35_40_train$Mets.num.x) 
female35_40_test$Mets.num.x <- as.factor(female35_40_test$Mets.num.x)
female35_40_all$Mets.num.x <- as.factor(female35_40_all$Mets.num.x) 

## SMOTE (not doing in this study)
library(DMwR)
table(female35_40_all$Mets.num.x)
female35_40_all$Mets.num.x <- as.factor(female35_40_all$Mets.num.x) 
female35_40_all_smote <- SMOTE(Mets.num.x ~ ., data = data.frame(female35_40_all),k=3,perc.over=1500,perc.under=1825)
table(female35_40_all_smote$Mets.num.x)
set.seed(43)
female35_40_test <- sample_frac(female35_40_all_smote,size=0.2)
female35_40_train <- anti_join(female35_40_all_smote,female35_40_test)
table(female35_40_train$Mets.num.x)
table(female35_40_test$Mets.num.x)
female35_40_train_smote <- SMOTE(Mets.num.x ~ ., data = data.frame(female35_40_train),k=3,perc.over=2400,perc.under=1584)
female35_40_test_smote <- SMOTE(Mets.num.x ~ ., data = data.frame(female35_40_test),k=2,perc.over=800,perc.under=1575)
table(female35_40_train_smote$Mets.num.x)
table(female35_40_test_smote$Mets.num.x)

## Logistic Regression
logit35_40female <- glm(Mets.num.x ~ BMI.y + fukui.y + sbp.y + dbp.y + TG.y + HDL.y + LDL.y + AST_GOT.y + ALT_GPT.y + GGT_GTP.y + BS.y + UA.y + HB.y + RBC.y + HT.y + WBC.y + taijyuzoukanum.y + asanasi.num.y + syokujisokudo.num.y + nerumaesyokuji..num.y + eiyoubaransu.num.y + alcoholhindo.num.y + undou.num.y + hokou.num.y + hokousokudo.num.y + suiminkyuyou.num.y + tabaco.num.y + kaizenisi.num.y , data=female35_40_train,family=binomial(logit))
summary(logit35_40female)
exp(confint(logit35_40female))
logistic.display(logit35_40female, simplified = T)
vif(logit35_40female)

## Logistic Regression with Firth
install.packages("brglm")
library(brglm)
brglm_model<- brglm(Mets.num.x ~ BMI.y + fukui.y + sbp.y + dbp.y + TG.y + HDL.y + LDL.y + AST_GOT.y + ALT_GPT.y + GGT_GTP.y + BS.y + UA.y + HB.y + RBC.y + HT.y + WBC.y + taijyuzoukanum.y + asanasi.num.y + syokujisokudo.num.y + nerumaesyokuji..num.y + eiyoubaransu.num.y + alcoholhindo.num.y + undou.num.y + hokou.num.y + hokousokudo.num.y + suiminkyuyou.num.y + tabaco.num.y + kaizenisi.num.y, data=female35_40_train , family = "binomial")
summary(brglm_model)
vif(brglm_model)

## Calculation of SPRC
cf35_40female <- coef(logit35_40female)  
female35_40_train_0 <- data.frame(lapply(female35_40_train, as.numeric)) 
sds <- sapply(female35_40_train_0, sd) 
mus <- sapply(female35_40_train_0, mean)
cf_std35_40female <- cf35_40female * c(1, c(sds[names(cf35_40female)][-1]))
cf_std35_40female[1] <- cf[1] + sum(cf35_40female[-1] * mus[names(cf35_40female)][-1])
as.data.frame(cf_std35_40female)


## Random Forest
ctrl <- trainControl(method="repeatedcv",number=10,repeats=10)
set.seed(43)
rf35_40female <- randomForest(Mets.num.x ~ BMI.y + fukui.y + sbp.y + dbp.y + TG.y + HDL.y + LDL.y + AST_GOT.y + ALT_GPT.y + GGT_GTP.y + BS.y + UA.y + HB.y + RBC.y + HT.y + WBC.y + taijyuzoukanum.y + asanasi.num.y + syokujisokudo.num.y + nerumaesyokuji..num.y + eiyoubaransu.num.y + alcoholhindo.num.y + undou.num.y + hokou.num.y + hokousokudo.num.y + suiminkyuyou.num.y + tabaco.num.y + kaizenisi.num.y , data=female35_40_train,proximity=T,ntree=1000,trControl=ctrl)
rf35_40female_all <- randomForest(Mets.num.x ~ BMI.y + fukui.y + sbp.y + dbp.y + TG.y + HDL.y + LDL.y + AST_GOT.y + ALT_GPT.y + GGT_GTP.y + BS.y + UA.y + HB.y + RBC.y + HT.y + WBC.y + taijyuzoukanum.y + asanasi.num.y + syokujisokudo.num.y + nerumaesyokuji..num.y + eiyoubaransu.num.y + alcoholhindo.num.y + undou.num.y + hokou.num.y + hokousokudo.num.y + suiminkyuyou.num.y + tabaco.num.y + kaizenisi.num.y , data=female35_40_all,proximity=T, ntree=1000,trControl=ctrl)
MDSplot(rf35_40female_all, female35_40_all$Mets.num.x)

TF35_40female_all <- rf35_40female_all$y == rf35_40female_all$predicted
TF35_40female_all.n <- as.numeric(TF35_40female_all)
MDS.result35_40female_all<-MDSplot(rf35_40female_all, female35_40_all$Mets.num.x)
plot(MDS.result35_40female_all$points, pch = as.numeric(female35_40_all$Mets.num.x), col = TF35_40female_all.n + 2)
plot(MDS.result35_40female_all$points, pch = c(16,1))  
MDSplot(rf35_40female_all, female35_40_all$Mets.num.x, palette = c(4, 2)) 


varImpPlot(rf35_40female_all)
varImpPlot(rf35_40female_all,n.var = 10)
varImp(rf35_40female_all)

vi <- data.frame(variable_importance_35_40female)  
ggplot(data=vi)+
  geom_bar(mapping = aes(x = reorder(variables,variable_importance), y=variable_importance), stat="identity") +
  coord_flip() +
  xlab("Variables")+
  ylab("Variable Importance")

table(female35_40_all$Mets.num.x)

### cforest
install.packages("partykit")
library(partykit)
female35_40_train$Mets.num.x <- as.factor(female35_40_train$Mets.num.x)
ctrl <- trainControl(method="repeatedcv",number=10,repeats=10)
set.seed(43)
crf35_40female <- cforest(Mets.num.x ~ BMI.y + fukui.y + sbp.y + dbp.y + TG.y + HDL.y + LDL.y + AST_GOT.y + ALT_GPT.y + GGT_GTP.y + BS.y + UA.y + HB.y + RBC.y + HT.y + WBC.y + taijyuzoukanum.y + asanasi.num.y + syokujisokudo.num.y + nerumaesyokuji..num.y + eiyoubaransu.num.y + alcoholhindo.num.y + undou.num.y + hokou.num.y + hokousokudo.num.y + suiminkyuyou.num.y + tabaco.num.y + kaizenisi.num.y , data=female35_40_train, ntree=1000)
varimp(crf35_40female)
create_crfplot <- function(rf, conditional = TRUE){
  
  imp <- rf %>%
    varimp(conditional = conditional) %>% 
    as_tibble() %>% 
    rownames_to_column("Feature") %>% 
    rename(Importance = value)
  
  p <- ggplot(imp, aes(x = reorder(Feature, Importance), y = Importance)) +
    geom_bar(stat = "identity", fill = "#53cfff", width = 0.65) +
    coord_flip() + 
    theme_light(base_size = 20) +
    theme(axis.title.x = element_text(size = 15, color = "black"),
          axis.title.y = element_blank(),
          axis.text.x  = element_text(size = 15, color = "black"),
          axis.text.y  = element_text(size = 15, color = "black")) 
  return(p)
}
create_crfplot(crf35_40female)

### cforest (conditional = T)
install.packages("partykit")
library(partykit)
female35_40_train$Mets.num.x <- as.factor(female35_40_train$Mets.num.x)
ctrl <- trainControl(method="repeatedcv",number=10,repeats=10)
set.seed(43)
crf35_40female <- cforest(Mets.num.x ~ BMI.y + fukui.y + sbp.y + dbp.y + TG.y + HDL.y + LDL.y + AST_GOT.y + ALT_GPT.y + GGT_GTP.y + BS.y + UA.y + HB.y + RBC.y + HT.y + WBC.y + taijyuzoukanum.y + asanasi.num.y + syokujisokudo.num.y + nerumaesyokuji..num.y + eiyoubaransu.num.y + alcoholhindo.num.y + undou.num.y + hokou.num.y + hokousokudo.num.y + suiminkyuyou.num.y + tabaco.num.y + kaizenisi.num.y , data=female35_40_train, ntree=1000)
varimp(crf35_40female,conditional = T)


## ROC curve
## Logistic Regression
logit35_40_female.prob <- predict(logit35_40female,probability=T,female35_40_test)
pred.logit35_40_female <- prediction(logit35_40_female.prob,female35_40_test$Mets.num.x)
rocObj_logit35_40_female <- performance(pred.logit35_40_female,measure="tpr", x.measure="fpr")
prObj_logit35_40_female <- performance(pred.logit35_40_female,measure="prec", x.measure="rec")
aucObj_logit35_40_female <- performance(pred.logit35_40_female,measure="auc")
aucprObj_logit35_40_female <- performance(pred.logit35_40_female,measure="aucpr")
aucObj_logit35_40female <- aucObj_logit35_40_female@y.values[[1]]
aucprObj_logit35_40female <- aucprObj_logit35_40_female@y.values[[1]]
aucObj_logit35_40female
aucprObj_logit35_40female

## Random Forest
rf35_40_female.prob <- predict(rf35_40female,type="prob",female35_40_test)
pred.rf35_40_female <- prediction(rf35_40_female.prob[,2],female35_40_test$Mets.num.x)
rocObj_rf35_40_female <- performance(pred.rf35_40_female,measure="tpr", x.measure="fpr")
prObj_rf35_40_female <- performance(pred.rf35_40_female,measure="prec", x.measure="rec")
aucObj_rf35_40_female <- performance(pred.rf35_40_female,measure="auc")
aucprObj_rf35_40_female <- performance(pred.rf35_40_female,measure="aucpr")
aucObj_rf35_40_female <- aucObj_rf35_40_female@y.values[[1]]
aucprObj_rf35_40_female <- aucprObj_rf35_40_female@y.values[[1]]
aucObj_rf35_40_female
aucprObj_rf35_40_female

## ROC drawing
plot(rocObj_logit35_40_female,main="35_40female", col="red")
par(new=T)
y <- function(x)
  plot(y,0,1,xlab="",ylab="")
legend("bottomright",legend=c("Logistic Regression","Random Forest"),col=c("red","blue"),lty=c(1))
par(new=T)
plot(rocObj_rf35_40_female, col="blue")

## PR drawing
plot(prObj_logit35_40_female,main="35_40female", col="blue",ylim=c(0,1),xlim=c(0,1))
par(new=T)
y <- function(x)
  plot(y,0,1,xlab="",ylab="")
legend("topright",legend=c("Random Forest","Logistic Regression"),col=c("red","blue"),lty=c(1))
par(new=T)
plot(prObj_rf35_40_female, col="red",ylim=c(0,1),xlim=c(0,1))

# AUC test
library(pROC)
rf_roc <- roc(female35_40_test$Mets.num.x,rf35_40_female.prob[,2])
mr_roc <- roc(female35_40_test$Mets.num.x,logit35_40_female.prob)
roc.test(rf_roc,mr_roc)

# ROC Table
fn_ROC_Table <- function(pred, obs){
  
  pred1 <- prediction(predictions=pred, labels=obs)
  perf1 <- performance(pred1, measure="tpr", x.measure="fpr")
  
  ROCtable <- data.frame(
    Cutoff=unlist(pred1@cutoffs), 
    TP=unlist(pred1@tp), FP=unlist(pred1@fp),
    FN=unlist(pred1@fn), TN=unlist(pred1@tn),
    TPR=unlist(perf1@y.values), FPR=unlist(perf1@x.values),
    diff_TPR_FPR=unlist(perf1@y.values)-unlist(perf1@x.values),
    MissClasR=(unlist(pred1@fp)+unlist(pred1@fn))/
      (unlist(pred1@tp)+unlist(pred1@fp)+unlist(pred1@fn)+unlist(pred1@tn))
  )
  
  return(ROCtable)
}

ROCtable_p1 <- fn_ROC_Table(rf35_40_female.prob[,2],female35_40_test$Mets.num.x) 
for(i in 1:nrow(ROCtable_p1)){
  mx <- max(ROCtable_p1$diff_TPR_FPR) # max value of TPR-FPR
  if(ROCtable_p1$diff_TPR_FPR[i]==mx){ print(ROCtable_p1[i,]) }
}

ROCtable_p2 <- fn_ROC_Table(logit35_40_female.prob,female35_40_test$Mets.num.x) 
for(i in 1:nrow(ROCtable_p2)){
  mx <- max(ROCtable_p2$diff_TPR_FPR) # max value of TPR-FPR
  if(ROCtable_p2$diff_TPR_FPR[i]==mx){ print(ROCtable_p2[i,]) }
}

