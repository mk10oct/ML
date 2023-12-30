# logistic regression

data1<-read.csv("C:/Users/Manohar Kapse/OneDrive/Desktop/Case study Youtube/Case study Logistic Regression/Case study happiness/happyness_logistic.csv", stringsAsFactors = TRUE)
str(data1)


# convert result into factor
data1$Happyness_Score<-factor(data1$Happyness_Score, levels = c(0,1), labels = c("low_Happiness", "high_happiness"))

table(data1$Happyness_Score)

model1 <- glm(Happyness_Score~., family = "binomial", data = data1)
model1
summary(model1)

# coefficient, p value, walds test (Z), odds ratio

# odds ratio

oddsr<-exp(coef(model1)); 
oddsr; 
options(scipen = 999)

library(blorr)
library(Rcpp)
library(tidymodels)

# loglikelihood test or Omnibus test
lr1<-blr_test_lr(model1)
lr1

# hosmer lemeshow test
blr_test_hosmer_lemeshow(model1)

# Pseudo R2
blr_multi_model_fit_stats(model1)

# confusion matrix
confusion_matrix1<-blr_confusion_matrix(model1,cutoff = 0.5)
confusion_matrix1

f1<- 2*((confusion_matrix1$precision*confusion_matrix1$recall)/ (confusion_matrix1$precision+confusion_matrix1$recall))
f1

predict1<-predict(model1, data1, type = "response")

library(pROC)
roc_curve <- roc(data1$Happyness_Score, predict1)
auc_value <- auc(roc_curve)
print(auc_value)
#--------------
# roc curve

plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)
lines(x = c(0, 1), y = c(0, 1), col = "gray", lty = 2)
legend("bottomright", legend = paste("AUC =", round(auc(roc_curve), 2)), col = "blue", lwd = 2)


# Step-wise regression
step_model_aic<-blr_step_aic_both(model1); 
step_model_aic$model; 
summary(step_model_aic$model);

library(car)
library(ggplot2)
library(caret)
varImp(model1)


#--------------------------------------------------------

library(DynNom)
DynNom(model = model1)

#-------------------------------------------------------------------------------------------------#
