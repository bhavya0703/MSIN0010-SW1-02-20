#LINEAR REGRESSION
#Install and load the following packages:
library(tidyverse)
library(ggplot2)
#Let's merge the trains  data set with the products and stores
totalll <-left_join(products, train)
View(total1)
colnames(stores)[1] <- c("STORE_NUM")
colnames(stores)[2] <- c("STORE_NUMM")
total1 <-left_join(totalll, stores)
#Let's merge the test data set with the products and stores
totall <-left_join(products, test)
total2 <-left_join(stores,totall)
View(total2)
#let's create a new column which is reduction
total2$REDUCTION<-total2$BASE_PRICE-total2$PRICE
total1$REDUCTION<-total1$BASE_PRICE-total1$PRICE
#let's create a new column which is reduction ratio
total1$REDUCTION_ratio<-(total1$BASE_PRICE-total1$PRICE)/(total1$BASE_PRICE)
total2$REDUCTION_ratio<-(total2$BASE_PRICE-total2$PRICE)/(total2$BASE_PRICE)
#linear regression
lm.fit = lm(UNITS ~ PRICE + FEATURE + DISPLAY + REDUCTION + TPR_ONLY + HHS ,
            data= total1)
summary(lm.fit)
#Predicting 
library(Metrics)
demandpred2 <- predict(lm.fit, total2)
demandpred1 <- predict(lm.fit, total1)
view(demandpred2)
library(Metrics)
#in sample rmse- actual train and predicted train
rmse(train$UNITS, demandpred1)
#out of sample rmse- actual test and predicted test
rmse(test$UNITS, demandpred2)
#neater graph
library(tidyverse)
library(cowplot)
theme_set(theme_cowplot())
library(modelsummary)
library(broom)
ggplot()  + 
  geom_point(aes(demandpred2, total2$UNITS)) + 
  geom_smooth(aes(demandpred2, total2$UNITS), method = "lm", se = FALSE, color = "lightgrey") + 
  labs(x = "Predicted Units sold", y = "Actual Units sold") + 
  theme_bw()


#REGRESSION TREE
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
library(Metrics)
tree.fit = rpart(UNITS~PRICE+REDUCTION_ratio+REDUCTION+FEATURE+TPR_ONLY+DISPLAY, data=total1,
                 method="anova")
rpart.plot(tree.fit,type=3,digits=3,fallen.leaves=TRUE)
RegressionTreePredict1 <- predict(tree.fit,total1)
RegressionTreePredict2 <- predict(tree.fit, total2)
#in sample rmse- train actual vs train predicted
rmse(total1$UNITS, RegressionTreePredict1)
#predictive rmse- test actual vs test predicted
rmse(total2$UNITS,RegressionTreePredict2)
#neater graph
library(tidyverse)
library(cowplot)
theme_set(theme_cowplot())
library(modelsummary)
library(broom)
ggplot()  + 
  geom_point(aes(RegressionTreePredict2, total2$UNITS)) + 
  geom_smooth(aes(RegressionTreePredict2, total2$UNITS), method = "lm", se = FALSE, color = "lightgrey") + 
  labs(x = "Predicted Units sold", y = "Actual Units sold") + 
  theme_bw()


#LASSO
library(glmnet)
library(glm.predict)
grid = 10^seq(-5,0,length=100)
trainX = train[,c("PRICE","DISPLAY","FEATURE","TPR_ONLY", "REDUCTION")]
y<- train$UNITS
trainX = as.matrix(trainX)
lasso = glmnet(trainX,train$UNITS) 
plot(lasso,xvar="lambda", label= TRUE)
print(lasso)
testX = test[,c("PRICE","DISPLAY","FEATURE","TPR_ONLY", "REDUCTION")]
#cross validation
cvfit = cv.glmnet(trainX, train$UNITS, alpha=1)
plot(cvfit)
bestlambda= cvfit$lambda.min
#prediction
testX = as.matrix(testX)
lassopred1 <- predict(lasso,s= bestlambda, newx=trainX)
lassopred2 <- predict(lasso,s= bestlambda, newx=testX)
cvfit$lambda.min
cvfit$lambda.1se
#in sample rmse- actual train vs predicted train
rmse(train$UNITS, lassopred1)
rmse(test$UNITS, lassopred2)
#neater graph
library(tidyverse)
library(cowplot)
theme_set(theme_cowplot())
library(modelsummary)
library(broom)
ggplot()  + 
  geom_point(aes(lassopred2, test$UNITS)) + 
  geom_smooth(aes(lassopred2, test$UNITS), method = "lm", se = FALSE, color = "lightgrey") + 
  labs(x = "Predicted Units sold", y = "Actual Units sold") + 
  theme_bw()


#NEURAL NET*add code pls romain*








