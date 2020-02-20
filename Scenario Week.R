
#Install and load the following packages:
library(tidyverse)
library(ggplot2)


#Let's merge the trains  data set with the products and stores
total1 <-left_join(products, train)
View(total1)
colnames(stores)[1] <- c("STORE_NUM")
colnames(stores)[2] <- c("STORE_NUMM")

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

#Linear regression for demand forecasting
lm.fit = lm(UNITS ~ PRICE + FEATURE + DISPLAY + REDUCTION + ADDRESS_CITY_NAME+ TPR_ONLY+ HHS,
            data= total1)
summary(lm.fit)
demandpred <- predict(lm.fit, total2)
view(demandpred)
library(Metrics)
rmse(total2$UNITS, demandpred)








#NEURALNET APPROACH 1
# Create Vector of Column Max and Min Values
maxs <- apply(total1[,2:29], 2, max)
mins <- apply(total1[,2:29], 2, min)
# Use scale() and convert the resulting matrix to a data frame
scaled.data <- as.data.frame(scale(total1, center = mins, scale = maxs - mins))
#Plotting the neural net 1.0
library(neuralnet)
n <- names(total1)
f <- as.formula(paste("UNITS ~ PRICE + FEATURE + DISPLAY ", paste(n[!n %in% "UNITS"], collapse = " + ")))
nn <- neuralnet(f,data=total1,hidden=c(5,3),linear.output=T)
plot(nn)
#Plotting the neural net 2.0
library(neuralnet)
n <- names(total1)
xnam <- paste0("PRICE + FEATURE + DISPLAY", 1:29)
(f <- as.formula(paste("UNITS ~ ", paste(xnam, collapse= "+"))))
nn <- neuralnet(f,data=total1,hidden=c(5,3),linear.output=T)
plot(nn)

#NEURAL NETWORK APPROACH 2
library(fastDummies)
library(janitor)
library(tidyselect)
# add category dummies to train data
train = dummy_cols(total1, "CATEGORY")
train = clean_names(total1, case="screaming_snake")
# select categorical variables
catvars = vars_select(names(total1), starts_with("CATEGORY_"))
catvars = paste(catvars, collapse=" + ")
# create formula
form = paste ("UNITS ~ PRICE + FEATURE + DISPLAY +" ,catvars)
# run NN
nn <- neuralnet(as.formula(form), data=total1)
plot(nn)



#NEURAL NETWORK APPROACH 3
#creating dummy variables for non numeric variables
dummy_cols(total1, remove_first_dummy = TRUE)
ADDRESS_CITY_NAME.f <- factor(total1$ADDRESS_CITY_NAME)
dummies = model.matrix(~ADDRESS_CITY_NAME.f)
m <- model.matrix( 
  ~ UNITS + PRICE + FEATURE + DISPLAY + REDUCTION + ADDRESS_CITY_NAME+ TPR_ONLY+ HHS, 
  data = total1
)
head(m)
library(neuralnet)
nn <- neuralnet( UNITS ~ PRICE + FEATURE + DISPLAY + REDUCTION + ADDRESS_CITY_NAME+ TPR_ONLY+ HHS, 
                 m, )
plot(nn)







