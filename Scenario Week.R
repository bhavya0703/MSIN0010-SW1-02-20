
#Install and load the following packages:
library(tidyverse)
library(ggplot2)

#Let's merge the trains data set with the products data set in order to get
total <-left_join(products, train)
View(total)
colnames(stores)[1] <- c("STORE_NUM")

#creating dummy variables for non numeric variables
dummy_cols(total1, remove_first_dummy = TRUE)
ADDRESS_CITY_NAME.f <- factor(total1$ADDRESS_CITY_NAME)
dummies = model.matrix(~ADDRESS_CITY_NAME.f)

#neuralnet for demand forecasting
m <- model.matrix( 
   ~ UNITS + PRICE + FEATURE + DISPLAY + REDUCTION + ADDRESS_CITY_NAME+ TPR_ONLY+ HHS, 
  data = total1
)
head(m)

library(neuralnet)
nn <- neuralnet( UNITS ~ PRICE + FEATURE + DISPLAY + REDUCTION + ADDRESS_CITY_NAME+ TPR_ONLY+ HHS, 
  m, )
plot(nn)


