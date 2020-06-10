#Open the data sets products, stores and transactions
products <- read.csv("C:/Users/romai/OneDrive/Documents/first year of management science/scenario week 3/data sets/products.csv")
transactions <- read.csv("C:/Users/romai/OneDrive/Documents/first year of management science/scenario week 3/data sets/transactions.csv")
stores <- read.csv("C:/Users/romai/OneDrive/Documents/first year of management science/scenario week 3/data sets/stores.csv")
test <- read.csv("C:/Users/romai/OneDrive/Documents/first year of management science/scenario week 3/data sets/test.csv")
train <- read.csv("C:/Users/romai/OneDrive/Documents/first year of management science/scenario week 3/data sets/train.csv")
us.zip.code.latitude.and.longitude <- read.csv("C:/Users/romai/OneDrive/Documents/first year of management science/scenario week 3/data sets/us-zip-code-latitude-and-longitude.csv", sep=";")

#Install and load the following packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(GGally)
library(viridis)
library(rpart)
library(rpart.plot)
library(rworldmap)
library(rgdal)
library(ggsn)
library(maps)
library(lattice)
library(rgl)
library(ggridges)
library(broom)
library(forcats)
library(neuralnet)
library(caret)
library(Metrics)
library(modelsummary)
library(cowplot)
library(glmnet)
library(glm.predict)
library(randomForest)

#CLEANING THE DATA SET
#Let's merge the transactions data set with the products data set in order to get a more detailed transaction data set called total
total<-left_join(transactions, products)
total_train<-left_join(train, products)
total_test<-left_join(test, products)
#let's create a units per visits column
total$UNITS_PER_VISITS<-total$UNITS/total$VISITS
total_train$UNITS_PER_VISITS<-total_train$UNITS/total_train$VISITS
total_test$UNITS_PER_VISITS<-total_test$UNITS/total_test$VISITS
#let's create a reduction column which is bas price minus price 
total$REDUCTION<-total$BASE_PRICE-total$PRICE
total_train$REDUCTION<-total_train$BASE_PRICE-total_train$PRICE
total_test$REDUCTION<-total_test$BASE_PRICE-total_test$PRICE
#let's create a reduction ratio column which is the reduction divided by the base price in order to find a percentage discount
total$REDUCTION_ratio<-(total$BASE_PRICE-total$PRICE)/(total$BASE_PRICE)
total_train$REDUCTION_ratio<-(total_train$BASE_PRICE-total_train$PRICE)/(total_train$BASE_PRICE)
total_test$REDUCTION_ratio<-(total_test$BASE_PRICE-total_test$PRICE)/(total_test$BASE_PRICE)
#renaming the binary elements
total$FEATURE[total$FEATURE=="1"]<-"yes"
total$FEATURE[total$FEATURE=="0"]<-"no"
total$DISPLAY[total$DISPLAY=="1"]<-"yes"
total$DISPLAY[total$DISPLAY=="0"]<-"no"
total$TPR_ONLY[total$TPR_ONLY=="1"]<-"yes"
total$TPR_ONLY[total$TPR_ONLY=="0"]<-"no"
#let's create a price promotion column which is when a product had a positive reduction ratio and/or a TPR
total$PRICE_PROMOTION<-ifelse(total$REDUCTION_ratio>0|total$TPR_ONLY=="yes","yes","no")
total_train$PRICE_PROMOTION<-ifelse(total_train$REDUCTION_ratio>0|total_train$TPR_ONLY=="yes","yes","no")
total_test$PRICE_PROMOTION<-ifelse(total_test$REDUCTION_ratio>0|total_test$TPR_ONLY=="yes","yes","no")
#let's create a store promotion column which is when a product was featured and/or displayed
total$STORE_PROMOTION<-ifelse(total$DISPLAY=="yes"|total$FEATURE=="yes","yes","no")
total_train$STORE_PROMOTION<-ifelse(total_train$DISPLAY=="yes"|total_train$FEATURE=="yes","yes","no")
total_test$STORE_PROMOTION<-ifelse(total_test$DISPLAY=="yes"|total_test$FEATURE=="yes","yes","no")

#BASIC VISUALISATION
#Plot Ridgeline of Prices with product sub categories 
ggplot(total, aes(x = PRICE, y =SUB_CATEGORY , fill = stat(x))) +
  geom_density_ridges_gradient(scale = 0.5, rel_min_height = 0.005) +
  scale_fill_viridis_c(name = "PRICE", option = "C") +
  labs(title = "Prices of products")+
  theme_ridges()+
  scale_x_continuous(limits = c(0,10))+
  scale_y_discrete(expand = c(0.05,0.05))+
  xlab("Price")+
  ylab("Sub Category")

#MAPPING LOCATIONS OF STORES
#add coordinates for "woodlands" and "the woodlands" as they are missing
df<-data.frame("Woodlands",30.171548,-95.507982, "TX")
names(df)<-c("City","Latitude","Longitude", "State")
de<-data.frame("The Woodlands",30.171548,-95.507982, "TX")
names(de)<-c("City","Latitude","Longitude", "State")
#merging the US cities coordinates data set with the stores data set in order to get the store locations
us_city<-us.zip.code.latitude.and.longitude[c(2,4,5,3)]
us_city<-rbind(us_city, df, de)
us_city$ADDRESS_CITY_NAME<-us_city$City
us_city$ADDRESS_CITY_NAME<-toupper(us_city$ADDRESS_CITY_NAME)
us_city$ADDRESS_STATE_PROV_CODE<-us_city$State
store_location<-left_join(stores, us_city, by=c("ADDRESS_STATE_PROV_CODE","ADDRESS_CITY_NAME"))
store_location$STORE_NUM<-store_location$STORE_ID
store_location<-store_location %>% distinct(STORE_ID,.keep_all = TRUE)
store_location<-store_location[,c(14,12,11,3,4)]
#get the usa map and smaller sections that we are intersted in (Texas and Ohia, Kentucky and Indiana)
states <- map_data("county")
states<-ggplot()+
  geom_polygon(data = states,
               aes(x = long, y = lat, fill=region, group = group), alpha=0.3)+
  guides(fill=FALSE)+
  coord_quickmap()+  # Prevents stretching when resizing
  xlab("Longitude")+
  ylab("Latitude")+
  theme_void()
all<-states+coord_fixed(xlim=c(-98,-82),ylim=c(25,43))
texas<-states+coord_fixed(xlim=c(-97.75,-93.75),ylim=c(28.75,34))
OH_IN_KY<-states+coord_fixed(xlim=c(-85,-83.5),ylim=c(38.75,40.75))
#plotting the stores on the us map
states+geom_point(data = store_location,aes(x = Longitude, y = Latitude)) #stores location in the USA
all+geom_point(data = store_location,aes(x = Longitude, y = Latitude)) #stores location in the concerned region
texas+geom_point(data = store_location,aes(x = Longitude, y = Latitude)) #stores location in Texas
OH_IN_KY+geom_point(data = store_location,aes(x = Longitude, y = Latitude)) #stores location in Ohio, Kentucky and Indiana
#store density
states+stat_density_2d(data=store_location, aes(x=Longitude, y=Latitude ,alpha=..level..),geom="polygon")+theme(legend.position = "none")


#ELASTICITY
#Multiple Linear Regression to Find Elasticity
train_merged <- left_join(train, products)
fitted_models <-  train_merged[!train_merged$PRICE==0,] %>% #to avoid error
  group_by(UPC)%>% #we group by UPC so it will perform a regression and find the elasticity for every unique product
  do(model = lm(log(UNITS) ~ log(PRICE), data=.)) #defining that we want to regress price by units
tidied <- fitted_models %>% tidy(model)%>% filter(term=="log(PRICE)") #tidying model and filtering so it only shows us the price coefficient (instead of price and intercept)
elasticities <- data.frame(tidied[,c(1,3)]) #Stores all the elasticities of each product. We want the UPC and its corresponding elasticity, which are column 1 and 3
names(elasticities) <- c("UPC","ELASTICITY") #renaming the columns of the dataframe so it is clearer
#Regression for PL MINI TWIST PRETZELS only; to show in report what happens in one iteration of the multiple regression
plpretz <-train_merged %>% filter(train_merged$DESCRIPTION == "PL MINI TWIST PRETZELS") 
lm.fit = lm(log(UNITS)~log(PRICE), data=plpretz)
summary(lm.fit)
#PLOTTING ALL ELASTICITIES OF EACH PRODUCT
#Make total dataset merging transactions and products by UPC
total <- left_join(transactions, products) #dataset for all transactions and products merged by UPC
#Clean total dataset; make sure each UPC has a unique description (we need to do this so our graph will plot!)
total <- within(total, {DESCRIPTION[UPC == "1600027528"] <- "GM CHEERIOS 18OZ"
DESCRIPTION[UPC == "1600027564"] <- "GM CHEERIOS 12OZ"
DESCRIPTION[UPC == "31254742725"] <- "LSTRNE CL MINT ANTSPTC MW 500ML"
DESCRIPTION[UPC == "31254742835"] <- "LSTRNE CL MINT ANTSPTC MW 1LT"
DESCRIPTION[UPC == "1111035398"] <- "PL BL MINT ANTSPTC RINSE 1.5LT"
DESCRIPTION[UPC == "1111038078"] <- "PL BL MINT ANTSPTC RINSE 500ML"})
#Make new dataframe with UPC, description, and elasticity
total.extracted <- distinct(data.frame(total[,c(3,13,15,17)])) #UPC, description, product category, product sub-category, and product size from the total dataset
UDCSE <- left_join(total.extracted, elasticities) #adding on elasticity according to UPC. UDCSE = UPC, description, category, product size, and elasticity. We specify size as some descriptions are the same but different sizes. 
#Clean UDCSE dataset
#Some variables have one description but have two different UPCs due to the size of the product. 
#We rename rows adding on the size of the product to the description name since they have duplicate descriptions which will cause problems when graphing later. 
#Note that our regression, and elasticities, are okay since they were done by UPC anyways.
#Ideally we would clean the total dataset, however this would take R an extremely long time, and we only have to clean because we are plotting by description. 
#We want to arrange the dataset by elasticity so our graph will look nicer.
UDCSE <- UDCSE[order(UDCSE$ELASTICITY),]
#We can even check and see that there are now both 55 UPCs and descriptions. We need to make sure of this or else our graph labels will not work!
length(unique(UDCSE$DESCRIPTION))
length(unique(UDCSE$UPC))
#Plotting the overall elasticities graph for every product
#We will re-use this; create a function!
#data specifies what dataset to use, title specifies the title, subtitle specifies the subtitle
plotElasticityByProduct <- function(data, title, subtitle){
  ggplot(data, mapping = aes(x = fct_reorder(data$DESCRIPTION, data$ELASTICITY), y=data$ELASTICITY, fill=data$CATEGORY)) + #Plotting product by elasticity. We know that the product corresponds to the elasticity because we made our index dataframe. We fill the colour from category in the dataset products.
    labs(x = "Product", 
         y = "Elasticity", 
         title = title,
         subtitle = subtitle,
         fill = "Product Category" #naming legend
    )+
    theme(axis.text.x = element_text(size=7, angle = 90,hjust=0.95))+ #Rotating the text and aligning it 
    geom_col() #colouring by product category
}
#--------REPORT: FIGURE 1----------
plotElasticityByProduct(UDCSE, "Elasticities by Product and Product Category", "Location: All Locations")

#PLOTTING ELASTICITY BY PRODUCT CATEGORY
#Creating the plotElasticity function
#We are plotting the elasticity of each product category; this will use the same code each time. Thus, we make a function to repeat the task
#category specifies which product category to plot
#col specifies which colour to make the grap
#subtitle specifies the specific subtitle for that graph

plotElasticityByCategory <- function(category, col, subtitle){
  UDCSE.subset <- UDCSE[UDCSE$CATEGORY == category,] #creating a subset of the UDCSE dataset based on the argument passed in the function.
  ggplot(UDCSE.subset, mapping = aes(x = fct_reorder(UDCSE.subset$DESCRIPTION, UDCSE.subset$ELASTICITY), y=UDCSE.subset$ELASTICITY, fill=UDCSE.subset$CATEGORY)) + #Plotting product by elasticity. We know that the product corresponds to the elasticity because we made our index dataframe. We fill the colour from category in the dataset products.
    labs(x = "Product", 
         y = "Elasticity", 
         title = "Elasticities by Product and Product Category",
         subtitle = subtitle,
         fill = "Product Category" #naming legend according to what product category it is
    )+
    scale_fill_manual(values=col)+ #changing the colour based on the colour we specify
    theme(axis.text.x = element_text(angle = 90,hjust=0.95))+ #Rotating the text and aligning it 
    geom_col()+
    expand_limits(y = c(-4, 2))+ #specifying range for the graph
    geom_text(aes(label= round(UDCSE$ELASTICITY[UDCSE$CATEGORY == category], digits = 2)), #we round the elasticity to make it easier to read on the graph; for the category specific graphs, the bars are big enough to add numbers on them
              color="black", vjust = ifelse(round(UDCSE$ELASTICITY[UDCSE$CATEGORY == category])>0, 1.2, -0.5), size=2.5) #We need to vertically adjust the labels differently if the elasticities are greater or less than zero
}
#We call the function and plot each product category
#--------REPORT: FIGURE 2.1 ----------
plotElasticityByCategory("BAG SNACKS", "#F8766D", "Product Category: Bag Snacks")
#--------REPORT: FIGURE 2.2 ----------
plotElasticityByCategory("COLD CEREAL", "#7CAE00", "Product Category: Cold Cereal") 
#--------REPORT: FIGURE 2.3 ----------
plotElasticityByCategory("FROZEN PIZZA", "#00BFC4", "Product Category: Frozen Pizza")
#--------REPORT: FIGURE 2.4 ----------
plotElasticityByCategory("ORAL HYGIENE PRODUCTS", "#C77CFF", "Product Category: Oral Hygiene Products")
#Calculating Mean Elasticities of Each Category
#To make the graphs more informative, we can find the mean elasticity of each product category to display on the graph.
#Let's find the mean elasticites:
categories <- unique(UDCSE$CATEGORY) #storing a vector of all the categories in a variable
for (i in categories){
  meanElasticity <- mean(UDCSE$ELASTICITY[UDCSE$CATEGORY == i]) #gets mean elasticity per category
  cat("Mean Elasticity of", i, ":", meanElasticity, "\n") #prints out the mean elasticities
}
#ELASTICITIES PER STATE
#Create a dataframe PER state
#Calculate the elasticity of each product PER state
#Find the mean elasticity per product category PER state
#Make a dataset with the state, product category and mean elasticity PER that state
#Use that dataset to plot on bargraph
#Merging datasets
#In the store dataset, the column identifying the store is the common variable. But they have different names, so we change the name of the column to make merging easier
names(stores)[names(stores) == "STORE_ID"] <- "STORE_NUM"
#Joining by STORE_NUM
total.ws <- left_join(total, stores)
#Making a dataframe per state
total.ws.TX <- total.ws[total.ws$ADDRESS_STATE_PROV_CODE == "TX",]
total.ws.OH <- total.ws[total.ws$ADDRESS_STATE_PROV_CODE == "OH",]
total.ws.KY <- total.ws[total.ws$ADDRESS_STATE_PROV_CODE == "KY",]
total.ws.IN <- total.ws[total.ws$ADDRESS_STATE_PROV_CODE == "IN",]
#Note that the number of unique UPCs per state are different (e.g. 49 in texas, 55 in ohio)
#We can infer this just means that some goods aren't sold in certain places
#This is okay for our purposes; we'll just calculate the mean elasticity for the product category given the products (UPCs) available
#Showing how many UPCs the dataset has data for in each state; later on when we make new dataframes with elasticities, we can refer back to these values to make sure the values are actually for that state.
length(unique(total.ws.TX$UPC))
length(unique(total.ws.OH$UPC))
length(unique(total.ws.KY$UPC))
length(unique(total.ws.IN$UPC))
#Calculating the elasticities per state; we use the same multiple regression code from earlier
#Note that some of the dataframes will have N/A values, since as we found earlier, not every state sells all products
total.ws.extracted <- distinct(data.frame(total.ws[,c(3,13,15,17)])) #extracting relevant rows from total.ws; we only need to do this the first time
#TEXAS---------------------------------------------------------
#Regression for Texas transactions only
fitted_models.TX <-  total.ws.TX[!total.ws.TX$PRICE==0,] %>%
  group_by(UPC)%>%
  do(model.TX = lm(log(UNITS) ~ log(PRICE), data=.))
tidied.TX <- fitted_models.TX %>% tidy(model.TX)%>% filter(term=="log(PRICE)")
elasticities.TX <- data.frame(tidied.TX[,c(1,3)])
names(elasticities.TX) <- c("UPC","ELASTICITY (TX)") #renaming the columns of the dataframe so it is clearer; we specify that the elasticity shown is for Texas only
#Making a dataframe that shows the product and corresponding elasticity in texas
UDCSE.TX <- left_join(total.ws.extracted, elasticities.TX)
UDCSE.TX <- UDCSE.TX[complete.cases(UDCSE.TX),] #Removing the N/A Rows since we know that in Texas, they don't sell all 55 products. 
#OHIO------------------------------------------------------------
fitted_models.OH <-  total.ws.OH[!total.ws.OH$PRICE==0,] %>%
  group_by(UPC)%>%
  do(model.OH = lm(log(UNITS) ~ log(PRICE), data=.))
tidied.OH <- fitted_models.OH %>% tidy(model.OH)%>% filter(term=="log(PRICE)")
elasticities.OH <- data.frame(tidied.OH[,c(1,3)])
names(elasticities.OH) <- c("UPC","ELASTICITY (OH)")
#Making a dataframe that shows the product and corresponding elasticity in texas
UDCSE.OH <- left_join(total.ws.extracted, elasticities.OH)
#KENTUCKY------------------------------------------------------------
fitted_models.KY <-  total.ws.KY[!total.ws.KY$PRICE==0,] %>%
  group_by(UPC)%>%
  do(model.KY = lm(log(UNITS) ~ log(PRICE), data=.))
tidied.KY <- fitted_models.KY %>% tidy(model.KY)%>% filter(term=="log(PRICE)")
elasticities.KY <- data.frame(tidied.KY[,c(1,3)])
names(elasticities.KY) <- c("UPC","ELASTICITY (KY)")
#Making a dataframe that shows the product and corresponding elasticity in Kentucky
UDCSE.KY <- left_join(total.ws.extracted, elasticities.KY)
UDCSE.KY <- UDCSE.KY[complete.cases(UDCSE.KY),] #Removing the N/A Rows since we know that in Kentucky, they don't sell all 55 products. 
#INDIANA------------------------------------------------------------
fitted_models.IN <-  total.ws.IN[!total.ws.IN$PRICE==0,] %>%
  group_by(UPC)%>%
  do(model.IN = lm(log(UNITS) ~ log(PRICE), data=.))
tidied.IN <- fitted_models.IN %>% tidy(model.IN)%>% filter(term=="log(PRICE)")
elasticities.IN <- data.frame(tidied.IN[,c(1,3)])
names(elasticities.IN) <- c("UPC","ELASTICITY (IN)")
#Making a dataframe that shows the product and corresponding elasticity in Indiana
UDCSE.IN <- left_join(total.ws.extracted, elasticities.IN)
#PLOTTING ELASTICITY BY PRODUCT BY STATE
#We can re-use our function from earlier to plot every product, but now, we specify that we are using the state specific datasets. 
#Note that for Texas and Kentucky, some of the UPCs are missing since they are not sold there (no transaction data)
plotElasticityByProduct(UDCSE.TX, "Elasticities by Product and Product Category", "Location: Texas") #Texas Graph
plotElasticityByProduct(UDCSE.OH, "Elasticities by Product and Product Category", "Location: Ohio") #Ohio Graph
plotElasticityByProduct(UDCSE.KY, "Elasticities by Product and Product Category", "Location: Kentucky") #Kentucky Graph
plotElasticityByProduct(UDCSE.IN, "Elasticities by Product and Prodphuct Category", "Location: Indiana") #Indiana Graph
#PLOITTING ELASTICITY BY PRODUCT CATEGORY BY STATE
#It would be difficult to show all the elasticities of each product for all the states and comapre them
#It's much more practical to just show by category
#First we calculate the mean elasticities of all the products in each category for all the states stored in a dataframe
meanElasticityTX <- aggregate(UDCSE.TX[, 5], list(UDCSE.TX$CATEGORY), mean) #We get the mean elasticity per product category for Texas. Elasticity is column 5 which is why we specify 5. 
meanElasticityOH <- aggregate(UDCSE.OH[, 5], list(UDCSE.OH$CATEGORY), mean)
meanElasticityKY <- aggregate(UDCSE.KY[, 5], list(UDCSE.KY$CATEGORY), mean)
meanElasticityIN <- aggregate(UDCSE.IN[, 5], list(UDCSE.IN$CATEGORY), mean)
#Combining the dataframes into one
statesVector <- c("TX","TX","TX","TX","OH","OH","OH","OH","KY","KY","KY","KY","IN","IN","IN","IN") #A quick work around here since we only ahve 16 columns...
meanElasticityStates <- data.frame(statesVector, rbind(meanElasticityTX, meanElasticityOH, meanElasticityKY, meanElasticityIN))
names(meanElasticityStates) <- c("STATE","CATEGORY", "MEAN_ELASTICITY") #re-naming
#Plotting
#Graph grouped by product category
ggplot(data=meanElasticityStates, aes(x=meanElasticityStates$CATEGORY, y=meanElasticityStates$MEAN_ELASTICITY, fill=meanElasticityStates$STATE)) +
  geom_bar(stat="identity", position=position_dodge())+
  labs(x = "Product Category", 
       y = "Mean Elasticity", 
       title = "Mean Elasticities of Product Categories by State",
       subtitle = "Grouped by: Product Category",
       fill = "State" #naming legend according to what product category it is
  )+
  geom_text(aes(label= round(meanElasticityStates$MEAN_ELASTICITY, digits = 2)), #we round the elasticity to make it easier to read on the graph
            color="black", position = position_dodge(0.9), vjust = -1.6, size=2.5)+
  scale_fill_manual(values = c("#EC2049", "#F26B38", "#F7DB4F", "#2F9599")) #We want different colours than the default since the bars will be coloured by state; we don't want the same colours as our product categories or else this will be confusing!

#It might be useful to also have a graph organized by state
ggplot(data=meanElasticityStates, aes(x=meanElasticityStates$STATE, y=meanElasticityStates$MEAN_ELASTICITY, fill=meanElasticityStates$CATEGORY)) +
  geom_bar(stat="identity", position=position_dodge())+
  labs(x = "State", 
       y = "Mean Elasticity", 
       title = "Mean Elasticities of Product Categories by State",
       subtitle = "Grouped by: State",
       fill = "Product Category" #naming legend according to what product category it is
  )+
  geom_text(aes(label= round(meanElasticityStates$MEAN_ELASTICITY, digits = 2)), #we round the elasticity to make it easier to read on the graph
            color="black", position = position_dodge(0.9), vjust = -1.6, size=2.5)
#getting the elasticities per product in each store
transaction_with_elasticities<-left_join(total,UDCSE)
store_elasticities<-left_join(store_location,transaction_with_elasticities)
store_elasticities<-store_elasticities %>%
  group_by(STORE_NUM, Longitude, Latitude, ADDRESS_CITY_NAME, ADDRESS_STATE_PROV_CODE, CATEGORY) %>%
  summarise(ELASTICITY=mean(ELASTICITY))
store_elas_fro_pizza<-store_elasticities[which(store_elasticities$CATEGORY=="FROZEN PIZZA"),]
store_elas_bag_sna<-store_elasticities[which(store_elasticities$CATEGORY=="BAG SNACKS"),]
store_elas_oral_hyg<-store_elasticities[which(store_elasticities$CATEGORY=="ORAL HYGIENE PRODUCTS"),]
store_elas_cold_cer<-store_elasticities[which(store_elasticities$CATEGORY=="COLD CEREAL"),]
#MAPPING ELASTICITY PER REGION
all+geom_point(data = store_elasticities,aes(x = Longitude, y = Latitude, color = store_elasticities$ELASTICITY),size=1.5, shape= 16)+
  scale_colour_gradient(store_elasticities$ELASTICITY, low = "firebrick1", high = "midnightblue")+
  theme_bw()+
  labs(legend="Elasticity")+
  guides(size=FALSE)+
  guides(color=guide_legend(title="Elasticity"))+
  labs(title = "Elasticities per store")
texas+geom_point(data = store_elasticities,aes(x = Longitude, y = Latitude, color = store_elasticities$ELASTICITY),size=1.5, shape= 16)+
  scale_colour_gradient(store_elasticities$ELASTICITY, low = "firebrick1", high = "midnightblue")+
  theme_bw()+
  labs(legend="Elasticity")+
  guides(size=FALSE)+
  guides(color=guide_legend(title="Elasticity"))+
  labs(title = "Elasticities per store in Texas")
OH_IN_KY+geom_point(data = store_elasticities,aes(x = Longitude, y = Latitude, color = store_elasticities$ELASTICITY),size=1.5, shape= 16)+
  scale_colour_gradient(store_elasticities$ELASTICITY, low = "firebrick1", high = "midnightblue")+
  theme_bw()+
  labs(legend="Elasticity")+
  guides(size=FALSE)+
  guides(color=guide_legend(title="Elasticities"))+
  labs(title = "Elasticities per store in Ohio, Kentucky and Indiana")
#MAPPING ELASTICITY PER REGION PER CATEGORY
#cold cereals
all+geom_point(data = store_elas_cold_cer,aes(x = Longitude, y = Latitude, color = store_elas_cold_cer$ELASTICITY),size=1.5, shape= 16)+
  scale_colour_gradient(store_elas_cold_cer$ELASTICITY, low = "firebrick1", high = "midnightblue")+
  theme_bw()+
  guides(size=FALSE)+
  guides(color=guide_legend(title="Elasticity"))
texas+geom_point(data = store_elas_cold_cer,aes(x = Longitude, y = Latitude, color = store_elas_cold_cer$ELASTICITY),size=3, shape= 16)+
  scale_colour_gradient(store_elas_cold_cer$ELASTICITY, low = "firebrick1", high = "midnightblue")+
  theme_bw()+
  guides(size=FALSE)+
  guides(color=guide_legend(title="Elasticity"))
OH_IN_KY+geom_point(data = store_elas_cold_cer,aes(x = Longitude, y = Latitude, color = store_elas_cold_cer$ELASTICITY),size=3, shape= 16)+
  scale_colour_gradient(store_elas_cold_cer$ELASTICITY, low = "firebrick1", high = "midnightblue")+
  theme_bw()+
  guides(size=FALSE)+
  guides(color=guide_legend(title="Elasticity"))
#frozen pizza
all+geom_point(data = store_elas_fro_pizza,aes(x = Longitude, y = Latitude, color = store_elas_fro_pizza$ELASTICITY),size=1.5, shape= 16)+
  scale_colour_gradient(store_elas_fro_pizza$ELASTICITY, low = "firebrick1", high = "midnightblue")+
  theme_bw()+
  guides(size=FALSE)+
  guides(color=guide_legend(title="Elasticity"))
texas+geom_point(data = store_elas_fro_pizza,aes(x = Longitude, y = Latitude, color = store_elas_fro_pizza$ELASTICITY),size=3, shape= 16)+
  scale_colour_gradient(store_elas_fro_pizza$ELASTICITY, low = "firebrick1", high = "midnightblue")+
  theme_bw()+
  guides(size=FALSE)+
  guides(color=guide_legend(title="Elasticity"))
OH_IN_KY+geom_point(data = store_elas_fro_pizza,aes(x = Longitude, y = Latitude, color = store_elas_fro_pizza$ELASTICITY),size=3, shape= 16)+
  scale_colour_gradient(store_elas_fro_pizza$ELASTICITY, low = "firebrick1", high = "midnightblue")+
  theme_bw()+
  guides(size=FALSE)+
  guides(color=guide_legend(title="Elasticity"))
#bag snacks
all+geom_point(data = store_elas_bag_sna,aes(x = Longitude, y = Latitude, color = store_elas_bag_sna$ELASTICITY),size=1.5, shape= 16)+
  scale_colour_gradient(store_elas_bag_sna$ELASTICITY, low = "firebrick1", high = "midnightblue")+
  theme_bw()+
  guides(size=FALSE)+
  guides(color=guide_legend(title="Elasticity"))
texas+geom_point(data = store_elas_bag_sna,aes(x = Longitude, y = Latitude, color = store_elas_bag_sna$ELASTICITY),size=3, shape= 16)+
  scale_colour_gradient(store_elas_bag_sna$ELASTICITY, low = "firebrick1", high = "midnightblue")+
  theme_bw()+
  guides(size=FALSE)+
  guides(color=guide_legend(title="Elasticity"))
OH_IN_KY+geom_point(data = store_elas_bag_sna,aes(x = Longitude, y = Latitude, color = store_elas_bag_sna$ELASTICITY),size=3, shape= 16)+
  scale_colour_gradient(store_elas_bag_sna$ELASTICITY, low = "firebrick1", high = "midnightblue")+
  theme_bw()+
  guides(size=FALSE)+
  guides(color=guide_legend(title="Elasticity"))
#oral hygiene products
all+geom_point(data = store_elas_oral_hyg,aes(x = Longitude, y = Latitude, color = store_elas_oral_hyg$ELASTICITY),size=1.5, shape= 16)+
  scale_colour_gradient(store_elas_oral_hyg$ELASTICITY, low = "firebrick1", high = "midnightblue")+
  theme_bw()+
  guides(size=FALSE)+
  guides(color=guide_legend(title="Elasticity"))
texas+geom_point(data = store_elas_oral_hyg,aes(x = Longitude, y = Latitude, color = store_elas_oral_hyg$ELASTICITY),size=3, shape= 16)+
  scale_colour_gradient(store_elas_oral_hyg$ELASTICITY, low = "firebrick1", high = "midnightblue")+
  theme_bw()+
  guides(size=FALSE)+
  guides(color=guide_legend(title="Elasticity"))
OH_IN_KY+geom_point(data = store_elas_oral_hyg,aes(x = Longitude, y = Latitude, color = store_elas_oral_hyg$ELASTICITY),size=3, shape= 16)+
  scale_colour_gradient(store_elas_oral_hyg$ELASTICITY, low = "firebrick1", high = "midnightblue")+
  theme_bw()+
  guides(size=FALSE)+
  guides(color=guide_legend(title="Elasticity"))


#PLOTTING FOR UNITS PER VISITS AND PROMOTION
#plot distribution of units/visits
ggplot(data=total, aes(x=total$UNITS_PER_VISITS))+
  geom_density(fill="firebrick2")+
  ggtitle("Distribution of units per visits")+
  xlab("Units per visits")+
  ylab(" ")+
  xlim(1,2)
#scatter plot of reduction vs units per visits
ggplot(total, aes (x=REDUCTION_ratio, y=UNITS_PER_VISITS))+
  geom_point()+
  xlab("Reduction ratio")+
  ylab("Units per visits")
#density plot of reduction vs units per visits
ggplot(total, aes (x=REDUCTION_ratio, y=UNITS_PER_VISITS))+
  stat_density_2d(aes(x=REDUCTION_ratio, y=UNITS_PER_VISITS,fill=..level..), geom = "polygon", colour="white")+
  title("Reduction ratio and units per visits: density plot")+xlab("Reduction ratio")+ ylab("Units per visits")+
  theme(legend.position = "none")
#plot feature, display, tpr only, reduction ratio colored by category
#plot ggparcoord
table=total %>%
  group_by(SUB_CATEGORY) %>%
  summarise(feature_share=mean(FEATURE=="yes"),
            tpr_share=mean(TPR_ONLY=="yes"),
            display_share=mean(DISPLAY=="yes"),
            reduction_ratio=mean(REDUCTION_ratio),
            units_per_visits=mean(UNITS_PER_VISITS)) 
ggparcoord(table, columns=2:6, groupColumn=1, order ="allClass", scale="uniminmax", showPoints = TRUE, 
           title = "Plot of the types of promotion and the units per visit",
           alphaLines = 0.8, theme_ipsum())+ labs(x="Type of promotion", y=" ")
#promotion share of products and units per visit 
table2=total %>%
  group_by(UPC) %>%
  summarise(feature_share=mean(FEATURE=="yes"),
            tpr_share=mean(TPR_ONLY=="yes"),
            display_share=mean(DISPLAY=="yes"),
            reduction_ratio_share=mean(REDUCTION_ratio),
            units_per_visits_share=mean(UNITS_PER_VISITS)) 
#same table but for each category
table2_bagsnacks=total[which(total$CATEGORY=="BAG SNACKS"),] %>% #for bag snacks
  group_by(UPC) %>%
  summarise(feature_share=mean(FEATURE=="yes"),
            tpr_share=mean(TPR_ONLY=="yes"),
            display_share=mean(DISPLAY=="yes"),
            reduction_ratio_share=mean(REDUCTION_ratio),
            units_per_visits_share=mean(UNITS_PER_VISITS)) 
table2_oralhyg=total[which(total$CATEGORY=="ORAL HYGIENE PRODUCTS"),] %>% #for oral hygiene products
  group_by(UPC) %>%
  summarise(feature_share=mean(FEATURE=="yes"),
            tpr_share=mean(TPR_ONLY=="yes"),
            display_share=mean(DISPLAY=="yes"),
            reduction_ratio_share=mean(REDUCTION_ratio),
            units_per_visits_share=mean(UNITS_PER_VISITS)) 
table2_coldcer=total[which(total$CATEGORY=="COLD CEREAL"),] %>% #for cold cereals
  group_by(UPC) %>%
  summarise(feature_share=mean(FEATURE=="yes"),
            tpr_share=mean(TPR_ONLY=="yes"),
            display_share=mean(DISPLAY=="yes"),
            reduction_ratio_share=mean(REDUCTION_ratio),
            units_per_visits_share=mean(UNITS_PER_VISITS)) 
table2_frozenpizza=total[which(total$CATEGORY=="FROZEN PIZZA"),] %>% #for frozen pizza
  group_by(UPC) %>%
  summarise(feature_share=mean(FEATURE=="yes"),
            tpr_share=mean(TPR_ONLY=="yes"),
            display_share=mean(DISPLAY=="yes"),
            reduction_ratio_share=mean(REDUCTION_ratio),
            units_per_visits_share=mean(UNITS_PER_VISITS)) 
#scatter plot with linear trend and confidence interval of feature vs units per visits
ggplot(table2, aes (x=feature_share, y=units_per_visits_share))+
  geom_point()+
  geom_smooth(method=lm , color="red", fill="#00F7FF", se=TRUE) +
  theme_ipsum()+
  ggtitle("Units per visit and featuring")+
  xlab("Amount of featuring per product")+
  ylab("Units bought per visit")
ggplot(table2_bagsnacks, aes (x=feature_share, y=units_per_visits_share))+ #for bag snacks
  geom_point()+
  geom_smooth(method=lm , color="red", fill="#00F7FF", se=TRUE) +
  theme_ipsum()+
  ggtitle("Units per visit and featuring for bag snacks")+
  xlab("Amount of featuring per product")+
  ylab("Units bought per visit")
ggplot(table2_coldcer, aes (x=feature_share, y=units_per_visits_share))+ #for cold cereal
  geom_point()+
  geom_smooth(method=lm , color="red", fill="#00F7FF", se=TRUE) +
  theme_ipsum()+
  ggtitle("Units per visit and featuring for cold cereal")+
  xlab("Amount of featuring per product")+
  ylab("Units bought per visit")
ggplot(table2_frozenpizza, aes (x=feature_share, y=units_per_visits_share))+ #for frozen pizza
  geom_point()+
  geom_smooth(method=lm , color="red", fill="#00F7FF", se=TRUE) +
  theme_ipsum()+
  ggtitle("Units per visit and featuring for frozen pizza")+
  xlab("Amount of featuring per product")+
  ylab("Units bought per visit")
ggplot(table2_oralhyg, aes (x=feature_share, y=units_per_visits_share))+ #for oral hygiene products
  geom_point()+
  geom_smooth(method=lm , color="red", fill="#00F7FF", se=TRUE) +
  theme_ipsum()+
  ggtitle("Units per visit and featuring for oral hygiene products")+
  xlab("Amount of featuring per product")+
  ylab("Units bought per visit")
#scatter plot with linear trend and confidence interval of display vs units per visits
ggplot(table2, aes (x=display_share, y=units_per_visits_share))+
  geom_point()+  
  geom_smooth(method=lm , color="red", fill="#00F7FF", se=TRUE) +
  theme_ipsum()+
  ggtitle("Units per visit and promotional display")+
  xlab("Amount of displaying per product")+
  ylab("Units bought per visit")
ggplot(table2_bagsnacks, aes (x=display_share, y=units_per_visits_share))+
  geom_point()+  
  geom_smooth(method=lm , color="red", fill="#00F7FF", se=TRUE) +
  theme_ipsum()+
  ggtitle("Units per visit and promotional display for bag snacks")+
  xlab("Amount of displaying per product")+
  ylab("Units bought per visit")
ggplot(table2_coldcer, aes (x=display_share, y=units_per_visits_share))+
  geom_point()+  
  geom_smooth(method=lm , color="red", fill="#00F7FF", se=TRUE) +
  theme_ipsum()+
  ggtitle("Units per visit and promotional display for cold cereal")+
  xlab("Amount of displaying per product")+
  ylab("Units bought per visit")
ggplot(table2_frozenpizza, aes (x=display_share, y=units_per_visits_share))+
  geom_point()+  
  geom_smooth(method=lm , color="red", fill="#00F7FF", se=TRUE) +
  theme_ipsum()+
  ggtitle("Units per visit and promotional display for frozen pizza")+
  xlab("Amount of displaying per product")+
  ylab("Units bought per visit")
ggplot(table2_oralhyg, aes (x=display_share, y=units_per_visits_share))+
  geom_point()+  
  geom_smooth(method=lm , color="red", fill="#00F7FF", se=TRUE) +
  theme_ipsum()+
  ggtitle("Units per visit and promotional display for oral hygiene products")+
  xlab("Amount of displaying per product")+
  ylab("Units bought per visit")
#scatter plot with linear trend and confidence interval of tpr vs units per visits
ggplot(table2, aes (x=tpr_share, y=units_per_visits_share))+
  geom_point()+
  geom_smooth(method=lm , color="red", fill="#00F7FF", se=TRUE) +
  theme_ipsum()+
  ggtitle("Units per visit and temporary price reduction only")+
  xlab("Amount of temporary price reduction per product")+
  ylab("Units bought per visit")
ggplot(table2_bagsnacks, aes (x=tpr_share, y=units_per_visits_share))+
  geom_point()+
  geom_smooth(method=lm , color="red", fill="#00F7FF", se=TRUE) +
  theme_ipsum()+
  ggtitle("Units per visit and temporary price reduction only for bag snacks")+
  xlab("Amount of temporary price reduction per product")+
  ylab("Units bought per visit")
ggplot(table2_coldcer, aes (x=tpr_share, y=units_per_visits_share))+
  geom_point()+
  geom_smooth(method=lm , color="red", fill="#00F7FF", se=TRUE) +
  theme_ipsum()+
  ggtitle("Units per visit and temporary price reduction only for cold cereals")+
  xlab("Amount of temporary price reduction per product")+
  ylab("Units bought per visit")
ggplot(table2_frozenpizza, aes (x=tpr_share, y=units_per_visits_share))+
  geom_point()+
  geom_smooth(method=lm , color="red", fill="#00F7FF", se=TRUE) +
  theme_ipsum()+
  ggtitle("Units per visit and temporary price reduction only for frozen pizza")+
  xlab("Amount of temporary price reduction per product")+
  ylab("Units bought per visit")
ggplot(table2_oralhyg, aes (x=tpr_share, y=units_per_visits_share))+
  geom_point()+
  geom_smooth(method=lm , color="red", fill="#00F7FF", se=TRUE) +
  theme_ipsum()+
  ggtitle("Units per visit and temporary price reduction only for oral hygiene products")+
  xlab("Amount of temporary price reduction per product")+
  ylab("Units bought per visit")
#scatter plot with linear trend and confidence interval of reduction ratio vs units per visits
ggplot(table2, aes (x=reduction_ratio_share, y=units_per_visits_share))+
  geom_point()+
  geom_smooth(method=lm , color="red", fill="#00F7FF", se=TRUE) +
  theme_ipsum()+
  ggtitle("Units per visit and reduction ratio")+
  xlab("Amount of reduction ratio per product")+
  ylab("Units bought per visit")
ggplot(table2_bagsnacks, aes (x=reduction_ratio_share, y=units_per_visits_share))+
  geom_point()+
  geom_smooth(method=lm , color="red", fill="#00F7FF", se=TRUE) +
  theme_ipsum()+
  ggtitle("Units per visit and reduction ratio for bag snacks")+
  xlab("Amount of reduction ratio per product")+
  ylab("Units bought per visit")
ggplot(table2_coldcer, aes (x=reduction_ratio_share, y=units_per_visits_share))+
  geom_point()+
  geom_smooth(method=lm , color="red", fill="#00F7FF", se=TRUE) +
  theme_ipsum()+
  ggtitle("Units per visit and reduction ratio for cold cereals")+
  xlab("Amount of reduction ratio per product")+
  ylab("Units bought per visit")
ggplot(table2_frozenpizza, aes (x=reduction_ratio_share, y=units_per_visits_share))+
  geom_point()+
  geom_smooth(method=lm , color="red", fill="#00F7FF", se=TRUE) +
  theme_ipsum()+
  ggtitle("Units per visit and reduction ratio for frozen pizza")+
  xlab("Amount of reduction ratio per product")+
  ylab("Units bought per visit")
ggplot(table2_oralhyg, aes (x=reduction_ratio_share, y=units_per_visits_share))+
  geom_point()+
  geom_smooth(method=lm , color="red", fill="#00F7FF", se=TRUE) +
  theme_ipsum()+
  ggtitle("Units per visit and reduction ratio for oral hygiene products")+
  xlab("Amount of reduction ratio per product")+
  ylab("Units bought per visit")
#correlogram summarising the correlation between the variables 
variables=total[,(4:12)]
ggpairs(variables, title="Correlogram")
#visualisations of units per visits with and without each types of promotion types 
table3=total%>%
  group_by(CATEGORY, SUB_CATEGORY, UPC) %>%
  summarise(store_promotion_share=mean(STORE_PROMOTION=="yes"),
            price_promotion_share=mean(PRICE_PROMOTION=="yes"),
            store_promotion_share_inter_price=mean(PRICE_PROMOTION=="yes"|STORE_PROMOTION=="yes"),
            store_promotion_share_inter_noprice=mean(PRICE_PROMOTION=="yes"|STORE_PROMOTION=="no"),
            price_promotion_share_inter_store=mean(STORE_PROMOTION=="yes"|PRICE_PROMOTION=="yes"),
            price_promotion_share_inter_nostore=mean(STORE_PROMOTION=="yes"|PRICE_PROMOTION=="no"),
            units_per_visits_share=mean(UNITS_PER_VISITS))
table3 %>% #Units per visits with store promotion
  ggplot( aes(x=store_promotion_share, y=units_per_visits_share, group=SUB_CATEGORY, color=CATEGORY)) +
  geom_line()+
  ggtitle("Units per visits and in store promotion (displays and/or features)")+
  labs(x="Amount of in store promotion", y = "Units per visits")
table3 %>% #Units per visits with price promotion
  ggplot( aes(x=price_promotion_share, y=units_per_visits_share, group=SUB_CATEGORY, color=CATEGORY)) +
  geom_line()+
  ggtitle("Units per visits and price promotion (temporary price reduction and/or discounts")+
  labs(x="Amount of price promotion", y = "Units per visits")
table3 %>% #Units per visits and price promotion when there is also store promotion
  ggplot( aes(x=price_promotion_share_inter_store, y=units_per_visits_share, group=SUB_CATEGORY, color=CATEGORY)) +
  geom_line()+
  ggtitle("Units per visits and price promotion for products with also in store promotion")+
  labs(x="Amount of price promotion for products with also in store promotion", y = "Units per visits")
table3 %>% #Units per visits and price promotion when there is no store promotion
  ggplot( aes(x=price_promotion_share_inter_nostore, y=units_per_visits_share, group=SUB_CATEGORY, color=CATEGORY)) +
  geom_line()+
  ggtitle("Units per visits and price promotion for products with no in store promotion")+
  labs(x="Amount of price promotion for products with no in store promotion", y = "Units per visits")
table3 %>% #Units per visits and store promotion when there is also price promotion
  ggplot( aes(x=store_promotion_share_inter_price, y=units_per_visits_share, group=SUB_CATEGORY, color=CATEGORY)) +
  geom_line()+
  ggtitle("Units per visits and store promotion for products with price promotion")+
  labs(x="Amount of store promotion for products with price promotion", y = "Units per visits")
table3 %>% #Units per visits and store promotion when there is no price promotion
  ggplot( aes(x=store_promotion_share_inter_noprice, y=units_per_visits_share, group=SUB_CATEGORY, color=CATEGORY)) +
  geom_line()+
  ggtitle("Units per visits and store promotion for products with no price promotion")+
  labs(x="Amount of store promotion for products with no price promotion", y = "Units per visits")
table3 %>% #In store promotion plus price reduction and units per visits
  ggplot( aes(x=store_promotion_share+price_promotion_share, y=units_per_visits_share, group=SUB_CATEGORY, color=CATEGORY)) +
  geom_line()+
  ggtitle("Units per visits and promotion")+
  labs(x="Amount of promotion", y = "Units per visits")
#3D plotting of store promotion and price promotion and units per visits
shares<-total%>%
  group_by(STORE_NUM, CATEGORY) %>%
  summarise(store_promotion_share=mean(STORE_PROMOTION=="yes"),
            price_promotion_share=mean(PRICE_PROMOTION=="yes"),
            store_promotion_share_inter_price=mean(PRICE_PROMOTION=="yes"|STORE_PROMOTION=="yes"),
            store_promotion_share_inter_noprice=mean(PRICE_PROMOTION=="yes"|STORE_PROMOTION=="no"),
            price_promotion_share_inter_store=mean(STORE_PROMOTION=="yes"|PRICE_PROMOTION=="yes"),
            price_promotion_share_inter_nostore=mean(STORE_PROMOTION=="yes"|PRICE_PROMOTION=="no"),
            units_per_visits_share=mean(UNITS_PER_VISITS))
mycolors<- rainbow(4)
shares$color <- mycolors[ as.numeric(shares$CATEGORY) ]
plot3d(x=shares$store_promotion_share, y=shares$price_promotion_share, z=shares$units_per_visits_share,
       col=mycolors, 
       type = 's', 
       radius = .005,
       xlab="In store promotion", ylab="Price promotion", zlab="Units per visits")
par3d(windowRect = c(2,2,512, 512))
legend3d("topright", legend = paste("Product category", c("Bag snacks","Cold cereal", "Frozen pizza", "Oral hygiene products")), pch = 16, col = mycolors, cex=1, inset=c(0.002))
#random forest to predict units per visits depending with reduction ratio
data1<- total[,c(20,18)]
str((data1))
train <- sample(nrow(data1), 0.7*nrow(data1), replace = FALSE)
TrainSet <- data1[train,]
ValidSet <- data1[-train,]
TrainSet<-TrainSet[sample(nrow(TrainSet), 5000),] #the data sets are too big to use them entirely
model1 <- randomForest(UNITS_PER_VISITS~., data = TrainSet, importance = TRUE)
predTrain <- predict(model1, TrainSet, type = "class")
RMSE(predTrain, ValidSet$UNITS_PER_VISITS)


#MAPPING PROMOTION AND UNITS PER VISITS
#creating a data set with the average amount of promotion types 
table4<-total
table4<-table4%>%
  group_by(STORE_NUM) %>%
  summarise(store_promotion_share=mean(STORE_PROMOTION=="yes"),
            price_promotion_share=mean(PRICE_PROMOTION=="yes"),
            store_promotion_share_inter_price=mean(PRICE_PROMOTION=="yes"|STORE_PROMOTION=="yes"),
            store_promotion_share_inter_noprice=mean(PRICE_PROMOTION=="yes"|STORE_PROMOTION=="no"),
            price_promotion_share_inter_store=mean(STORE_PROMOTION=="yes"|PRICE_PROMOTION=="yes"),
            price_promotion_share_inter_nostore=mean(STORE_PROMOTION=="yes"|PRICE_PROMOTION=="no"),
            units_per_visits_share=mean(UNITS_PER_VISITS))
#creating a data set with the average amount of promotion of each store and there location
store_and_ratios<-left_join(table4,store_location)
#plotting units per visits and store promotion
all+geom_point(data=store_and_ratios, aes(x=Longitude, y=Latitude, size=units_per_visits_share, color=store_promotion_share))
texas+geom_point(data=store_and_ratios, aes(x=Longitude, y=Latitude, size=units_per_visits_share, color=store_promotion_share))
OH_IN_KY+geom_point(data=store_and_ratios, aes(x=Longitude, y=Latitude, size=units_per_visits_share, color=store_promotion_share))


#LINEAR REGRESSION FOR DEMAND FORECASTING 
#training of a linear regression model to predict units per visits depending on the advertisement 
lm.units_per_visits=lm(UNITS_PER_VISITS~FEATURE+DISPLAY+TPR_ONLY+REDUCTION_ratio,data=total_train)
summary(lm.units_per_visits)
#regression tree to predict units per visits depending on promotion
tree.units_per_visits=rpart(UNITS_PER_VISITS~FEATURE+DISPLAY+TPR_ONLY+REDUCTION_ratio,data=total_train)
rpart.plot(tree.units_per_visits,)
text(tree.units_per_visits,cex=0.5)
#linear regressions to predict units per visits vs store promotion with price promotion as an interaction term
lm.interaction=lm(UNITS_PER_VISITS~STORE_PROMOTION*PRICE_PROMOTION,data=total)
summary(lm.interaction)
#Linear regression for demand forecasting
lm.fit = lm(UNITS ~ PRICE + FEATURE + DISPLAY + REDUCTION + TPR_ONLY+ HHS + REDUCTION_ratio,
            data= total_train)
summary(lm.fit)
demandpred <- predict(lm.fit, total_test)
rmse(test$UNITS, demandpred)
#Lets get in sample RMSE, by comparing train and test data.
plot(lm.fit)
demandpredicttrain <- predict(lm.fit, total_train)
rmse(total_train$UNITS, demandpredicttrain)
#I'm going to try and repeat this, but for each product category, so we can see if features have more impact on cereal than on bag snacks.
BagSnack <- filter(total,total$CATEGORY == "BAG SNACKS")
#Now, separate linear regression models for each product category provide more specific representation
BagSnack$REDUCTION<-BagSnack$BASE_PRICE-BagSnack$PRICE
BagSnack$REDUCTION_ratio<-(BagSnack$BASE_PRICE-BagSnack$PRICE)/(BagSnack$BASE_PRICE)
BagSnack.fit = lm(UNITS ~ PRICE + FEATURE + DISPLAY + TPR_ONLY+ HHS + REDUCTION + REDUCTION_ratio,
                  data= BagSnack)
summary(BagSnack.fit)
plot(BagSnack.fit)
#Next, Frozen Pizza
FrozenPizza <- filter(total,total$CATEGORY == "FROZEN PIZZA")
FrozenPizza$REDUCTION<-FrozenPizza$BASE_PRICE-FrozenPizza$PRICE
FrozenPizza$REDUCTION_ratio<-(FrozenPizza$BASE_PRICE-FrozenPizza$PRICE)/(FrozenPizza$BASE_PRICE)
FrozenPizza.fit = lm(UNITS ~ PRICE + FEATURE + DISPLAY + TPR_ONLY+ HHS + REDUCTION + REDUCTION_ratio,
                     data= FrozenPizza)
summary(FrozenPizza.fit)
#Again for COLD CEREAL:
ColdCereal <- filter(total,total$CATEGORY == "COLD CEREAL")
ColdCereal$REDUCTION<-ColdCereal$BASE_PRICE-ColdCereal$PRICE
ColdCereal$REDUCTION_ratio<-(ColdCereal$BASE_PRICE-ColdCereal$PRICE)/(ColdCereal$BASE_PRICE)
ColdCereal.fit = lm(UNITS ~ PRICE + FEATURE + DISPLAY + TPR_ONLY+ HHS + REDUCTION + REDUCTION_ratio,
                    data= ColdCereal)
summary(ColdCereal.fit)
#Lastly for Oral Hygiene Products
OralHygieneProducts <- filter(total,total$CATEGORY == "ORAL HYGIENE PRODUCTS")
OralHygieneProducts$REDUCTION<-OralHygieneProducts$BASE_PRICE-OralHygieneProducts$PRICE
OralHygieneProducts$REDUCTION_ratio<-(OralHygieneProducts$BASE_PRICE-OralHygieneProducts$PRICE)/(OralHygieneProducts$BASE_PRICE)
OralHygieneProducts.fit = lm(UNITS ~ PRICE + FEATURE + DISPLAY + TPR_ONLY+ HHS + REDUCTION + REDUCTION_ratio,
                             data= OralHygieneProducts)
summary(OralHygieneProducts.fit)
#Let's make a regression tree for the same data, determining the importance of values. 
tree.fit = rpart(UNITS~PRICE+REDUCTION_ratio+REDUCTION+FEATURE+TPR_ONLY+DISPLAY, data=total_train,
                 method="anova")
# plot tree output
tree.fit = rpart(UNITS~PRICE+REDUCTION_ratio+REDUCTION+FEATURE+TPR_ONLY+DISPLAY, data=total_train,
                 method="anova")
rpart.plot(tree.fit,type=3,digits=3,fallen.leaves=TRUE)
#let's find rmse from regression tree
RegressionTreePredictTrain<- predict(tree.fit,total_train)
RegressionTreePredictTest<- predict(tree.fit,total_test)
#in sample rmse
rmse(total_train$UNITS, RegressionTreePredictTrain)
#predictive rmse
rmse(total_test$UNITS,RegressionTreePredictTest)
#plotting regression tree prediction vs actual values
ggplot()  + 
  geom_point(aes(RegressionTreePredictTest, total_test$UNITS)) + 
  geom_smooth(aes(RegressionTreePredictTest, total_test$UNITS), method = "lm", se = FALSE, color = "lightgrey") + 
  labs(x = "Predicted Units sold", y = "Actual Units sold") + 
  theme_bw()

#NEURAL NETWORK FOR DEMAND FORECASTING 
#preprocessing the data
maxs1 <- apply(total_train[c(4:12,18:20)], 2, max) 
mins1 <- apply(total_train[c(4:12,18:20)], 2, min)
maxs2 <- apply(total_test[c(4:12,18:20)], 2, max) 
mins2 <- apply(total_test[c(4:12,18:20)] , 2, min)
trains <- as.data.frame(scale(total_train[c(4:12,18:20)], center = mins1, scale = maxs1 - mins1))
tests <- as.data.frame(scale(total_test[c(4:12,18:20)], center = mins2, scale = maxs2 - mins2))
sample_train<- trains[sample(nrow(trains), 10000), ] #as the data set is too large, we decide to run it for sample of 10000 rows
#plotting the neuralnet
nns = neuralnet(UNITS ~ PRICE + FEATURE + DISPLAY + TPR_ONLY + REDUCTION , data= sample_train , hidden=2, threshold= 0.01, stepmax = 1000000)
plot(nns)
#prediction
pr.nn1 <- neuralnet :: compute(nns,trains)
pr.nn2 <- neuralnet :: compute(nns,tests)
#error
rmse(trains$UNITS, pr.nn1$net.result)
rmse(tests$UNITS,pr.nn2$net.result)

#DEMAND FORECASTING WITH A LASSO MODEL
grid = 10^seq(-5,0,length=100)
trainX = total_train[,c("PRICE","DISPLAY","FEATURE","TPR_ONLY", "REDUCTION")]
y<- total_train$UNITS
trainX = as.matrix(trainX)
lasso = glmnet(trainX,total_train$UNITS) 
plot(lasso,xvar="lambda", label= TRUE)
testX = total_test[,c("PRICE","DISPLAY","FEATURE","TPR_ONLY", "REDUCTION")]
#cross validation
cvfit = cv.glmnet(trainX, total_train$UNITS, alpha=1)
plot(cvfit)
bestlambda= cvfit$lambda.min
#prediction
testX = as.matrix(testX)
lassopred1 <- predict(lasso,s= bestlambda, newx=trainX)
lassopred2 <- predict(lasso,s= bestlambda, newx=testX)
cvfit$lambda.min
cvfit$lambda.1se
#in sample rmse- actual train vs predicted train
rmse(total_train$UNITS, lassopred1)
rmse(total_test$UNITS, lassopred2)
#plotting lasso regression vs actual values
theme_set(theme_cowplot())
ggplot()  + 
  geom_point(aes(lassopred2, test$UNITS)) + 
  geom_smooth(aes(lassopred2, test$UNITS), method = "lm", se = FALSE, color = "lightgrey") + 
  labs(x = "Predicted Units sold", y = "Actual Units sold") + 
  theme_bw()