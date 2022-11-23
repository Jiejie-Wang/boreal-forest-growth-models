#******************************************************************************
# Purpose: R script for fitting species-specific BRT models
#
# Author: Jiejie Wang (2022-11-23)
#
#******************************************************************************


library(data.table)
library(caret)
library(gbm)
library(hydroGOF)
library(Metrics)
library(plot3Drgl)
library(reshape2)
library(dismo)
library(pdp)
library(ggpubr)



## read data 
summary(tree_data_modelfit)



#Fit species-specific brt models 


# balsam fir
balsam=tree_data_modelfit[tree_data_modelfit$SpeciesFIA==12,]
balsam=balsam[balsam$BAI<=(quantile(balsam$BAI, 0.75)+ 3*IQR(balsam$BAI)),]



# set up training and testing dataset
set.seed(1)
n <- nrow(balsam)

# number of rows for the training set (80% of the dataset)
n_train <- round(0.80 * n)

# create a vector of indices which is an 80% random sample
train_indices <- sample(1:n, n_train)


# subset the data frame to training indices only
balsam_train <- balsam[train_indices, ]  
nrow(balsam_train) 

# exclude the training indices to create the test set
balsam_test <- balsam[-train_indices, ]
nrow(balsam_test)


balsam_model_1 <- gbm(formula =  log(BAI+1) ~ Max_ST + Min_WT + MSP + PAS + FFP+
                        BA9 +TreeBA+ Moist + Slope + Aspect_Cat + CO2,
                      data = balsam_train,
                      shrinkage = 0.05,
                      interaction.depth = 2,
                      n.trees =1000,
                      n.minobsinnode = 10,
                      cv=10)

summary(balsam_model_1)
# goodness-of-fit 
cor(balsam_model_1$fit, log(balsam_train$BAI+1))^2 

# predict the model ---------------------------------------
predict_balsam_train=predict.gbm(object=balsam_model_1, newdata=balsam_train, n.trees=1000)
rmse(log(balsam_train$BAI+1), predict_balsam_train)/(max(predict_balsam_train)-min(predict_balsam_train))


predict_balsam_test <- predict.gbm(object=balsam_model_1, newdata=balsam_test, n.trees=1000)
rmse(log(balsam_test$BAI+1), predict_balsam_test)/(max(predict_balsam_test)-min(predict_balsam_test)) 

cor(predict_balsam_test, log(balsam_test$BAI+1))^2  






# black spruce

black=tree_data_modelfit[tree_data_modelfit$SpeciesFIA==95,]
black=black[black$BAI<=(quantile(black$BAI, 0.75)+ 3*IQR(black$BAI)),]



## set up training and testing dataset
n <- nrow(black)

# number of rows for the training set (80% of the dataset)
n_train <- round(0.80 * n)

# create a vector of indices which is an 80% random sample
train_indices <- sample(1:n, n_train)


# subset the data frame to training indices only
black_train <- black[train_indices, ]  
nrow(black_train) 

# exclude the training indices to create the test set
black_test <- black[-train_indices, ]
nrow(black_test)  



# Model for black spruce
black_model_1 <- gbm(formula =  log(BAI+1) ~ Max_ST + Min_WT + MSP + PAS + FFP+
                       BA9+TreeBA+ Moist + Slope + Aspect_Cat+CO2,
                     data = black_train,
                     shrinkage = 0.05,
                     interaction.depth = 3,
                     n.trees =1000,
                     n.minobsinnode = 5,
                     cv=10)

summary(black_model_1)
## goodness-of-fit 
cor(black_model_1$fit, log(black_train$BAI+1))^2 

## predict the model ---------------------------------------
predict_black_train=predict.gbm(object=black_model_1, newdata=black_train, n.trees=1000)
rmse(log(black_train$BAI+1), predict_black_train)/(max(predict_black_train)-min(predict_black_train))


predict_black_test <- predict.gbm(object=black_model_1, newdata=black_test, n.trees=1000)
rmse(log(black_test$BAI+1), predict_black_test)/(max(predict_black_test)-min(predict_black_test))

cor(predict_black_test, log(black_test$BAI+1))^2 






# white spruce
white=tree_data_modelfit[tree_data_modelfit$SpeciesFIA==94,]
white=white[white$BAI<=(quantile(white$BAI, 0.75)+ 3*IQR(white$BAI)),]


# set up training and testing dataset
n <- nrow(white)

# number of rows for the training set (80% of the dataset)
n_train <- round(0.80 * n)

# create a vector of indices which is an 80% random sample
set.seed(1) 
train_indices <- sample(1:n, n_train)


# subset the data frame to training indices only
white_train <- white[train_indices, ]  
nrow(white_train)  

# exclude the training indices to create the test set
white_test <- white[-train_indices, ]
nrow(white_test) 



# Model for white spruce
white_model_1 <- gbm(formula =  log(BAI+1) ~ Max_ST + Min_WT + MSP + PAS + FFP+
                       BA9+TreeBA+ Moist + Slope + Aspect_Cat+CO2, 
                     data = white_train,
                     shrinkage = 0.05,
                     interaction.depth = 3,
                     n.trees =1000,
                     n.minobsinnode = 5,
                     cv=10)

summary(white_model_1)
# goodness-of-fit 
cor(white_model_1$fit, log(white_train$BAI+1))^2  

# predict the model ---------------------------------------
predict_white_train=predict.gbm(object=white_model_1, newdata=white_train, n.trees=1000)
rmse(log(white_train$BAI+1), predict_white_train)/(max(predict_white_train)-min(predict_white_train))


predict_white_test <- predict.gbm(object=white_model_1, newdata=white_test, n.trees=1000)
rmse(log(white_test$BAI+1), predict_white_test)/(max(predict_white_test)-min(predict_white_test)) 

cor(predict_white_test, log(white_test$BAI+1))^2 






# white birch 
birch=tree_data_modelfit[tree_data_modelfit$SpeciesFIA==375,]
birch=birch[birch$BAI<=(quantile(birch$BAI, 0.75)+ 3*IQR(birch$BAI)),]


# set up training and testing dataset
# total number of rows in the restaurant data frame
n <- nrow(birch)

# number of rows for the training set (80% of the dataset)
n_train <- round(0.80 * n)

# create a vector of indices which is an 80% random sample
train_indices <- sample(1:n, n_train)


# subset the data frame to training indices only
birch_train <- birch[train_indices, ]  
nrow(birch_train)  

# exclude the training indices to create the test set
birch_test <- birch[-train_indices, ]
nrow(birch_test) 



# Model for birch
birch_model_1 <- gbm(formula =  log(BAI+1) ~ Max_ST + Min_WT + MSP + PAS + FFP+
                       BA9+TreeBA+ Moist + Slope + Aspect_Cat+CO2, 
                     data = birch_train,
                     shrinkage = 0.05,
                     interaction.depth = 3,
                     n.trees =1000,
                     n.minobsinnode = 5,
                     cv=10)

summary(birch_model_1)
## goodness-of-fit 
cor(birch_model_1$fit, log(birch_train$BAI+1))^2  

## predict the model ---------------------------------------
predict_birch_train=predict.gbm(object=birch_model_1, newdata=birch_train, n.trees=1000)
rmse(log(birch_train$BAI+1), predict_birch_train)/(max(predict_birch_train)-min(predict_birch_train))


predict_birch_test <- predict.gbm(object=birch_model_1, newdata=birch_test, n.trees=1000)
rmse(log(birch_test$BAI+1), predict_birch_test)/(max(predict_birch_test)-min(predict_birch_test)) 

cor(predict_birch_test, log(birch_test$BAI+1))^2 





# jack pine
jack=tree_data_modelfit[tree_data_modelfit$SpeciesFIA==105,]
jack=jack[jack$BAI<=(quantile(jack$BAI, 0.75)+ 3*IQR(jack$BAI)),]

# set up training and testing dataset
# total number of rows in the restaurant data frame
n <- nrow(jack)

# number of rows for the training set (80% of the dataset)
n_train <- round(0.80 * n)

# create a vector of indices which is an 80% random sample
set.seed(1)
train_indices <- sample(1:n, n_train)


# subset the data frame to training indices only
jack_train <- jack[train_indices, ]  
nrow(jack_train)  

# exclude the training indices to create the test set
jack_test <- jack[-train_indices, ]
nrow(jack_test) 



# Model for jack pine
jack_model_1 <- gbm(formula =  log(BAI+1) ~ Max_ST + Min_WT + MSP + PAS + FFP+
                      BA9 +TreeBA+ Moist + Slope + Aspect_Cat+CO2, 
                    data = jack_train,
                    shrinkage = 0.05,
                    interaction.depth = 3,
                    n.trees =2000,
                    n.minobsinnode = 5,
                    cv=10)

summary(jack_model_1)
## goodness-of-fit 
cor(jack_model_1$fit, log(jack_train$BAI+1))^2  

## predict the model ---------------------------------------
predict_jack_train=predict.gbm(object=jack_model_1, newdata=jack_train, n.trees=1000)
rmse(log(jack_train$BAI+1), predict_jack_train)/(max(predict_jack_train)-min(predict_jack_train))


predict_jack_test <- predict.gbm(object=jack_model_1, newdata=jack_test, n.trees=1000)
rmse(log(jack_test$BAI+1), predict_jack_test)/(max(predict_jack_test)-min(predict_jack_test)) 

cor(predict_jack_test, log(jack_test$BAI+1))^2  




# aspen

aspen=tree_data_modelfit[tree_data_modelfit$SpeciesFIA==746,]
aspen=aspen[aspen$BAI<=(quantile(aspen$BAI, 0.75)+ 3*IQR(aspen$BAI)),]



# set up training and testing dataset
# total number of rows in the restaurant data frame
n <- nrow(aspen)

# number of rows for the training set (80% of the dataset)
n_train <- round(0.80 * n)

# create a vector of indices which is an 80% random sample
set.seed(1) 
train_indices <- sample(1:n, n_train)


# subset the data frame to training indices only
aspen_train <- aspen[train_indices, ]  
nrow(aspen_train)  

# exclude the training indices to create the test set
aspen_test <- aspen[-train_indices, ]
nrow(aspen_test) 



# Model for aspens 
aspen_model_1 <- gbm(formula =  log(BAI+1) ~ Max_ST + Min_WT + MSP + PAS + FFP+
                       BA9+TreeBA+ Moist + Slope + Aspect_Cat+CO2, 
                     data = aspen_train,
                     shrinkage = 0.05,
                     interaction.depth = 3,
                     n.trees =1200,
                     n.minobsinnode = 5,
                     cv=10)

summary(aspen_model_1)
# goodness-of-fit 
cor(aspen_model_1$fit, log(aspen_train$BAI+1))^2  

## predict the model ---------------------------------------
predict_aspen_train=predict.gbm(object=aspen_model_1, newdata=aspen_train, n.trees=1000)
rmse(log(aspen_train$BAI+1), predict_aspen_train)/(max(predict_aspen_train)-min(predict_aspen_train))


predict_aspen_test <- predict.gbm(object=aspen_model_1, newdata=aspen_test, n.trees=1000)
rmse(log(aspen_test$BAI+1), predict_aspen_test)/(max(predict_aspen_test)-min(predict_aspen_test))

cor(predict_aspen_test, log(aspen_test$BAI+1))^2  



# End of script
#******************************************************************************