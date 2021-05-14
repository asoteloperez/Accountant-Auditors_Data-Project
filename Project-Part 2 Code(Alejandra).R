####Plotting a linear model-Alejandra####

library(ggplot2)

##import dataset##

#fraction of sample to be used for training
p<-.7 #use 70% of the data to train/build the model#########

#number of observations (rows) in the dataframe
obs_count<-dim(df)[1]

#number of observations to be selected for the training partition
#the floor() function rounds down to the nearest integer
training_size <- floor(p * obs_count)
training_size
#set the seed to make your partition reproducible
set.seed(1234)
#create a vector with the shuffled row numbers of the original dataset
train_ind <- sample(obs_count, size = training_size)

Training <- df[train_ind, ] #pulls random rows for training
Testing <- df[-train_ind, ] #pulls random rows for testing

#CHECKING THE DIMENSIONS OF THE PARTITIONED DATA
dim(Training)
dim(Testing)

#PLOTS THE IN-SAMPLE TRAINING PARTITION

plot(Hourlywage ~ homeval, Testing, xlim=c(10000,500000), ylim=c(10,70),  col ='red', pch=3) #PLOTS THE OUT-OF-SAMPLE TESTING PARTITION
points(Training$homeval, Training$Hourlywage, col='blue') #PLOTS THE OUT-OF-SAMPLE TESTING PARTITION
points(Testing$homeval, Testing$Hourlywage, col='red', pch=3) #PLOTS THE OUT-OF-SAMPLE TESTING PARTITION


#BUILDING THE MODEL FROM THE TRAINING DATA
M1 <- lm(Hourlywage ~ homeval, Training)
summary(M1) #generates summary diagnostic output

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_1_IN <- predict(M1, Training) #generate predictions on the (in-sample) training data
View(PRED_1_IN)
View(M1$fitted.values) #these are the same as the fitted values

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_1_OUT <- predict(M1, Testing) #generate predictions on the (out-of-sample) testing data (red points)

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_1_IN<-sqrt(sum((PRED_1_IN-Training$Hourlywage)^2)/length(PRED_1_IN))  #computes in-sample error
RMSE_1_OUT<-sqrt(sum((PRED_1_OUT-Testing$Hourlywage)^2)/length(PRED_1_OUT)) #computes out-of-sample 

RMSE_1_IN #IN-SAMPLE ERROR
RMSE_1_OUT #OUT-OF-SAMPLE ERROR

#PLOTTING THE MODEL IN 2D AGAINST BOTH DATA PARTITIONS

x_grid <- seq(0,15000000,1)  #CREATES GRID OF X-AXIS VALUES
predictions <- predict(M1, list(homeval=x_grid))
plot(Training$Hourlywage ~ Training$homeval, col='blue', xlab="Home Value", ylab="Hourly Wage", main= "Mean Hourly Wage vs. Typical Home Value")
lines(x_grid, predictions, col='green', lwd=3)
points(Testing$Hourlywage ~ Testing$homeval, col='red', pch=3) 
legend ("bottomright", legend= c("Testing Data Points","Training Data Points","Model"), fill= c("red", "blue", "green"), bg= "orange", title= "Legend")


################################################
#####Plotting with more than one x variable#####
################################################

library(ggplot2)

##import dataset##

#fraction of sample to be used for training
p<-.7 #use 70% of the data to train/build the model#########

#number of observations (rows) in the dataframe
obs_count<-dim(df)[1]

#number of observations to be selected for the training partition
#the floor() function rounds down to the nearest integer
training_size <- floor(p * obs_count)
training_size
#set the seed to make your partition reproducible
set.seed(1234)
#create a vector with the shuffled row numbers of the original dataset
train_ind <- sample(obs_count, size = training_size)

Training <- df[train_ind, ] #pulls random rows for training
Testing <- df[-train_ind, ] #pulls random rows for testing

#CHECKING THE DIMENSIONS OF THE PARTITIONED DATA
dim(Training)
dim(Testing)


#BUILDING THE MODEL FROM THE TRAINING DATA
M2 <- lm(Hourlywage ~ homeval+ Employment, Training)
summary(M2) #generates summary diagnostic output

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_1_IN_2 <- predict(M2, Training) #generate predictions on the (in-sample) training data
View(PRED_1_IN_2)
View(M2$fitted.values) #these are the same as the fitted values

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_1_OUT_2 <- predict(M2, Testing) #generate predictions on the (out-of-sample) testing data (red points)

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_1_IN_2<-sqrt(sum((PRED_1_IN_2-Training$Hourlywage)^2)/length(PRED_1_IN_2))  #computes in-sample error
RMSE_1_OUT_2<-sqrt(sum((PRED_1_OUT_2-Testing$Hourlywage)^2)/length(PRED_1_OUT_2)) #computes out-of-sample 

RMSE_1_IN_2 #IN-SAMPLE ERROR
RMSE_1_OUT_2 #OUT-OF-SAMPLE ERROR





