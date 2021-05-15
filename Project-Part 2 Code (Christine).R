#Christine's pt. 2 model 1#

##SETUP##


#INSTALL THE GGPLOT2 PACKAGE (MUST BE DONE ONCE)
install.packages('ggplot2')

#LOAD THE GGPLOT2 LIBRARY (MUST BE DONE EVERY TIME)
library(ggplot2)

#TRYING TO MODEL THE RELATIONSHIP BETWEEN ANNUAL HOURLY WAGE AND TYPICAL $ VALUE OF 3 BEDROOM HOMES
#INCORPORATING NONLINEAR (POLYNOMIAL) TRANSFORMATIONS OF homeval
acct$homeval2<-acct$homeval^2 #QUADRATIC TRANSFORMATION (2nd ORDER)
acct$homeval3<-acct$homeval^3
acct$homeval4<-acct$homeval^4
acct$homeval5<-acct$homeval^5
acct$ln_homeval<-log(acct$homeval)
acct$emp2<-acct$emp^2
acct$emp3<-acct$emp^3
acct$emp4<-acct$emp^4
acct$emp5<-acct$emp^5
acct$ln_emp<-log(acct$emp)

#fraction of sample to be used for training
p<-.7 #use 70% of the data to train/build the model

#number of observations (rows) in the dataframe
obs_count<-dim(acct)[1]

#number of observations to be selected for the training partition
#the floor() function rounds down to the nearest integer
training_size <- floor(p * obs_count)
training_size

#set the seed to make your partition reproducible
set.seed(1234)

#create a vector with the shuffled row numbers of the original dataset
train_ind <- sample(obs_count, size = training_size)

Training <- acct[train_ind, ] #pulls random rows for training
Testing <- acct[-train_ind, ] #pulls random rows for testing




##CHRISTINE'S MODEL 1##
#BUILDING THE QUADRATIC MODEL FROM THE TRAINING DATA
M1 <- lm(Hourlywage ~ homeval + homeval2, Training)
summary(M1) #generates summary diagnostic output

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_1_IN <- predict(M1, Training) #generate predictions on the (in-sample) training data

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_1_OUT <- predict(M1, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_1_IN<-sqrt(sum((PRED_1_IN-Training$Hourlywage)^2)/length(PRED_1_IN))  #computes in-sample error
RMSE_1_OUT<-sqrt(sum((PRED_1_OUT-Testing$Hourlywage)^2)/length(PRED_1_OUT)) #computes out-of-sample 

RMSE_1_IN #IN-SAMPLE ERROR
RMSE_1_OUT #OUT-OF-SAMPLE ERROR

#PLOTTING THE MODEL IN 2D AGAINST BOTH DATA PARTITIONS
x_grid <- seq(0,1600000,1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(M1, list(homeval=x_grid, homeval2=x_grid^2))
plot(Training$Hourlywage ~ Training$homeval, xlim= c(95000,1000000), main= "Mean Hourly Wage vs. Typical Home Value", sub=
       "Quadratic Model", xlab= "Typical Home Value", ylab= "Mean Hourly Wage", col='blue')
lines(x_grid, predictions, col='green', lwd=3)
points(Testing$Hourlywage ~ Testing$homeval, col='red', pch=3)
legend("bottomright", legend = c("Testing Data Points","Training Data","Model"), fill=c("red", "blue", "green"), bg="orange", title="Legend")

#Christine's Model #2
M2 <- lm(Hourlywage ~ emp4 + emp3 + emp2 + emp + homeval2 + homeval , Training)
summary(M2)
PRED_2_IN <- predict(M2, Training)
PRED_2_OUT <- predict(M2, Testing)
RMSE_2_IN<-sqrt(sum((PRED_2_IN-Training$Hourlywage)^2)/length(PRED_2_IN))
RMSE_2_OUT<-sqrt(sum((PRED_2_OUT-Testing$Hourlywage)^2)/length(PRED_2_OUT))
RMSE_2_IN
RMSE_2_OUT

#can't plot multivariate model# - don't need plots for multivariate cases


