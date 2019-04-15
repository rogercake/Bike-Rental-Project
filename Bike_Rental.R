#Remove all objects stored
rm(list = ls())



#set working directory
setwd("E:/R/Project_2")

#Loading dataset in csv format
library(readr)
df <- read_csv("day.csv", col_names = T)
View(df)
str(df)
head(df)
nrow(df)
ncol(df)
dim(df)
data.class(df)
names(df)

#Removing unnecessary variables from our dataframe,which might not be useful for our analysis
  #Removing "instant" Variable as it contains recorded index numbers i.e 1st Column
  #Removing "dteday" as we already have month year and weekday and columns for our analysis.
df_new <- df[,c(-1,-2)]
View(df_new)
str(df_new)
dim(df_new)
names(df_new)
#Changing Column names
library(data.table)
setnames(df_new, old=c("yr","mnth", "hum", "cnt"), new=c("year", "month", "humidity","count"))
names(df_new)
View(df_new)
dim(df_new)

#Checking for duplicate columns
library(dplyr)
df_new %>% distinct() %>% dim()
#No duplicate columns present


########################################Exploratory Data Analysis######################################################
library(DataExplorer)
library(ggplot2)
library(ggpubr)
library(Hmisc)
library(ggpmisc)
theme_set(theme_pubr())

##Variable Identification
Hmisc::describe(df_new)

continuous_vars <-  c("temp", "atemp", "humidity", "windspeed", "casual", "registered")

categorical_vars <-  c("season", "year", "month", "holiday", "weekday", "workingday", "weathersit")

target_var <- c("count")


########################################Univariate Analysis######################################################
#Creating new copy of dataset for visualization purpose


##For Continuous Variables##

##temp

    #Converting temperature into celsius scale adn saving as new column in df
celsius_scale <- function(x, t_max, t_min) {
  temperature = (t_max - t_min)*x + t_min
}
df$temp_celsius <- celsius_scale(df_new$temp, 39, -8)
df$temp_celsius <- round(df$temp_celsius, digits = 2)

hist(df$temp_celsius, col = "deepskyblue",border = F,probability = T,main = "",
                      xlab = "Temperature(Celsius)", breaks = 40)
lines(density(df$temp_celsius), lwd = 2, col = "red")

##atemp
#Converting atemp into celsius scale and saving as new column in df
a_celsius_scale <- function(x, t_max, t_min) {
  temperature = (t_max - t_min)*x + t_min
}
df$atemp_celsius <- a_celsius_scale(df_new$atemp, 50, -16)
df$atemp_celsius <- round(df$atemp_celsius, digits = 2)

hist(df$atemp_celsius, col = "deepskyblue",border = F,probability = T,main = "",
     xlab = "A_Temperature(Celsius)", breaks = 40)
lines(density(df$atemp_celsius), lwd = 2, col = "red")

##humidity
df$actual_hum <- df$hum * 100
hist(df$actual_hum, col = "deepskyblue",border = F,probability = T,main = "",
     xlab = "Actual Humidity", breaks = 40)
lines(density(df$actual_hum), lwd = 2, col = "red")

##windspeed
df$actual_windspeed <- df$windspeed * 67
hist(df$actual_windspeed, col = "deepskyblue",border = F,probability = T,main = "",
     xlab = "Actual Windspeed", breaks = 40)
lines(density(df$actual_windspeed), lwd = 2, col = "red")

##casual
hist(df$casual, col = "deepskyblue", border = F, probability = T, main = "",
     xlab = "Casual Users Bike Rental Count", breaks = 40)
lines(density(df$casual), lwd = 2, col = "red")

##registered
hist(df$registered, col = "deepskyblue", border = F, probability = T, main = "",
     xlab = "Registered Users Bike Rental Count", breaks = 50)
lines(density(df$registered), lwd = 2, col = "red")

##count
hist(df$cnt, col = "deepskyblue", border = F, probability = T, main = "",
     xlab = "Total Bike Rental Count", breaks = 50)
lines(density(df$cnt), lwd = 2, col = "red")

######################################Making copy of data df###################################
day_data <- df
#Converting categorical variables into categories
#season
day_data$season <- factor(format(day_data$season, format="%A"),
                          levels = c("1", "2","3","4") , labels = c("Spring","Summer","Fall","Winter"))
#year
day_data$yr <- factor(format(day_data$yr, format="%A"),
                      levels = c("0", "1") , labels = c("2011","2012"))
#month
day_data$mnth <- as.factor(day_data$mnth)

#holiday
day_data$holiday <- factor(format(day_data$holiday, format="%A"),
                           levels = c("0", "1") , labels = c("Working Day","Holiday"))

#weekday
day_data$weekday <- as.factor(day_data$weekday)

#workingday
day_data$workingday <- as.factor(day_data$workingday)

#weathersit
day_data$weathersit <- factor(format(day_data$weathersit, format="%A"),
                              levels = c("1", "2","3","4") , 
                              labels = c("Good:Clear/Sunny","Moderate:Cloudy/Mist","Bad: Rain/Snow/Fog","Worse: Heavy Rain/Snow/Fog"))



                            ##For Categorical Variables##


#season

season_count <- day_data %>% group_by(season) %>% summarise(counts = n())
ggplot(season_count, aes(x = season , y = counts)) + 
  geom_bar(fill = "#0073C2FF", stat = "identity") + 
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean()

#year

year_count <- df_new %>% group_by(year) %>% summarise(counts = n())
ggplot(year_count, aes(x = year , y = counts)) + 
  geom_bar(fill = "#0073C2FF", stat = "identity") + 
  geom_text(aes(label = counts), vjust = -0.3) + scale_x_continuous(breaks = 0:1) +
  theme_pubclean()

#month
                    
month_count <- df_new %>% group_by(month) %>% summarise(counts = n())
ggplot(month_count, aes(x = month , y = counts)) + 
  geom_bar(fill = "#0073C2FF",stat = "identity") + 
  geom_text(aes(label = counts), vjust = -0.3) + scale_x_continuous(breaks = 1:12) +
  theme_pubclean()

#holiday

holiday_count <- df_new %>% group_by(holiday) %>% summarise(counts = n())
ggplot(holiday_count, aes(x = holiday , y = counts)) + 
  geom_bar(fill = "#0073C2FF",stat = "identity") + 
  geom_text(aes(label = counts), vjust = -0.3) + scale_x_continuous(breaks = 0:1) +
  theme_pubclean()

#weekday

weekday_count <- df_new %>% group_by(weekday) %>% summarise(counts = n())
ggplot(weekday_count, aes(x = weekday , y = counts)) + 
  geom_bar(fill = "#0073C2FF",stat = "identity") + 
  geom_text(aes(label = counts), vjust = -0.3) + scale_x_continuous(breaks = 0:6) +
  theme_pubclean()

#workingday

workingday_count <- df_new %>% group_by(workingday) %>% summarise(counts = n())
ggplot(workingday_count, aes(x = workingday , y = counts)) + 
  geom_bar(fill = "#0073C2FF",stat = "identity") + 
  geom_text(aes(label = counts), vjust = -0.3) + scale_x_continuous(breaks = 0:1) +
  theme_pubclean()

#weathersit

weathersit_count <- df_new %>% group_by(weathersit) %>% summarise(counts = n())
ggplot(weathersit_count, aes(x = weathersit , y = counts)) + 
  geom_bar(fill = "#0073C2FF",stat = "identity") + 
  geom_text(aes(label = counts), vjust = -0.3) + scale_x_continuous(breaks = 1:3) +
  theme_pubclean()


########################################Bivariate Analysis######################################################

                                #########For continuous variables###########

#Temperature v/s Total Rental Count
ggplot(day_data, aes(x = temp_celsius, y = cnt)) + geom_point(aes(color = season))+ geom_smooth(method = "lm") +
  stat_cor(method = "pearson", label.x = 0, label.y = 7500) + 
  labs(title="Temperature v/s Total Rental Count",x ="Temperature(celsius)", y = "Total Rental count") +
  theme(legend.position="right", plot.title = element_text(hjust = 0.5))

#"Feels Like" Temperature v/s Total Rental Count
ggplot(day_data, aes(x = atemp_celsius, y = cnt)) + geom_point(aes(color = season))+ geom_smooth(method = "lm") +
  stat_cor(method = "pearson", label.x = 0, label.y = 7500) + 
  labs(title='"Feels Like" Temperature v/s Total Rental Count',x ='Feels Like" Temperature(celsius)', y = "Total Rental count") +
  theme(legend.position="right", plot.title = element_text(hjust = 0.5))

#Humidity
ggplot(day_data, aes(x = actual_hum, y = cnt)) + geom_point(aes(color = season))+ geom_smooth(method = "lm") +
  stat_cor(method = "pearson", label.x = 0, label.y = 7500) + 
  labs(title='"Humidity v/s Total Rental Count',x ='Humidity', y = "Total Rental count") +
  theme(legend.position="right", plot.title = element_text(hjust = 0.5))

#windspeed
ggplot(day_data, aes(x = actual_windspeed, y = cnt)) + geom_point(aes(color = season))+ geom_smooth(method = "lm") +
  stat_cor(method = "pearson", label.x = 0, label.y = 7500) + 
  labs(title='"windspeed v/s Total Rental Count',x ='windspeed', y = "Total Rental count") +
  theme(legend.position="right", plot.title = element_text(hjust = 0.5))

#casual Users
ggplot(day_data, aes(x = casual, y = cnt)) + geom_point(aes(color = season))+ geom_smooth(method = "lm") +
  stat_cor(method = "pearson", label.x = 0, label.y = 7500) + 
  labs(title='"casual Users v/s Total Rental Count',x ='casual Users', y = "Total Rental count") +
  theme(legend.position="right", plot.title = element_text(hjust = 0.5))

#Registered Users
ggplot(day_data, aes(x = registered, y = cnt)) + geom_point(aes(color = season))+ geom_smooth(method = "lm") +
  stat_cor(method = "pearson", label.x = 0, label.y = 7500) + 
  labs(title='"Registered Users v/s Total Rental Count',x ='Registered Users', y = "Total Rental count") +
  theme(legend.position="right", plot.title = element_text(hjust = 0.5))

#Registered Users v/s casual Users
ggplot(day_data, aes(x = casual, y = registered)) + geom_point(aes(color = season))+ geom_smooth(method = "lm") +
  stat_cor(method = "pearson", label.x = 0, label.y = 7500) + 
  labs(title='"Registered Users v/s casual Users',x ='casual Users', y = "Registered Users") +
  theme(legend.position="right", plot.title = element_text(hjust = 0.5))


                               ########For Categorical variables##########

#season
ggplot(data=day_data, aes(x=season, y=cnt, fill=season)) + geom_boxplot() +
  labs(title='season v/s Total Bike Count',x ='season', y = "Total Bike Count") +
  theme(legend.position="right", plot.title = element_text(hjust = 0.5))
#year
ggplot(data=day_data, aes(x=yr, y=cnt, fill=mnth)) + geom_boxplot() +
  labs(title='Year v/s Total Bike Count',x ='Year', y = "Total Bike Count") +
  theme(legend.position="right", plot.title = element_text(hjust = 0.5))

#month
ggplot(data=day_data, aes(x=mnth, y=cnt, fill=season)) + geom_boxplot() +
  labs(title='Month v/s Total Bike Count',x ='Month', y = "Total Bike Count") +
  theme(legend.position="right", plot.title = element_text(hjust = 0.5))

#holiday
ggplot(data=day_data, aes(x=holiday, y=cnt, fill=mnth)) + geom_boxplot() +
  labs(title='holiday v/s Total Bike Count',x ='holiday', y = "Total Bike Count") +
  theme(legend.position="right", plot.title = element_text(hjust = 0.5))


#weekday
ggplot(data=day_data, aes(x=weekday, y=cnt, fill=holiday)) + geom_boxplot() +
  labs(title='weekday v/s Total Bike Count',x ='weekday', y = "Total Bike Count") +
  theme(legend.position="right", plot.title = element_text(hjust = 0.5))


#workingday
ggplot(data=day_data, aes(x=workingday, y=cnt, fill=weekday)) + geom_boxplot() +
  labs(title='workingday v/s Total Bike Count',x ='workingday', y = "Total Bike Count") +
  theme(legend.position="right", plot.title = element_text(hjust = 0.5))

#weathersit
ggplot(data=day_data, aes(x=weathersit, y=cnt, fill=season)) + geom_boxplot() +
  labs(title='weathersit v/s Total Bike Count',x ='weathersit', y = "Total Bike Count") +
  theme(legend.position="right", plot.title = element_text(hjust = 0.5))

######################################## Creating Dataset for further Analysis######################################################
bike_data <- df_new
######################################## Missing Value Treatment######################################################
library(Hmisc)
library(DataExplorer)

#Visualizing Missing Values for each variable
plot_missing(bike_data)   #No missing values in entire dataset
#looking for NULL values
sum(is.null(bike_data))   #No Null values

##############################################Outlier Analysis######################################################

describe(day_data$casual)
summary(day_data$casual)
describe(day_data$registered)
summary(day_data$registered)
describe(day_data$temp_celsius)
summary(day_data$temp_celsius)
describe(day_data$atemp_celsius)
summary(day_data$atemp_celsius)
describe(day_data$actual_hum)
summary(day_data$actual_hum)
describe(day_data$actual_windspeed)
summary(day_data$actual_windspeed) 


#Visualizing outlier using boxplots
#selecting only continuous variable
numeric_data <- day_data[ ,c(14:20)]


cnames = colnames(numeric_data)
for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i])), data = subset(day_data))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "skyblue" ,outlier.shape=18,
                        outlier.size=2, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i])+
           ggtitle(paste("Box plot for",cnames[i])))
}
## Plotting plots together
gridExtra::grid.arrange(gn2,gn3,ncol=2)
gridExtra::grid.arrange(gn4,gn5,gn6,ncol=3)
gridExtra::grid.arrange(gn7,gn1, ncol=2)

#As casual and registered sums up to make our target variable and we dont want to delete entries in our target variable.
#Other variables have values in acceptable range as per our business understanding.
#hence we won't remove values showing as outlier in our analysis.

########################################Feature Selection##########################################
## Correlation Plot 
library(ggcorrplot)
library(VIF)
library(usdm)

 bike_data[ ,c(8:13)] %>% na.omit() %>% cor() %>%  ggcorrplot(lab = T)


#Check for multicollinearity using VIF
vifcor(as.matrix(bike_data[ ,c(8:13)]))
#Removing atemp variable as a part of our feature selection
 bike_data = subset(bike_data, select = -c(atemp))
#Checking VIF after removing Weight Column
 vifcor(as.matrix(bike_data[ ,c(8:12)]))
 
 

##################################Feature Scaling################################################
 
#All our predictor variables are already in scaled version so we wont be applying scaling in this dataset
 
 ##################################Saving Pre-Processed Data################################################
 # Writing bike_data data to a csv file
 write_csv(bike_data, path = "bike_data_final.csv")
 
 ##################################Loading Pre-Processed Data################################################
 bike_data_final <- read_csv("bike_data_final.csv", col_names = T)

View(bike_data_final)
dim(bike_data_final)

###################################### Model Developement################################################
bike_rental <- bike_data_final

#Clean the environment
DataCombine::rmExcept("bike_rental")
#Converting variables with two levels into categorical
bike_rental$year <- as.factor(bike_rental$year)
bike_rental$holiday <- as.factor(bike_rental$holiday)
bike_rental$workingday <- as.factor(bike_rental$workingday)
str(bike_rental)
#converting variables with more than two levels into dummy variables
bike_rental$season <- as.factor(bike_rental$season)
bike_rental$month <- as.factor(bike_rental$month)
bike_rental$weekday <- as.factor(bike_rental$weekday)
bike_rental$weathersit <- as.factor(bike_rental$weathersit)
#Analysis of dataframe
str(bike_rental)

#Making dummy variables
results <- fastDummies::dummy_cols(bike_rental, 
           select_columns = c("season", "year", "month", "holiday", "weekday", "workingday", "weathersit"), remove_first_dummy = TRUE)

rental_data <- results[, c(-1:-7)]
#Changing order of traget variable
rental_data <- rental_data[, c(1:3,7:31,4:6)]


library(caret)
#We will develope three models here one for each casual, regsitered and count target variables
#Creating three dataset taking one target variable ata a time.



###############################################***********Linear Regression***********####################################################

###########################1>Count############################

#Total Count of bike rentals
data_count <- as.data.frame(rental_data[, -c(29:30)])
#Splitting data into train and test
set.seed(4444)
train_index_0 = sample(1:nrow(data_count), 0.8 * nrow(data_count))
train_count = data_count[train_index_0,]
test_count = data_count[-train_index_0,]

##Train the model using training data for Total Count of bike rentals
lr_model_count = lm(formula = count~., data = data_count)

#Get the summary of the model for Total Count of bike rentals
summary(lr_model_count)
#Predict the test cases for Total Count of bike rentals
lr_pred_count = predict(lr_model_count, test_count[, -29])
#Error metric evaluation
DMwR::regr.eval(test_count[, 29], lr_pred_count, stats = c('mae', 'rmse','mse', 'mape'))
plot(lr_pred_count - test_count[, 29])

#Statistics of model
    #Residual standard error: 769.5 on 703 degrees of freedom
    #Multiple R-squared:  0.848,	Adjusted R-squared:  0.8422 
    #F-statistic: 145.3 on 27 and 703 DF,  p-value: < 2.2e-16

    #mae         rmse          mse         mape 
# 8.038337e+04 9.220910e+03 8.502518e+07 2.836920e+01 

#######################2>Registered################################


#Registered count of bike rental
data_reg   <- as.data.frame(rental_data[, -c(29,31)])
#Splitting data into train and test
set.seed(1024)
train_index_1 = sample(1:nrow(data_reg), 0.8 * nrow(data_reg))
train_reg = data_reg[train_index_1,]
test_reg = data_reg[-train_index_1,]

##Train the model using training data for Registered
lr_model_reg = lm(formula = registered~., data = data_reg)

#Get the summary of the model for Registered
summary(lr_model_reg)
#Predict the test cases for Registered
lr_pred_reg = predict(lr_model_reg, test_reg[, -29])
#Error metric evaluation
DMwR::regr.eval(test_reg[, 29], lr_pred_reg, stats = c('mae', 'rmse','mse', 'mape'))
plot(lr_pred_reg - test_reg[, 29])

#Statistics of model
    #Residual standard error: 609 on 703 degrees of freedom
    #Multiple R-squared:  0.8533,	Adjusted R-squared:  0.8477 
    #F-statistic: 151.4 on 27 and 703 DF,  p-value: < 2.2e-16

    #mae         rmse          mse         mape 
  #6.253608e+04 6.839000e+03 4.677193e+07 2.030446e+01

#######################3>Casual################################


#Casual users count bike rental
data_casual<- as.data.frame(rental_data[, -c(30:31)])
#Splitting data into train and test
set.seed(2048)
train_index_2 = sample(1:nrow(data_casual), 0.8 * nrow(data_casual))
train_casual = data_casual[train_index_2,]
test_casual = data_casual[-train_index_2,]

##Train the model using training data for Casual users
lr_model_casual = lm(formula = casual~., data = data_casual)

#Get the summary of the model for Casual users
summary(lr_model_casual)
#Predict the test cases for Casual users
lr_pred_casual = predict(lr_model_casual, test_casual[, -29])
#Error metric evaluation
DMwR::regr.eval(test_casual[, 29], lr_pred_casual, stats = c('mae', 'rmse','mse', 'mape'))
plot(lr_pred_casual - test_casual[, 29])

#Statistic of model
    #Residual standard error: 353.3 on 703 degrees of freedom
    #Multiple R-squared:  0.745,	Adjusted R-squared:  0.7352 
    #F-statistic: 76.05 on 27 and 703 DF,  p-value: < 2.2e-16

    #mae         rmse          mse         mape 
  #3.825059e+04 4.043559e+03 1.635037e+07 1.617419e+02 



###############################################***********Decision Tree***********####################################################
library(rpart)
###########################1>Count############################
# ##rpart for regression
fit_count = rpart(count ~ ., data = data_count, method = "anova")
#Predict for new test cases
predictions_dt_count = predict(fit_count, test_count[,-29])
#Error metric evaluation
DMwR::regr.eval(test_count[ , 29], predictions_dt_count, stats = c('mae', 'rmse','mse','mape'))
plot(predictions_dt_count - test_count[ , 29])

#Statistics of model
  #   mae         rmse          mse         mape 
# 9.317740e+04 9.954193e+03 9.908596e+07 2.724658e+01

#######################2>Registered###########################
# ##rpart for regression
fit_reg = rpart(registered ~ ., data = data_reg, method = "anova")
#Predict for new test cases
predictions_dt_reg = predict(fit_reg, test_reg[,-29])
#Error metric evaluation
DMwR::regr.eval(test_reg[ , 29], predictions_dt_reg, stats = c('mae', 'rmse','mse','mape'))
plot(predictions_dt_reg - test_reg[ , 29])

#Statistics of model

  #    mae         rmse          mse         mape 
#   5.024423e+02 6.564812e+02 4.309675e+05 1.586881e-01

#######################3>Casual###############################
# ##rpart for regression
fit_casual = rpart(casual ~ ., data = data_casual, method = "anova")
#Predict for new test cases
predictions_dt_casual = predict(fit_casual, test_casual[,-29])
#Error metric evaluation
DMwR::regr.eval(test_casual[ , 29], predictions_dt_casual, stats = c('mae', 'rmse','mse','mape'))
plot(predictions_dt_casual - test_casual[ , 29])

#Statistics of model

    # mae         rmse          mse         mape 
# 2.252273e+02 3.314639e+02 1.098683e+05 7.977651e-01


###############################################***********Random Forest***********####################################################
library(randomForest)
###########################1>Count############################
###Random Forest
RF_model_count = randomForest(count ~ ., data = data_count, importance = TRUE, ntree = 100)
#Presdict test data using random forest model
RF_Predictions_count = predict(RF_model_count, test_count[,-29])
#Error metric evaluation
DMwR::regr.eval(test_count[ , 29], RF_Predictions_count, stats = c('mae', 'rmse','mse', 'mape'))
plot(RF_Predictions_count - test_count[ , 29])

#Statistics of model

#     mae         rmse          mse         mape 
# 3.229971e+04 3.532401e+03 1.247785e+07 8.664303e+00


#######################2>Registered###########################
###Random Forest
RF_model_reg = randomForest(registered ~ ., data = data_reg, importance = TRUE, ntree = 100)
#Presdict test data using random forest model
RF_Predictions_reg = predict(RF_model_reg, test_reg[,-29])
#Error metric evaluation
DMwR::regr.eval(test_reg[ , 29], RF_Predictions_reg, stats = c('mae', 'rmse','mse', 'mape'))
plot(RF_Predictions_reg - test_reg[ , 29])

#Statistics of model

#       mae         rmse          mse         mape 
#   1.695521e+02 2.451433e+02 6.009524e+04 5.586804e-02

#######################3>Casual###############################
###Random Forest
RF_model_casual = randomForest(casual ~ ., data = data_casual, importance = TRUE, ntree = 100)
#Presdict test data using random forest model
RF_Predictions_casual = predict(RF_model_casual, test_casual[,-29])
#Error metric evaluation
DMwR::regr.eval(test_casual[ , 29], RF_Predictions_casual, stats = c('mae', 'rmse','mse', 'mape'))
plot(RF_Predictions_casual - test_casual[ , 29])

#Statistics of model

#       mae         rmse          mse         mape 
#   8.644157e+01 1.298413e+02 1.685876e+04 2.333892e-01



###############################################***********XGB TREE***********####################################################
library(caret)
TrainControl <- trainControl( method = "repeatedcv", number = 5, repeats = 4)

###########################1>Count############################
set.seed(2468)
##Train the model using training data
model_xboost_count <- train(count ~ ., data = train_count, method = "xgbTree", trControl = TrainControl,verbose = FALSE)
#Predict the test cases
predicted_xgb_count <- predict(model_xboost_count, test_count[, -29])
DMwR::regr.eval(test_count[ , 29], predicted_xgb_count, stats = c('mae', 'rmse', 'mse', 'mape'))
plot(predicted_xgb_count - test_count[ , 29])

#Statistics of model

#          mae         rmse          mse         mape 
#     7.466990e+04 8.037006e+03 6.459347e+07 1.936431e+01 

#######################2>Registered###########################
set.seed(46819)
##Train the model using training data
model_xboost_reg <- train(registered ~ ., data = train_reg, method = "xgbTree", trControl = TrainControl,verbose = FALSE)
#Predict the test cases
predicted_xgb_reg <- predict(model_xboost_reg, test_reg[, -29])
DMwR::regr.eval(test_reg[ , 29], predicted_xgb_reg, stats = c('mae', 'rmse', 'mse', 'mape'))
plot(predicted_xgb_reg - test_reg[ , 29])

#Statistics of model
#     mae         rmse          mse         mape 
#   3.657507e+02 5.259043e+02 2.765754e+05 1.262242e-01 
#######################3>Casual###############################
set.seed(9213)
##Train the model using training data
model_xboost_cas <- train(casual ~ ., data = train_casual, method = "xgbTree", trControl = TrainControl,verbose = FALSE)
#Predict the test cases
predicted_xgb_casual <- predict(model_xboost_cas, test_casual[, -29])
DMwR::regr.eval(test_casual[ , 29], predicted_xgb_casual, stats = c('mae', 'rmse', 'mse', 'mape'))
plot(predicted_xgb_casual - test_casual[ , 29])

#Statistics of model

#       mae         rmse          mse         mape 
#   1.851897e+02 2.716535e+02 7.379563e+04 4.418736e-01


#################################****************THE END****************#####################################################