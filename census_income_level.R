
### 1. Problem Statement and Hypothesis Generation
#set working directory
path <- "~/working/capstone/"
setwd(path)

#load packages & data
#install.packages("data.table") 
library(data.table)
train <- fread("train.csv",na.strings = c(""," ","?","NA",NA))
test <- fread("test.csv",na.strings = c(""," ","?","NA",NA))

### 2. Data Exploration
#look at data
dim(train); str (train); 
#View(train)
dim(test); str (test); 
# View(test)

#check first few rows of train & test
train[1:5]
test [1:5]

#check target variables
unique(train$income_level)
unique(test$income_level)

table(train$income_level)
table(test$income_level)

#encode target variables
train[,income_level := ifelse(income_level == "-50000",0,1)]
test[,income_level := ifelse(income_level == "-50000",0,1)]


# Let’s look at the severity of imbalanced classes in our data:
round(prop.table(table(train$income_level))*100)
(prop.table(table(test$income_level))*100)

#set column classes (changed from int, chr to num, factor)
factcols <- c(2:5,7,8:16,20:29,31:38,40,41)
numcols <- setdiff(1:40,factcols)

train[,(factcols) := lapply(.SD, factor), .SDcols = factcols]
train[,(numcols) := lapply(.SD, as.numeric), .SDcols = numcols]

test[,(factcols) := lapply(.SD, factor), .SDcols = factcols]
test[,(numcols) := lapply(.SD, as.numeric), .SDcols = numcols]


#subset categorical variables
cat_train <- train[,factcols, with=FALSE]
cat_test <- test[,factcols,with=FALSE]

#subset numerical variables
num_train <- train[,numcols,with=FALSE]
num_test <- test[,numcols,with=FALSE] 
rm(train,test) # to save memory

### 3. EDA
# install.packages("ggplot2") 
# install.packages("plotly") 
library(ggplot2)
library(plotly)


#plot variable age 
plt <- ggplot(data = num_train, aes(x= age, y=..density..)) + geom_histogram(fill="blue",color="red",alpha = 0.5,bins =100) + geom_density()
plt + ggtitle("Age Distribution") + theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5))

#variable capital_gains
plt <- ggplot(data = num_train, aes(x= capital_gains, y=..density..)) + geom_histogram(fill="blue",color="red",alpha = 0.5,bins =100) + geom_density()
plt + ggtitle("Capital Gains Distribution") + theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5))


#variable capital_losses
plt <- ggplot(data = num_train, aes(x= capital_losses, y=..density..)) + geom_histogram(fill="blue",color="red",alpha = 0.5,bins =100) + geom_density()
plt + ggtitle("Capital Losses Distribution") + theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5))


# wage per hour
plt <- ggplot(data = num_train, aes(x= wage_per_hour, y=..density..)) + geom_histogram(fill="blue",color="red",alpha = 0.5,bins =100) + geom_density()
plt + ggtitle("Hourly Wage Distribution") + theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5))


## Plot numerical columns vs. target variable
#add target variable
num_train[,income_level := cat_train$income_level]
#create a scatter plot
plt <- ggplot(data=num_train,aes(x = age, y=wage_per_hour))+geom_point(aes(colour=income_level))+scale_y_continuous("wage per hour", breaks = seq(0,10000,1000))
plt + ggtitle("Hourly Wage vs. Age") + theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5))



## Plot categorical columns vs. target variable
#dodged bar chart
#variable class_of_worker
plt <- ggplot(cat_train,aes(x=class_of_worker,fill=income_level))+geom_bar(position = "dodge",  color="black")+scale_fill_brewer(palette = "Pastel1")+theme(axis.text.x =element_text(angle  = 60,hjust = 1,size=10))
plt + ggtitle("Income Distribution by worker's class") + theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5))
# Only two category levels (Not In Universe & Private) seem to dominate. 
# In such situation, a good practice is to combine levels having less than 5% frequency of the total category frequency.

#variable education
plt <- ggplot(cat_train,aes(x=education,fill=income_level))+geom_bar(position = "dodge",  color="black")+scale_fill_brewer(palette = "Pastel1")+theme(axis.text.x =element_text(angle  = 60,hjust = 1,size=10))
plt + ggtitle("Income Distribution by education") + theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5))


# checking categories using 2 way tables
prop.table(table(cat_train$marital_status,cat_train$income_level),1)
prop.table(table(cat_train$class_of_worker,cat_train$income_level),1)


### 3. Data Cleansing

#check missing values in numerical data and do imputation if needed
table(is.na(num_train))
sum(is.na(num_train))
table(is.na(num_test))
sum(is.na(num_test))
# We see that numeric variables has no missing values

#check correlations between numerical columns
# install.packages("caret") 
library(caret)
#set threshold as 0.7
num_train[,income_level := NULL]
ax <-findCorrelation(x = cor(num_train), cutoff = 0.7)
num_train <- num_train[,-ax,with=FALSE] 
num_test[,weeks_worked_in_year := NULL]
# The variable weeks_worked_in_year gets removed. 
# For hygiene purpose, we’ve removed that variable from test data too. It’s not necessary though!


# check for missing values in categorical data. We’ll use base sapply() to find out percentage of missing values per column.
#check missing values per columns
mvtr <- sapply(cat_train, function(x){sum(is.na(x))/length(x)})*100
mvte <- sapply(cat_test, function(x){sum(is.na(x)/length(x))}*100)
mvtr
mvte


### 4. Data Manipulation
#select columns with missing value less than 5%
cat_train <- subset(cat_train, select = mvtr < 5 )
cat_test <- subset(cat_test, select = mvte < 5)


### Don't want to do imputation.....
#set NA as Unavailable - train data
#convert to characters
cat_train <- cat_train[,names(cat_train) := lapply(.SD, as.character),.SDcols = names(cat_train)]
for (i in seq_along(cat_train)) set(cat_train, i=which(is.na(cat_train[[i]])), j=i, value="Unavailable")
#convert back to factors
cat_train <- cat_train[, names(cat_train) := lapply(.SD,factor), .SDcols = names(cat_train)]

#set NA as Unavailable - test data
cat_test <- cat_test[, (names(cat_test)) := lapply(.SD, as.character), .SDcols = names(cat_test)]
for (i in seq_along(cat_test)) set(cat_test, i=which(is.na(cat_test[[i]])), j=i, value="Unavailable")
#convert back to factors
cat_test <- cat_test[, (names(cat_test)) := lapply(.SD, factor), .SDcols = names(cat_test)]



#combine factor levels with less than 5% values
#train
for(i in names(cat_train)){
  p <- 5/100
  ld <- names(which(prop.table(table(cat_train[[i]])) < p))
  levels(cat_train[[i]])[levels(cat_train[[i]]) %in% ld] <- "Other"
}

#test
for(i in names(cat_test)){
  p <- 5/100
  ld <- names(which(prop.table(table(cat_test[[i]])) < p))
  levels(cat_test[[i]])[levels(cat_test[[i]]) %in% ld] <- "Other"
}



#check columns with unequal levels (unique values)
#install.packages("mlr") 
library(mlr)
summarizeColumns(cat_train)[,"nlevs"]
summarizeColumns(cat_test)[,"nlevs"]



# let’s look at numeric variables and reflect on possible ways for binning.
num_train[,.N,age][order(age)]
num_train[,.N,wage_per_hour][order(-N)]
num_train[,.N,capital_gains][order(-N)]
num_train[,.N,capital_losses][order(-N)]
num_train[,.N,dividend_from_Stocks][order(-N)]
num_train[,.N,num_person_Worked_employer][order(-N)]


#bin age variable 0-30 31-60 61 - 90
num_train[,age:= cut(x = age,breaks = c(0,30,60,90),include.lowest = TRUE,labels = c("young","adult","old"))]
num_train[,age := factor(age)]

num_test[,age:= cut(x = age,breaks = c(0,30,60,90),include.lowest = TRUE,labels = c("young","adult","old"))]
num_test[,age := factor(age)]

#Bin numeric variables with Zero and MoreThanZero (basically converting continuous to discrete)
num_train[,wage_per_hour := ifelse(wage_per_hour == 0,"Zero","MoreThanZero")][,wage_per_hour := as.factor(wage_per_hour)]
num_train[,capital_gains := ifelse(capital_gains == 0,"Zero","MoreThanZero")][,capital_gains := as.factor(capital_gains)]
num_train[,capital_losses := ifelse(capital_losses == 0,"Zero","MoreThanZero")][,capital_losses := as.factor(capital_losses)]
num_train[,dividend_from_Stocks := ifelse(dividend_from_Stocks == 0,"Zero","MoreThanZero")][,dividend_from_Stocks := as.factor(dividend_from_Stocks)]

num_test[,wage_per_hour := ifelse(wage_per_hour == 0,"Zero","MoreThanZero")][,wage_per_hour := as.factor(wage_per_hour)]
num_test[,capital_gains := ifelse(capital_gains == 0,"Zero","MoreThanZero")][,capital_gains := as.factor(capital_gains)]
num_test[,capital_losses := ifelse(capital_losses == 0,"Zero","MoreThanZero")][,capital_losses := as.factor(capital_losses)]
num_test[,dividend_from_Stocks := ifelse(dividend_from_Stocks == 0,"Zero","MoreThanZero")][,dividend_from_Stocks := as.factor(dividend_from_Stocks)]

###################################################################################
### 5. Machine Learning
#combine data and make test & train files
d_train <- cbind(num_train,cat_train)
d_test <- cbind(num_test,cat_test)

#remove unwanted files
rm(num_train,num_test,cat_train,cat_test) #save memory

#load library for machine learning
library(mlr)

#create task
train.task <- makeClassifTask(data = d_train,target = "income_level")
test.task <- makeClassifTask(data=d_test,target = "income_level")

#remove zero variance features
train.task <- removeConstantFeatures(train.task)
test.task <- removeConstantFeatures(test.task)

#get variable importance chart
# install.packages("FSelector")
var_imp <- generateFilterValuesData(train.task, method = c("information.gain"))
plotFilterValues(var_imp,feat.type.cols = TRUE)

#install.packages("DMwR")
library(DMwR)

# original dataset
write.csv(d_train, "census_orig.csv")
write.csv(d_test, "test.csv")

TPR = TP/(TP+FN)


#undersampling 
train.under <- undersample(train.task,rate = 0.1) #keep only 10% of majority class
table(getTaskTargets(train.under))
write.csv(getTaskData(train.under, recode.target = "01"), "census_under.csv")

#oversampling
train.over <- oversample(train.task,rate=15) #make minority class 15 times
table(getTaskTargets(train.over))
write.csv(getTaskData(train.over, recode.target = "01"), "census_over.csv")


#SMOTE
#train.smote <- smote(train.task,rate = 15,nn = 5)
system.time(
  train.smote <- smote(train.task,rate = 10,nn = 3) 
)
table(getTaskTargets(train.smote))
write.csv(getTaskData(train.smote, recode.target = "01"), "census_smote.csv")

# ROSE
library(ROSE)
# use ROSE to synthetically generate data
data.rose <- ROSE(income_level ~ ., data = d_train, seed = 1)$data
table(data.rose$income_level)
write.csv(data.rose, "census_rose.csv")

dim(d_train)
dim(data.rose)
d_train



























#lets see which algorithms are available
listLearners("classif","twoclass")[c("class","package")]

#################################################################################
#naive Bayes
naive_learner <- makeLearner("classif.naiveBayes",predict.type = "response")
naive_learner$par.vals <- list(laplace = 1)

#10fold CV - stratified
folds <- makeResampleDesc("CV",iters=10,stratify = TRUE)
#cross validation function
fun_cv <- function(a){
  crv_val <- resample(naive_learner,a,folds,measures = list(acc,tpr,tnr,fpr,fp,fn))
  crv_val$aggr
}
fun_cv(train.task) 
fun_cv(train.under) 
fun_cv(train.over)
fun_cv(train.smote)

## SMOTE Data
#train and predict 
nB_model <- train(naive_learner, train.smote)
nB_predict <- predict(nB_model,test.task)
#evaluate
nB_prediction <- nB_predict$data$response
dCM <- confusionMatrix(d_test$income_level,nB_prediction)
#calculate metrics
dCM$byClass['Sensitivity']
dCM$byClass['Specificity']
dCM$byClass

## original Data
#train and predict 
nB_model <- train(naive_learner, train.task)
nB_predict <- predict(nB_model,test.task)
#evaluate
nB_prediction <- nB_predict$data$response
dCM <- confusionMatrix(d_test$income_level,nB_prediction)
#calculate metrics
dCM$byClass['Sensitivity']
dCM$byClass['Specificity']


#########################################################################
#xgboost
# install.packages("drat", repos="https://cran.rstudio.com")
# drat:::addRepo("dmlc")
# install.packages("xgboost", repos="http://dmlc.ml/drat/", type = "source")
set.seed(2002)
xgb_learner <- makeLearner("classif.xgboost",predict.type = "response")
xgb_learner$par.vals <- list(
  objective = "binary:logistic",
  eval_metric = "error",
  nrounds = 150,
  print_every_n = 50
)

#define hyperparameters for tuning
xg_ps <- makeParamSet( 
  makeIntegerParam("max_depth",lower=3,upper=10),
  makeNumericParam("lambda",lower=0.05,upper=0.5),
  makeNumericParam("eta", lower = 0.01, upper = 0.5),
  makeNumericParam("subsample", lower = 0.50, upper = 1),
  makeNumericParam("min_child_weight",lower=2,upper=10),
  makeNumericParam("colsample_bytree",lower = 0.50,upper = 0.80)
)

#define search function
rancontrol <- makeTuneControlRandom(maxit = 5L) #do 5 iterations

#5 fold cross validation
set_cv <- makeResampleDesc("CV",iters = 5L,stratify = TRUE)

#tune parameters
xgb_tune <- tuneParams(learner = xgb_learner, task = train.task, resampling = set_cv, measures = list(acc,tpr,tnr,fpr,fp,fn), par.set = xg_ps, control = rancontrol)
# Tune result:
# Op. pars: max_depth=3; lambda=0.221; eta=0.161; subsample=0.698; min_child_weight=7.67; colsample_bytree=0.642
# acc.test.mean=0.948,tpr.test.mean=0.989,tnr.test.mean=0.324,fpr.test.mean=0.676

#set optimal parameters
#xgb_new <- setHyperPars(learner = xgb_learner, par.vals = xgb_tune$x)
xgb_new <- setHyperPars(learner = xgb_learner, par.vals = list(max_depth=3, lambda=0.221, eta=0.161, subsample=0.698, min_child_weight=7.67, colsample_bytree=0.642) )


#train model
xgmodel <- train(xgb_new, train.task)

#test model
predict.xg <- predict(xgmodel, test.task)

#make prediction
xg_prediction <- predict.xg$data$response
getNamespaceExports("xgboost")
#make confusion matrix
xg_confused <- confusionMatrix(d_test$income_level,xg_prediction)

precision <- xg_confused$byClass['Pos Pred Value']
recall <- xg_confused$byClass['Sensitivity']

f_measure <- 2*((precision*recall)/(precision+recall))
f_measure
