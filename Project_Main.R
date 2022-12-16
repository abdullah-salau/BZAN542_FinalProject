# BZAN 542 Project

#load libraries 
library("readxl")
library(data.table)
library(mlr)
library(h2o)
library(caret)
#install.packages("cowplot")
library(cowplot)
library(dplyr)
library(caTools)
library(ROCR)
library(caret)
library(e1071)
library(class)
library(corrplot)


setwd("C:/Users/asalau/OneDrive - University of Tennessee/UTK Documents/Classes/Data Mining for Business/BZAN542_FinalProject")

#importing dataset

df = read.csv("telecom_customer_churn.csv")
df = data.frame(df)
View(df)

#Subsetting dataset to only include churned or stayed customers
df = df["|"(df$Customer.Status=='Churned',df$Customer.Status=='Stayed'),]

#Analyze missing values
summary(df)
table(is.na(df))
sapply(df, function(x) sum(is.na(x))/length(x))*100

#Columns with NA values (not include=ing character features): Avg.Monthly.Long.Distance.Charges and Avg.Monthly.GB.Download

#Checking how many '0' values in missing variables
nrow(df[df$Avg.Monthly.GB.Download==0,])
#1344
nrow(df[df$Avg.Monthly.Long.Distance.Charges==0,])
#644

'&'(df$Avg.Monthly.GB.Download!=0,!is.na(df$Avg.Monthly.GB.Download))
#False is either 0 or NA


nrow(df[!('&'(df$Avg.Monthly.GB.Download!=0,!is.na(df$Avg.Monthly.GB.Download))),])

#Turning all columns with character class into factors

fact_col <- colnames(df)[sapply(df,is.character)]
for(i in fact_col)
  set(df,j=i,value = factor(df[[i]]))

(summary(df))

#Checking to see if not having phone service always means not having internet service
View(df[df$Phone.Service=='No',])
summary(df[df$Phone.Service=='No',])

#Looks like no phone service actually guarantees that the line will definitely have internet service

#We create different models for lines with internet service and lines without internet service

#dataframe with Internet Service: ISdf
#dataframe without Internet Service: NISdf

ISdf = df[df$Internet.Service=='Yes',]
View(ISdf)
NISdf = df[df$Internet.Service!='Yes',]
View(NISdf)

#Visualizing distribution of features
#ISdf
my_plots <- lapply(names(ISdf), function(var_x){
  p <- 
    ggplot(ISdf) +
    aes_string(var_x)
  
  if(is.numeric(ISdf[[var_x]])) {
    p <- p + geom_density()
    
  } else {
    p <- p + geom_bar()
  } 
  
})

plot_grid(plotlist = my_plots)

#NISdf
my_plots <- lapply(names(NISdf), function(var_x){
  p <- 
    ggplot(NISdf) +
    aes_string(var_x)
  
  if(is.numeric(NISdf[[var_x]])) {
    p <- p + geom_density()
    
  } else {
    p <- p + geom_bar()
  } 
  
})

plot_grid(plotlist = my_plots)

#Getting rid of (predicted) unimportant features
#For ISdf: Customer ID, Longitude, Latitude, Zipcode, Internet Service
#For NISdf: Customer ID, Longitude, Latitude, Zipcode, Internet Service, Phone Service, Internet TYpe, Avg Monthly GB DOwnload,
#Online Security, Internet...

#For ISdf
fullISdf = ISdf
ISdf = subset(ISdf, select = -c(Customer.ID, Longitude, Latitude, Zip.Code, Internet.Service))
View(ISdf)

#For NISdf
fullNISdf = NISdf
NISdf = subset(NISdf, select = -c(Customer.ID, Longitude, Latitude, Zip.Code, Internet.Service, Phone.Service, Internet.Type, Avg.Monthly.GB.Download,
                                  Online.Security, Online.Backup, Device.Protection.Plan, Premium.Tech.Support, Streaming.TV, Streaming.Movies, 
                                  Streaming.Music, Unlimited.Data))
View(NISdf)

#setting Customer.Status type to factor
set(ISdf,j='Customer.Status',value = factor(ISdf[['Customer.Status']]))
set(NISdf,j='Customer.Status',value = factor(NISdf[['Customer.Status']]))

#Removing Churn Category and Churn Reason for both datasets
ISdf = subset(ISdf, select = -c(Churn.Category, Churn.Reason))
NISdf = subset(NISdf, select = -c(Churn.Category, Churn.Reason))

#Removing Total.Extra.Data.Charges (Only contains 0 values) for NISdf
NISdf = subset(NISdf, select = -c(Total.Extra.Data.Charges))

#Missing values for both datasets after subsetting for important features
summary(ISdf)
summary(NISdf)

#Imputation: For ISdf, Replacing NA values in Avg.Monthly.Long.Distance.Charges and Multiple.Line with 0 and 'No' respectively
#Go through each value of column, replace NA values
#count = 0
for(j in 1:nrow(ISdf))
{
  if(is.na(ISdf[j,'Avg.Monthly.Long.Distance.Charges']))
  {
    #count = count + 1
    ISdf[j,'Avg.Monthly.Long.Distance.Charges'] = 0
  }
}
#count = 0
for(j in 1:nrow(ISdf))
{
  if(ISdf[j,'Multiple.Lines']=='')
  {
    #count = count + 1
    ISdf[j,'Multiple.Lines'] = 'No'
  }
}

#There does not seem to be any missing values for the NISdf

#Before splitting, we will standardize the numerical features of both datasets
trainParams = preProcess(ISdf, method=c("center", "scale"))
ISdf_standardized = predict(trainParams, ISdf)
summary(ISdf_standardized)

trainParams2 = preProcess(NISdf, method=c("center", "scale"))
NISdf_standardized = predict(trainParams2, NISdf)
summary(NISdf_standardized)

#Splitting both datasets into training and testing datasets with 75:25 ratio
#We use the createDataPartition function to maintain the event rate in both training and testing dataset
trainIndex <- createDataPartition(ISdf_standardized$Customer.Status, p = .75,
                                  list = FALSE,
                                  times = 1)
trainISdf <- ISdf_standardized[trainIndex,]
testISdf <- ISdf_standardized[-trainIndex,]
View(trainISdf)
View(testISdf)

#For NISdf
trainIndex2 <- createDataPartition(NISdf_standardized$Customer.Status, p = .75,
                                  list = FALSE,
                                  times = 1)
trainNISdf <- NISdf_standardized[trainIndex2,]
testNISdf <- NISdf_standardized[-trainIndex2,]
View(trainNISdf)
View(testNISdf)

#Creating Model
#ISdf
library(randomForest)

#Deleting City Feature
table(ISdf_standardized$City)
trainISdf = subset(trainISdf, select = -c(City))
testISdf = subset(testISdf, select = -c(City))

trainNISdf = subset(trainNISdf, select = -c(City))
testNISdf = subset(testNISdf, select = -c(City))

rf_A <- randomForest(Customer.Status ~ .,
                     data = trainISdf,
)
rfTest_A = subset(testISdf, select = -c(Customer.Status))
View(rfTest_A)

##RFPred_A: 
RFpred_A <- predict(object=rf_A, rfTest_A, type='prob')#, probability=TRUE)#probabilities
#head(attr(RFpred, "probabilities"))
head(RFpred_A)
response_A <- predict(object=rf_A, rfTest_A, type='response')#only the labels
head(response_A)
correctResponse_A = testISdf[,"Customer.Status"]#correct response labels for rf modelA

table(response_A, correctResponse_A)
#         correctResponse_A
#response_A Churned Stayed
#Churned     293     61
#Stayed      146    811

#Scores, Precision, F1 Score
confusionMatrix(response_A, correctResponse_A, mode="everything")

#For NISdf
rf_B <- randomForest(Customer.Status ~ .,
                     data = trainNISdf,
)
rfTest_B = subset(testNISdf, select = -c(Customer.Status))
View(rfTest_B)

##RFPred_B: 
RFpred_B <- predict(object=rf_B, rfTest_B, type='prob')#, probability=TRUE)#probabilities
#head(attr(RFpred, "probabilities"))
head(RFpred_B)
response_B <- predict(object=rf_B, rfTest_B, type='response')#only the labels
head(response_B)
correctResponse_B = testNISdf[,"Customer.Status"]#correct response labels for rf modelA

table(response_B, correctResponse_B)
#         correctResponse_A
#response_A Churned Stayed
#Churned     293     61
#Stayed      146    811

#Scores, Precision, Recall, F1 Score
confusionMatrix(response_B, correctResponse_B, mode="everything")

#Run tests on other models
#Logistic Regression
#Training Model (ISdf)
logModel_A <- glm(Customer.Status ~ ., 
                      data = trainISdf, 
                      family = "binomial")
logModel_A

summary(logModel_A)
#Modifying test dataset for logistic regression prediction
# dummy <- dummyVars(" ~ .", data=testISdf)
# logTest_A <- data.frame(predict(dummy, newdata = testISdf))
# logTest_A = subset(logTest_A, select = -c(Gender.Female,Married.No,Offer.None,Phone.Service.No,Multiple.Lines., Multiple.Lines.No, 
#                                           Internet.Type., Internet.Type.Cable, Online.Security., Online.Security.No, Online.Backup., Online.Backup.No,
#                                           Device.Protection.Plan., Device.Protection.Plan.No, Premium.Tech.Support., Premium.Tech.Support.No,
#                                           Streaming.TV., Streaming.TV.No, Streaming.Movies., Streaming.Movies.No, Streaming.Music., Streaming.Music.No,
#                                           Unlimited.Data., Unlimited.Data.No, Contract.Month.to.Month, Paperless.Billing.No, Payment.Method.Bank.Withdrawal,
#                                           Customer.Status.Churned))
# 

# Predict test data based on model
logTest_A = subset(testISdf, select = -c(Customer.Status))
predict_reg <- predict(logModel_A, 
                       logTest_A, type = "response")
predict_reg
logResponse_A <- ifelse(predict_reg >0.5, 1, 0)

log_correctResponse_A <- ifelse(correctResponse_A =='Churned', 0, 1)
log_correctResponse_A<- as.factor(log_correctResponse_A)
logResponse_A<- as.factor(logResponse_A)
confusionMatrix((logResponse_A), (log_correctResponse_A), mode="everything")

#Training Model (NISdf)
logModel_B <- glm(Customer.Status ~ ., 
                  data = trainNISdf, 
                  family = "binomial")
logModel_B

summary(logModel_B)

# Predict test data based on model
logTest_B = subset(testNISdf, select = -c(Customer.Status))
predict_reg_B <- predict(logModel_B, 
                       logTest_B, type = "response")
predict_reg_B
logResponse_B <- ifelse(predict_reg_B >0.5, 1, 0)

log_correctResponse_B <- ifelse(correctResponse_B =='Churned', 0, 1)
log_correctResponse_B<- as.factor(log_correctResponse_B)
logResponse_B<- as.factor(logResponse_B)
confusionMatrix((logResponse_B), (log_correctResponse_B), mode="everything")

#kNN Model
#ISdf
#Training Model
dummy <- dummyVars(" ~ .", data=trainISdf)
knnTrain <- data.frame(predict(dummy, newdata = trainISdf))
knnTrain = subset(knnTrain, select = -c(Gender.Female,Married.No,Offer.None,Phone.Service.No,Multiple.Lines., Multiple.Lines.No,
                                          Internet.Type., Internet.Type.Cable, Online.Security., Online.Security.No, Online.Backup., Online.Backup.No,
                                          Device.Protection.Plan., Device.Protection.Plan.No, Premium.Tech.Support., Premium.Tech.Support.No,
                                          Streaming.TV., Streaming.TV.No, Streaming.Movies., Streaming.Movies.No, Streaming.Music., Streaming.Music.No,
                                          Unlimited.Data., Unlimited.Data.No, Contract.Month.to.Month, Paperless.Billing.No, Payment.Method.Bank.Withdrawal,
                                          Customer.Status.Churned))

dummy <- dummyVars(" ~ .", data=testISdf)
knnTest <- data.frame(predict(dummy, newdata = testISdf))
knnTest = subset(knnTest, select = -c(Gender.Female,Married.No,Offer.None,Phone.Service.No,Multiple.Lines., Multiple.Lines.No,
                                          Internet.Type., Internet.Type.Cable, Online.Security., Online.Security.No, Online.Backup., Online.Backup.No,
                                          Device.Protection.Plan., Device.Protection.Plan.No, Premium.Tech.Support., Premium.Tech.Support.No,
                                          Streaming.TV., Streaming.TV.No, Streaming.Movies., Streaming.Movies.No, Streaming.Music., Streaming.Music.No,
                                          Unlimited.Data., Unlimited.Data.No, Contract.Month.to.Month, Paperless.Billing.No, Payment.Method.Bank.Withdrawal,
                                          Customer.Status.Churned))

knnTrain_A = subset(knnTrain, select = -c(Customer.Status.Stayed))
knnTest_A = subset(knnTest, select = -c(Customer.Status.Stayed))


#knnTest_A = subset(testISdf, select = -c(Customer.Status))
#knnTrain_A = subset(trainISdf, select = -c(Customer.Status))
knn.70_A <- knn(train = knnTrain_A,
                      test = knnTest_A,
                      cl = knnTrain$Customer.Status.Stayed,
                      k = 70)
knn.70_A

confusionMatrix(knn.70_A,as.factor(knnTest$Customer.Status.Stayed), mode = 'everything')

kList = c(30,40,50,60,80,90,100)
kList = c(25, 20 ,15,10, 5)
for(k in kList){
  knn_A <- knn(train = knnTrain_A,
                  test = knnTest_A,
                  cl = knnTrain$Customer.Status.Stayed,
                  k = k)
  #knn_A
  
  print(confusionMatrix(knn_A,as.factor(knnTest$Customer.Status.Stayed), mode = 'everything'))
  
}


M<- cor(knnTrain)
corrplot(M, method='color')
#We will delete Total Revenue, Total Extra Data Charges, and DSL Internet type.

#PCA
pca = prcomp(knnTrain_A, center = TRUE,scale. = TRUE)
summary(pca)

#Retrying randomforest model on updated training dataset:
newTrain = subset(trainISdf, select=-c(Total.Revenue, Total.Extra.Data.Charges, Contract, Unlimited.Data, Streaming.Music, Streaming.Movies,
                                      Streaming.TV, Premium.Tech.Support, Device.Protection.Plan, Online.Backup, Online.Security))


rf2_A <- randomForest(Customer.Status ~ .,
                     data = newTrain,
)
rfTest2_A = subset(testISdf, select = -c(Customer.Status,Total.Revenue, Total.Extra.Data.Charges, Contract, Unlimited.Data, Streaming.Music, Streaming.Movies,
                                         Streaming.TV, Premium.Tech.Support, Device.Protection.Plan, Online.Backup, Online.Security))
View(rfTest2_A)

##RFPred_A: 
RFpred2_A <- predict(object=rf2_A, rfTest2_A, type='prob')#, probability=TRUE)#probabilities
#head(attr(RFpred, "probabilities"))
head(RFpred2_A)
response2_A <- predict(object=rf2_A, rfTest2_A, type='response')#only the labels
head(response2_A)
correctResponse2_A = testISdf[,"Customer.Status"]#correct response labels for rf modelA

table(response2_A, correctResponse2_A)
#         correctResponse_A
#response_A Churned Stayed
#Churned     293     61
#Stayed      146    811

#Scores, Precision, F1 Score
confusionMatrix(response2_A, correctResponse2_A, mode="everything")

#Including more features
newTrain2 = subset(trainISdf, select=-c(Total.Revenue, Total.Extra.Data.Charges, Contract, Unlimited.Data, Streaming.Music, Streaming.Movies,
                                       Streaming.TV, Premium.Tech.Support))#, Device.Protection.Plan, Online.Backup, Online.Security))


rf3_A <- randomForest(Customer.Status ~ .,
                      data = newTrain2,
)
rfTest3_A = subset(testISdf, select = -c(Customer.Status,Total.Revenue, Total.Extra.Data.Charges, Contract, Unlimited.Data, Streaming.Music, Streaming.Movies,
                                         Streaming.TV, Premium.Tech.Support))#, Device.Protection.Plan, Online.Backup, Online.Security))
View(rfTest3_A)

##RFPred_A: 
RFpred3_A <- predict(object=rf3_A, rfTest3_A, type='prob')#, probability=TRUE)#probabilities
#head(attr(RFpred, "probabilities"))
head(RFpred3_A)
response3_A <- predict(object=rf3_A, rfTest3_A, type='response')#only the labels
head(response3_A)
correctResponse3_A = testISdf[,"Customer.Status"]#correct response labels for rf modelA

table(response3_A, correctResponse3_A)
#         correctResponse_A
#response_A Churned Stayed
#Churned     293     61
#Stayed      146    811

#Scores, Precision, F1 Score
confusionMatrix(response3_A, correctResponse3_A, mode="everything")












#Focus on whichever one has highest F1 scores
#Do PCA to cut down on features (Could also do correlation matrix to drop features as well).
#Huge objective is to improve recall rate for accounts that have churned
#Handle Data imbalance problem for NISdf
#Parameter tuning
#Report improvements
#Introduce usefulness of employing Churn probabilities and use Slide 6 diagram as example of proposed customer retention strategy

# Our conclusion should have an assertive conclusion on which model will be best for prediction, a discussion of its results,
#and how it compares to the other models we tested. 
























