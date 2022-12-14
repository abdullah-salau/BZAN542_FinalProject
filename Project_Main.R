# BZAN 542 Project

#load libraries 
library("readxl")
library(data.table)
library(mlr)
library(h2o)
library(caret)
#install.packages("cowplot")
library(cowplot)


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

#Handling Missing Values
summary(NISdf)
summary(ISdf)
































