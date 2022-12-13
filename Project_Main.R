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

#Columns with NA values: Avg.Monthly.Long.Distance.Charges and Avg.Monthly.GB.Download

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







































