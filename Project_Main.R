# BZAN 542 Project

setwd("C:/Users/abdul/Desktop/UTK Documents/Classes/Data Mining for Business/BZAN542_FinalProject")

#importing dataset

df = read.csv("telecom_customer_churn.csv")
View(df)

#Subsetting dataset to only include churned or stayed customers
df = df["|"(df$Customer.Status=='Churned',df$Customer.Status=='Stayed'),]

#Analyze missing values
summary(df)
table(is.na(df))
sapply(df, function(x) sum(is.na(x))/length(x))*100

#Columns with NA values: Avg.Monthly.Long.Distance.Charges and Avg.Monthly.GB.Download
#commir test