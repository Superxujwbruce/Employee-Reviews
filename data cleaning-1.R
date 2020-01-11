library(readr)
library(tidyr)
library(dplyr)
library(tidytext)
library(wordcloud)
library(ggplot2); 
library(ggthemes)
library(qdap)
library(tm)
library(gridExtra)
library(corrplot)
#From the Project 1, we clean the data from raw dataset and named mydata.csv,
#Then, for Project 2 data cleaning, we import mydata.csv first and finally get mydata_clean.csv at the end

#we import mydata.csv to Rstudio from 'Import Dataset' on the upper right 'Environment'
#then choose 'From Text(readr)', and select mydata.csv
mydata <- read_csv("C:/Users/Administrator/Downloads/mydata.csv")

#explore data
head(mydata)
#name
dataNames <- colnames(mydata);dataNames
#type
dataType <- sapply(mydata, class);dataType
#which variables are factors
isFactor <- sapply(mydata, is.factor);isFactor
#Check how many missing value
NaVariables <- sapply(mydata, function(x) any(is.na(x)));NaVariables
CountNAs <- sapply(mydata, function(x) sum(is.na(x)));CountNAs
sum(is.na(mydata))
#Check levels
numOfLevels <- sapply(mydata, function(x) length(levels(x)));numOfLevels
#Check if overall-ratings has missing value
sum(is.na(mydata$`overall-ratings`))
#impute missing value in categories
cc=is.na(mydata$summary)
m=which(cc==c("TRUE"))
mydata=mydata[-m,]
cc=is.na(mydata$cons)
m=which(cc==c("TRUE"))
mydata=mydata[-m,]
cc=is.na(mydata$`advice-to-mgmt`)
m=which(cc==c("TRUE"))
mydata=mydata[-m,]
cc=is.na(mydata$`overall-ratings`)
m=which(cc==c("TRUE"))
mydata=mydata[-m,]
cc=is.na(mydata$`carrer-opportunities-stars` & mydata$`comp-benefit-stars`)
m=which(cc==c("TRUE"))
mydata=mydata[-m,]
cc=is.na(mydata$`senior-mangemnet-stars` | mydata$`helpful-count`)
m=which(cc==c("TRUE"))
mydata=mydata[-m,]
cc=is.na(mydata$`helpful-count`)
m=which(cc==c("TRUE"))
mydata=mydata[-m,]
n=which(mydata$summary=="n/a")
mydata=mydata[-n,]
n=which(mydata$summary=="N/A")
mydata=mydata[-n,]
n=which(mydata$summary=="Na")
mydata=mydata[-n,]
n=which(mydata$summary=="na")
mydata=mydata[-n,]
n=which(mydata$summary=="-")
mydata=mydata[-n,]
n=which(mydata$summary==".")
mydata=mydata[-n,]
n=which(mydata$summary=="...")
mydata=mydata[-n,]
NaVariables <- sapply(mydata, function(x) any(is.na(x)));NaVariables
CountNAs <- sapply(mydata, function(x) sum(is.na(x)));CountNAs
sum(is.na(mydata))
# We now have 0 missing values and 66,638 entries.

#the second method, we find it is the same results and we name mydata_clean as our processed dataset
mydata_clean<-na.omit(mydata)
mydata_clean<-mydata_clean%>%
  filter(!summary %in% c("n/a","N/A","na","Na","-",".",'...'))%>%
  filter(!pros %in% c("n/a","N/A","na","Na","-",".",'...'))%>%
  filter(!cons %in% c("n/a","N/A","na","Na","-",".",'...'))%>%
  filter(!`advice-to-mgmt` %in% c("n/a","N/A","na","Na","-",".",'...'))
# We now have 0 missing values and 66,329 entries.

#Finally, we finish the data cleaing and export to get the mydata_clean.csv
write.table(mydata_clean, file="mydata_clean.csv",sep=",",row.names=F)
