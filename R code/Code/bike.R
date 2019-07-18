rm(list=ls(all=T))

setwd("C:/Users/ASUS/Desktop/Edwisor Training/Project-2_Bike rental prediction/R code")

#Sample to install a package
#install.packages('ggplot2')


x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees','fastDummies')

lapply(x, require, character.only = TRUE)

rm(x)


#Load CSV
bike_data = read.csv("day.csv", header = T, na.strings = c(" ", "", "NA"))


#Load data to another dataframe to avoid modification in original loaded data
ana_data=bike_data

######Exploratory Data Analysis##################
#Taking a glance through the data and it's feature types 
#str(ana_data)

#dim(ana_data)
#We have 16 features and 731 obervations 
#taking a glance at data
#head(ana_data)
#As we can see lot of variables are actually should be categorical but are having integer type
#So we will need to convert them to appropriate type
#Season
#
#Data type conversion

ana_data$season=as.factor(ana_data$season)

ana_data$mnth=as.factor(ana_data$mnth)

ana_data$yr=as.factor(ana_data$yr)

ana_data$holiday=as.factor(ana_data$holiday)

ana_data$weekday=as.factor(ana_data$weekday)

ana_data$workingday=as.factor(ana_data$workingday)

ana_data$weathersit=as.factor(ana_data$weathersit)

#Removing unnessary features from dataset as they are of no use 
#for us
#As per discription about features
#instant: Record index 
#casual: count of casual users 
#registered: count of registered user
ana_data=subset(ana_data,select = -c(instant,casual,registered))

#converting dates
d1=unique(ana_data$dteday)

#str(d1)

df=data.frame(d1)

ana_data$dteday=as.Date(df$d1,format="%Y-%m-%d")

#has been converted to date format
#Let's have a look
str(ana_data$dteday)

#head(df)
#changing to categorical
df$d1=as.Date(df$d1,format="%Y-%m-%d")

ana_data$dteday=format(as.Date(df$d1,format="%Y-%m-%d"), "%d")

#str(ana_data$dteday)

ana_data$dteday=as.factor(ana_data$dteday)



#All required features have been converted now ...let's recheck
#str(ana_data)

#Successful conversion


###Missing Values Analysis###############################################



# 1. checking for missing value

colSums(sapply(ana_data, is.na))
sum(is.na(ana_data)) / (nrow(ana_data) *ncol(ana_data))
#There are no missing values in data

########Checking duplicate values######
cat("The number of duplicated rows are", nrow(ana_data) - nrow(unique(ana_data)))
#There are no duplicate rows present in the dataset




##############Outlier Analysis##########



# 1.BoxPlots - Distribution and Outlier Check
#Check numeric data first 
numeric_index = sapply(ana_data,is.numeric) 
numeric_data = ana_data[,numeric_index]
cnames = colnames(numeric_data)

for (i in 1:length(cnames))
  
{
  
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "cnt"), data = subset(ana_data))+ 
           
           stat_boxplot(geom = "errorbar", width = 0.5) +
           
           geom_boxplot(outlier.colour="red", fill = "blue" ,outlier.shape=18,
                        
                        outlier.size=1, notch=FALSE) +
           
           theme(legend.position="bottom")+
           
           labs(y=cnames[i],x="count")+
           
           ggtitle(paste("Count Box plot for",cnames[i])))
  
}



gridExtra::grid.arrange(gn1,gn2,ncol=3)

gridExtra::grid.arrange(gn3,gn4,ncol=2)

#we can see that some of the outliers are there (in red colour)
#####Outlier analysis and treatment for Humidity#####
#Detecting Outlier on the boxplot

outlier_values <- boxplot.stats(ana_data$hum)$out 
boxplot(ana_data$hum, main="Humidity", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

###Cook's distance approach
#select feature
#Coefficient

mod <- lm(hum ~ ., data=ana_data)

#calculate cook's distance - It computes the influence 
#exerted by each data point (row) on the predicted outcome.
#cook's distance greater than 4 times 
#the mean may be classified as influential.

cooksd <- cooks.distance(mod)

#Plot
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels

#finding Influence
influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])
head(ana_data[influential, ])

#The function outlierTest from car package 
#gives the most extreme observation based on the given model
#install.packages('car')
library('car')
car::outlierTest(mod)

#here we can see in the output that 69th observation 
#is most influenced

#Treating Outlier by using Capping
x <- ana_data$hum
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
#Commenting this because outliers removal was affecting Model accuracy
#We can neglect those Outliers because they are having minor impact
#ana_data$hum=x

#Checking outliers now on the plot
#outlier_values <- boxplot.stats(ana_data$hum)$out 
#boxplot(ana_data$hum, main="Humidity", boxwex=0.1)
#mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

#Now there are no Outliers present

#####Outlier analysis and treatment for windspeed#####
#Detecting Outlier on the boxplot

outlier_values <- boxplot.stats(ana_data$windspeed)$out 
boxplot(ana_data$windspeed, main="windspeed", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

#We can see 13 outliers there
###Cook's distance approach
#select feature
#Coefficient

mod <- lm(windspeed ~ ., data=ana_data)

#calculate cook's distance - It computes the influence 
#exerted by each data point (row) on the predicted outcome.
#Calculating Cook's distance

cooksd <- cooks.distance(mod)

#Plot
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels

#finding Influence
influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])
#head(ana_data[influential, ])

#The function outlierTest from car package 
#gives the most extreme observation based on the given model
#install.packages('car')
#library('car')
car::outlierTest(mod)

#here we can see in the output that 595th observation 
#is most influenced

#Treating Outlier by using Capping
x <- ana_data$hum
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]

#Commenting this because outliers removal was affecting Model accuracy
#We can neglect those Outliers because they are having minor impact
#ana_data$windspeed=x

#Checking outliers now on the plot
#outlier_values <- boxplot.stats(ana_data$windspeed)$out 
#boxplot(ana_data$hum, main="windspeed", boxwex=0.1)
#mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

#Now there are no Outliers present

#####################Feature Selection#################

#Finding features with high correlation
#colnames(data)
rel = cor(ana_data[,numeric_index])
#rel
#Here we can summarize correlation of numeric data with count
#feature


## Correlation Plot 

corrgram(ana_data[,numeric_index], order = F,
         
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")







## Dimension Reduction####


#Remove atemp
ana_data = subset(ana_data,select = -c(atemp))





####### Models #########################



#####Data shuffeling and train test split#####
rmExcept("ana_data")

##We can use below lines as well:
#head(ana_data)

#shuffle_index = sample(1:nrow(ana_data))

#head(shuffle_index)

#ana_data <- ana_data[shuffle_index, ]

#head(ana_data)

#########Train_test split#########
#install.packages("rpart.plot")
#suffling and splitting
library(rpart.plot)

#sample out 80% of data from whole training data
#choose random indexing from data
train_index = sample(1:nrow(ana_data), 0.8 * nrow(ana_data))

#train_index
#sample
#x = 0.8*nrow(ana_data)
#x
#head(train_index)
#Split into train by taking above index
train = ana_data[train_index,]

#split into test by removing above index rows
test = ana_data[-train_index,]

#Models

###########Decision tree regression  #################
#Anova for regression tree
#class for classification tree
#rpart is the function to apply Dicision tree

fit = rpart(cnt ~ ., data = train, method = "anova")
cnt ~ 

predict_DT = predict(fit, test[,-12])

head(predict_DT)
#removed Count column for prediction

#plot tree
rpart.plot(fit,extra = 101)

#############Random Forest Model##########################

#model_RF = randomForest(cnt ~ ., train, importance = TRUE, ntree = 250)
#model_RF = randomForest(cnt ~ ., train, importance = TRUE, ntree = 300)
#model_RF = randomForest(cnt ~ ., train, importance = TRUE, ntree = 150)
#model_RF = randomForest(cnt ~ ., train, importance = TRUE, ntree = 170)
#model_RF = randomForest(cnt ~ ., train, importance = TRUE, ntree = 500)
#Perfect eror fall below 

model_RF = randomForest(cnt ~ ., train, importance = TRUE, ntree = 200)

predict_RF = predict(model_RF, test[,-12])

plot(model_RF)

################Linear Regression#################



#converting multilevel categorical variable into binary dummy variable

cnames= c("dteday","season","mnth","weekday","weathersit")

data_lr=ana_data[,cnames]

cnt=data.frame(ana_data$cnt)

names(cnt)[1]="cnt"

#creating dummy columns

data_lr <- fastDummies::dummy_cols(data_lr)

data_lr= subset(data_lr,select = -c(dteday,season,mnth,weekday,weathersit))
#head(data_lr)

#Appending datset after Dummies

d3 = cbind(data_lr,ana_data)
#head(d3)

#Droping some features

d3= subset(d3,select = -c(dteday,season,mnth,weekday,weathersit,cnt))

#d3
data_lr=cbind(d3,cnt)





#dividind data into test and train

train_index = sample(1:nrow(data_lr), 0.8 * nrow(data_lr))

train_lr = data_lr[train_index,]

test_lr = data_lr[-train_index,]



#Linear regression model making

model_lm = lm(cnt ~., data = train_lr)

predict_LR = predict(model_lm,test_lr[,-64])





#summary(model_lm)



#################evaluating MApe value###############





MAPE = function(y, yhat){
  
  mean(abs((y - yhat)/y))*100
  
}

MAPE(test[,12], predict_DT)



MAPE(test[,12], predict_RF)



MAPE(test_lr[,64],  predict_LR)

#14.55029
#13.23267
#18.07619

#so least is second one using Random forest , so we will use that 
#for prediction






##########extacting predicted values output from Random forest model######################

results <- data.frame(test, pred_cnt = predict_RF)



write.csv(results, file = 'RF output R .csv', row.names = FALSE, quote=FALSE)


rm(list=ls(all=T))
