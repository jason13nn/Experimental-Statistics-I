##################################
###########Final Project##########
##################################

###Q1###
#read dataset
train1<-read.csv("/Users/jason13nn/Desktop/SMU/Fall 2019/STAT 6301(Experimental Stat)/Project/data/train.csv")

#Subset data
train1<-subset(train1,Neighborhood=="NAmes" | Neighborhood=="Edwards" | Neighborhood=="BrkSide",
               select = c(SalePrice, GrLivArea, Neighborhood))

#Normality of Y
ggplot(train1, aes(x=SalePrice))+
  geom_histogram(color="lightblue")+
  stat_bin(binwidth = 30)

#Correlation
pairs(~SalePrice + GrLivArea, data = train1, main="Simple Scatterplot")

#Diagnostic Plot
q1.lm <- lm(SalePrice ~ GrLivArea+ Neighborhood+ GrLivArea*Neighborhood, data = train1)
q1.lm

par(mfrow=c(2,2))
plot(q1.lm)

#Interaction
q1.lm2 <- lm(SalePrice ~ GrLivArea+ Neighborhood, data = train1)
anova(q1.lm2, q1.lm)
#there is interaction effect

#Add Influence Statistics
train1<-transform(train1, hat=hatvalues(q1.lm))
train1<-transform(train1, studres=studres(q1.lm))
train1<-transform(train1, cooks=cooks.distance(q1.lm))

#Find outliers 
subset(train1, studres> 4)

#calculate Anova for each 
a1<-subset(new3,Neighborhood=="NAmes")
a1 <- setnames(a1,"Neighborhood","NeiNAmes")
a1a<-lm(SalePrice~GrLivArea,data = a1)
anova(a1a)
a2<-subset(new3,Neighborhood=="BrkSide")
a2a<-lm(SalePrice~GrLivArea,data = a2)
anova(a2a)
a3<-subset(new3,Neighborhood=="Edwards")
a3a<-lm(SalePrice~GrLivArea,data = a3)
anova(a3a)

mutinova<-aov((SalePrice/GrLivArea)~Neighborhood,new3)
TukeyHSD(mutinova)
#graph for conclusion 
ggplot(last,aes(last$Group.1,SalePrice,fill=last$Group.1))+geom_bar(stat="identity",width = 0.3)+
  labs(title="Average Price for Each Neighborhood",x="Neighborhood")+
  theme(plot.title = element_text(hjust = 0.5))
ggplot(new3,aes(x=GrLivArea, y=SalePrice,colour=Neighborhood))+
  geom_point()+
  geom_smooth(method = "lm")+
  #facet_grid(.~Neighborhood)+
  scale_y_continuous(breaks=seq(0, 350000, 20000),labels = dollar)+
  scale_x_continuous(breaks=seq(0, 5000, 250))





###Q2###
###Data Cleaning###

#install.packages("VIM")
#install.packages("tidyverse")
library(VIM)
library(tidyverse)

#Read data
#read datasets
train<-read.csv("/Users/jason13nn/Desktop/SMU/Fall 2019/STAT 6301(Experimental Stat)/Project/data/train.csv")
test<-read.csv("/Users/jason13nn/Desktop/SMU/Fall 2019/STAT 6301(Experimental Stat)/Project/data/test.csv")

SalePrice<-train$SalePrice

#view train data
summary(train)
str(train)

#Plots for Problematic Variables
library(ggplot2)
ggplot(train, aes(x=Street,fill=Street))+
  geom_bar(width = 0.3)
ggplot(train, aes(x=Utilities,fill=Utilities))+
  geom_bar(width = 0.3)
ggplot(train, aes(x=Condition2,fill=Condition2))+
  geom_bar(width = 0.3)
ggplot(train, aes(x=RoofMatl,fill=RoofMatl))+
  geom_bar()
ggplot(train, aes(x=Heating,fill=Heating))+
  geom_bar()

#Merge datasets
train<-train[,-81]
data<-rbind(train,test)

##Delete Variables(too many missing value, class imbalanced)
data$Alley<-NULL
data$PoolQC<-NULL
data$Fence<-NULL
data$MiscFeature<-NULL
data$Street<-NULL
data$Utilities<-NULL
data$Condition2<-NULL
data$FireplaceQu<-NULL 
data$MiscVal<-NULL 
data$RoofMatl<-NULL
data$Heating<-NULL 
#69 variables left

#subset numeric and categorical variables
data$MSSubClass<-as.factor(data$MSSubClass)
data_factor<-names(data)[which(sapply(data,is.factor))]
data_numeric<-names(data)[which(sapply(data,is.numeric))]

##Single Imputation
#See the number of NAs for each variable
sapply(data, function(x) sum(is.na(x)))

#Impute missing values
data <- hotdeck(data)
#View(train)

#Check if there are stll have any NA 
sapply(data, function(x) sum(is.na(x)))

data<-data[,1:69]

data_factor
data_numeric

##transformation of numeric data
#check linearity between SalePrice and Each Predictor variable
ggplot(data=train) +
  geom_point(mapping = aes(x=LotFrontage,y=SalePrice))

ggplot(data=train) +                       ##transformation of LotArea
  geom_point(mapping = aes(x=log(LotArea),y=SalePrice))

ggplot(data=train) +
  geom_point(mapping = aes(x=OverallQual,y=SalePrice))

ggplot(data=train) +
  geom_point(mapping = aes(x=OverallCond,y=SalePrice))

ggplot(data=train) +
  geom_point(mapping = aes(x=YearBuilt,y=SalePrice))

ggplot(data=train) +
  geom_point(mapping = aes(x=YearRemodAdd,y=SalePrice))

ggplot(data=train) +
  geom_point(mapping = aes(x=MasVnrArea,y=SalePrice))

ggplot(data=train) +
  geom_point(mapping = aes(x=BsmtFinSF1,y=SalePrice))

ggplot(data=train) +
  geom_point(mapping = aes(x=BsmtFinSF2,y=SalePrice))

ggplot(data=train) +
  geom_point(mapping = aes(x=BsmtUnfSF,y=SalePrice))

ggplot(data=train) +
  geom_point(mapping = aes(x=TotalBsmtSF,y=SalePrice))

ggplot(data=train) +
  geom_point(mapping = aes(x=X1stFlrSF,y=SalePrice))

ggplot(data=train) +
  geom_point(mapping = aes(x=X2ndFlrSF,y=SalePrice))

ggplot(data=train) +
  geom_point(mapping = aes(x=LowQualFinSF,y=SalePrice))

ggplot(data=train) +
  geom_point(mapping = aes(x=GrLivArea,y=SalePrice))

ggplot(data=train) +
  geom_point(mapping = aes(x=BsmtFullBath,y=SalePrice))

ggplot(data=train) +
  geom_point(mapping = aes(x=BsmtHalfBath,y=SalePrice))

ggplot(data=train) +
  geom_point(mapping = aes(x=FullBath,y=SalePrice))

ggplot(data=train) +
  geom_point(mapping = aes(x=HalfBath,y=SalePrice))

ggplot(data=train) +
  geom_point(mapping = aes(x=BedroomAbvGr,y=SalePrice))

ggplot(data=train) +
  geom_point(mapping = aes(x=KitchenAbvGr,y=SalePrice))

ggplot(data=train) +
  geom_point(mapping = aes(x=TotRmsAbvGrd,y=SalePrice))

ggplot(data=train) +
  geom_point(mapping = aes(x=Fireplaces,y=SalePrice))

ggplot(data=train) +
  geom_point(mapping = aes(x=GarageYrBlt,y=SalePrice))

ggplot(data=train) +
  geom_point(mapping = aes(x=GarageCars,y=SalePrice))

ggplot(data=train) +
  geom_point(mapping = aes(x=GarageArea,y=SalePrice))

ggplot(data=train) +
  geom_point(mapping = aes(x=WoodDeckSF,y=SalePrice))

ggplot(data=train) +
  geom_point(mapping = aes(x=OpenPorchSF,y=SalePrice))

ggplot(data=train) +
  geom_point(mapping = aes(x=EnclosedPorch,y=SalePrice))

ggplot(data=train) +
  geom_point(mapping = aes(x=X3SsnPorch,y=SalePrice))

ggplot(data=train) +
  geom_point(mapping = aes(x=ScreenPorch,y=SalePrice))

ggplot(data=train) +
  geom_point(mapping = aes(x=PoolArea,y=SalePrice))

ggplot(data=train) +
  geom_point(mapping = aes(x=MoSold,y=SalePrice))

ggplot(data=train) +
  geom_point(mapping = aes(x=YrSold,y=SalePrice))

#tramsformation of LotArea
data$log_LotArea<-log(data$LotArea)
data <- subset(data, select = -c(LotArea))

#install.packages("dummies")
library(dummies)

#Transform categorical variables into Dummy variables
dummy1<-dummy(data$LotShape)
dummy2<-dummy(data$LandContour)
dummy3<-dummy(data$LotConfig)
dummy4<-dummy(data$LandSlope)
dummy5<-dummy(data$Neighborhood)
dummy6<-dummy(data$Condition1)
dummy7<-dummy(data$BldgType)
dummy8<-dummy(data$HouseStyle)
dummy9<-dummy(data$RoofStyle)
dummy10<-dummy(data$Exterior1st)
dummy11<-dummy(data$Exterior2nd)
dummy12<-dummy(data$MasVnrType)
dummy13<-dummy(data$ExterQual)
dummy14<-dummy(data$ExterCond)
dummy15<-dummy(data$Foundation)
dummy16<-dummy(data$BsmtQual)
dummy17<-dummy(data$BsmtCond)
dummy18<-dummy(data$BsmtExposure)
dummy19<-dummy(data$BsmtFinType1)
dummy20<-dummy(data$BsmtFinType2)
dummy21<-dummy(data$HeatingQC)
dummy22<-dummy(data$CentralAir)
dummy23<-dummy(data$Electrical)
dummy24<-dummy(data$KitchenQual)
dummy25<-dummy(data$Functional)
dummy26<-dummy(data$GarageType)
dummy27<-dummy(data$GarageFinish)
dummy28<-dummy(data$GarageQual)
dummy29<-dummy(data$GarageCond)
dummy30<-dummy(data$PavedDrive)
dummy31<-dummy(data$SaleType)
dummy32<-dummy(data$SaleCondition)
dummy33<-dummy(data$MSZoning)
dummy34<-dummy(data$MSSubClass)

#Combine Dummy Variables 
dummies <- dummy1
for(i in 2:34) {
  dummy_i <- eval(parse(text = paste("dummy", i, sep = "")))
  dummies <- cbind(dummies, dummy_i)
}
dim(dummies)
#Delete all the categorical variables
data<-data[,sapply(data, is.numeric)]

#Add Dummy Variables into Data
data<-cbind(data,dummies)

#Split dataset
train<-data[1:1460,]
train<-cbind(train,SalePrice)

test<-data[1461:2919,]

###Selection Methods###
#install.packages("glmnet")
#install.packages("caret")
library(glmnet)
library(caret)
require(methods)

##1. Ridge Regression
#select model
train_ridge<-cv.glmnet(
  x = as.matrix(train[,2:259]),
  y = train[,260],
  alpha = 0
)
attributes(train_ridge)

summary(train)

##Optimal tuning parameter
best.lambda <- train_ridge$lambda.min

##Check parameter estimates for the optimal model
coef(train_ridge, s=best.lambda)

##2. LASSO
train_lasso<-cv.glmnet(
  x = as.matrix(train[,2:259]),
  y = train[,260],
  alpha = 1
)

#Optimal tuning parameter
best.lambda2<-train_lasso$lambda.min

#Check parameter estimate for the optimal model
coef(train_lasso, s = best.lambda2)

##3. Elastic Net
tcontrol<-trainControl(method = "repeatedcv", number = 10, repeats = 5)
train_en<-train(
  x = as.matrix(train[,2:259]),
  y = train[,260],
  trControl = tcontrol,
  method = "glmnet",
  tuneLength = 10
)

#Best combination of tuning parameters
train_en$bestTune

train_en.best<- train_en$finalModel
coef(train_en.best, s = train_en$bestTune$lambda)

##Test Prediction

ridge.pred <- predict(train_ridge, as.matrix(train[,2:259]), s=best.lambda)
ridge.rmse <- sqrt(mean((ridge.pred - train[,260])^2))

lasso.pred <- predict(train_lasso, as.matrix(train[,2:259]), s=best.lambda2)
lasso.rmse <- sqrt(mean((lasso.pred - train[,260])^2))

en.pred <- predict(train_en, as.matrix(train[,2:259]), s=train_en$bestTune$lambda)
en.rmse <- sqrt(mean((en.pred - as.matrix(train[,260]))^2))


results <- data.frame(Method=c('Ridge', 'Lasso', 'Elastic Net'), 
                      RMSE=c(ridge.rmse, lasso.rmse, en.rmse))
results

##Make Prediction on test.csv data set
test_ridge_pred <- predict(train_ridge, as.matrix(test[,2:259]), s=best.lambda)
test_lasso_pred <- predict(train_lasso, as.matrix(test[,2:259]), s=best.lambda2)
test_en_pred <- predict(train_en, as.matrix(test[,2:259]), s=train_en$bestTune$lambda)


##Final Predict Result
ID<-c(1461:2919)

#Ridge Prediction
test_pred1<-cbind(ID,test_ridge_pred)
dim(test_pred1)
colnames(test_pred1)<-c("ID","SalePrice")
write.csv(test_pred1,file="/Users/wang/Desktop/test_a.csv")

#Lasso Prediction
test_pred2<-cbind(ID,test_lasso_pred)
dim(test_pred2)
colnames(test_pred2)<-c("ID","SalePrice")
write.csv(test_pred2,file="/Users/wang/Desktop/test_b.csv")

#ElasticNet Prediction
test_pred3<-cbind(ID,test_en_pred)
dim(test_pred2)
colnames(test_pred3)<-c("ID","SalePrice")
write.csv(test_pred3,file="/Users/wang/Desktop/test_c.csv")


##Ridge model has lowest RMSE based, but Elastic Net regression
##is finally selected based on the lowest Kaggle score

##Check parameter estimates for the optimal model
train_en_final<-train_en$finalModel
betas_en<-coef(train_en_final, s=train_en$bestTune$lambda)
betas_en<-as.matrix(betas_en)
length(betas_ridge)
betas_en<-data.frame(rownames(betas_en)[2:259],betas_en[-c(1)])
colnames(betas_en)<-c("Predictor","Beta")
betas_en<-betas_en[betas_en$Beta!=0,]

#Get predictors from Ridge Regression selection
predictors_en<-betas_en[,1]

#Fit a model
train_en1<-cbind(train[,predictors_en],SalePrice)
en_lm <- lm(SalePrice ~., data = train_en1)

#Diagnostic plots
par(mfrow=c(2,2))
plot(en_lm)

#install.packages("MASS")
library(MASS)
#Add leverage, studentized residuals, and Cook's D to data set
train_en1 <- transform(train_en1, hat = hatvalues(en_lm))
train_en1 <- transform(train_en1, studres = studres(en_lm))
train_en1 <- transform(train_en1, cooks = cooks.distance(en_lm))

#plot studentized graph
ggplot(data=train_en1) +
  geom_point(mapping = aes(x=hat,y=studres))+
  geom_hline(yintercept=5, linetype="dashed", color = "red")+
  geom_hline(yintercept=-5, linetype="dashed", color = "red")

#plot Cook's D graph
ggplot(data=train_en1) +
  geom_point(mapping = aes(x=hat,y=cooks))+
  geom_hline(yintercept=1, linetype="dashed", color = "red")

#Find outliers
subset(train_en1, studres> 5)

#final data set for Elastic Net regression
train_en2<-train_en1[,-(92:95)]
en_lm <- lm(SalePrice ~., data = train_en2)
coefficients(en_lm)

#Coefficients
betas_en<-coefficients(en_lm)
betas_en<-as.matrix(betas_en)
length(betas_en)
betas_en<-data.frame(rownames(betas_en)[2:92],betas_en[-c(1)])
colnames(betas_en)<-c("Predictor","Beta")
betas_en<-na.omit(betas_en)

#sort by beta
betas_en<-betas_en[order(-betas_en$Beta),]
betas_en

#Plot the top 20 influencial variables
betas_en_pos<-top_n(betas_en, 5, wt = Beta)
betas_en_neg<-top_n(betas_en, 5, wt = -Beta)

#Positive influence
ggplot(betas_en_pos, aes(x = Beta, y = Predictor)) +
  geom_point() +
  ggtitle("Top 5 positive influential variables") +
  xlab("Coefficient") +
  ylab(NULL)

#Negative influence
ggplot(betas_en_neg, aes(x = Beta, y = Predictor)) +
  geom_point() +
  ggtitle("Top 5 negative influential variables") +
  xlab("Coefficient") +
  ylab(NULL)

#######END#########
###################
