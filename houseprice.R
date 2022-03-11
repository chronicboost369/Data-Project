setwd("E:/jhk/Competition_Kaggle/House Price")

# Packages
library("tidyverse")
library("corrplot")
library(RColorBrewer)
library("randomForest")
##################################################################################################################################
# Data Cleaning
#combine test data files then merging with training data for cleaning
test <- read_csv("test.csv")
test$SalePrice <- 0
train <- read_csv("train.csv")
data <- rbind(train,test)
dim(data) #1460 x 81
sort(colSums(is.na(data)),decreasing=T)

colnames(data)[which(colnames(data)=="1stFlrSF")] <- "firstFlrSF"
colnames(data)[which(colnames(data)=="2ndFlrSF")] <- "secondFlrSF"
## PoolQC
### Based on the dataset description, it's orderly categorical values. Convert to yes or no. Multiple level in regression 
### doesn't usually yield to a good result. Furthermore interpretability becomes more complicated.
### Further more majority, 1453/1460, are NA, which means no pool. So this is dropped.
data <- data%>%select(-PoolQC)

## MiscFeature
### Again, according to the dataset description, miscfeature is another variable with multiple levels of nonorderly categorical variables
### so dropped.
data <- data%>%select(-MiscFeature)

## Alley
### Based on the datadescription, NA is converted as "No" to indicate "No alley access"
data[which(is.na(data$Alley)),"Alley"] <- "No"
table(data$Alley)

### Note: Alley is extremely unbalanced in terms of proportion of factors. If model doesn't perform well, consider removing first.

## Fence
### Like Alley, NA is first converted to "No"
data[which(is.na(data$Fence)),"Fence"] <- "No"
table(data$Fence)

### Note: Fence is extremely unbalanced in terms of proportion of factors. If model doesn't perform well, consider removing first.

## FireplaceQu
### It's an orderly categorical variable. NA can be treated as 0, and this value can be converted to numeric with respect to their levels.
### However, this isn't ideal as the distance between each level is equal. For now, let's just convert NA.
data[which(is.na(data$FireplaceQu)),"FireplaceQu"] <- "No"
table(data$FireplaceQu)

## LotFrontage
### According to the description, this is a continuous variable. However, with having 70+ predictors, using KNN to replace
### NA(most likely missing values) isn't ideal due to the curse of dimensionality, which states that, with high dimensions, all observations
### are numerically similar

data[which(is.na(data$LotFrontage)),]
cor(na.omit(data[,c("LotFrontage","LotArea")])) #cor = 0.426095

### Note: Because observations w/ NA account for about 20% of observations, droppping is not realistic
###       LotArea didn't have high correlation to use KNN to fill in the NA's
###       Choice 1: Later, find a variable with high correlation then use KNN with it.
###       Choice 2: Replace with 0 or median

## GarageType
### Based on the description, it has multiple levels reflecting types of garages, and NA means no garage.
### replace with either yes or no. No includes no garage and detached garage where you'll get wet if rains to get in to the house
### from the car.

data$GarageType <- ifelse(is.na(data$GarageType) | data$GarageType == "Detchd", "No","Yes")

## GarageYrBlt
### Most likely, garage should be built with house if not added later.
### Due to its complexity and prevoius change with garagetype, this variable is dropped.

data<- data%>%select(-GarageYrBlt)

## GarageFinish
### Based on the description, NA is replaced with no
data[which(is.na(data$GarageFinish)), "GarageFinish"] <- "No"

## GarageCond & GarageQual
### Same approach as the previous predictors related to garage
### Furthermore, they seemed to be resembling GarageFinish
### So just dropped

data <- data%>% select(-c(GarageCond,GarageQual))

## BsmtExposure
### Based on the description, NA is replaced with "No" to indicate no basement.
data[which(is.na(data$BsmtExposure)),"BsmtExposure"] <- "No"
table(data$BsmtExposure)

## BsmtFinType1 and BsmtFinType2
### Based on the description, they seem to be redundant. So only BsmtFinType1 is chosen and BsmtFinType2 is dropped.
### NA replaced with no
data[which(is.na(data$BsmtFinType1)),"BsmtFinType1"] <- "No"
table(data$BsmtFinType1)
data <- data%>%select(-BsmtFinType2)


## BsmtQual and BsmtCond
### both are similiar orderly categorical variables. NA indicate no basement so converted to "No"

data[which(is.na(data$BsmtQual)),"BsmtQual"] <- "No"
data[which(is.na(data$BsmtCond)),"BsmtCond"] <- "No"

## MasVnrType
### NA no longer means "No". It's a missing variable. Most likely, same observations are also missing for MasVnrArea
### obs #2611 is missing MasVnrType, and this complicates cleaning other NA variables for MasVnrType. So it's dropped for convenience
data <- data[-2611,]
which(is.na(data$MasVnrArea)) == which(is.na(data$MasVnrType)) # assumption is proven true.
### For now dropped since it only accounts for a small percentage of observations. Using KNN is not considered because
### same observations for MasVnrType, a categorical variable, is missing so just removed.
data <- data[-which(is.na(data$MasVnrArea)),]

## Electrical
### A somewhat orderly categorical variable, NA is a missing variable.
### Replaced it to a lable indicating "Average"
data[which(is.na(data$Electrical)), "Electrical"] <- "FuseA"

#################################################################################################################################
# Replacing Missing values using KNN for LotFrontage

numericcol <- unlist(lapply(data,is.numeric)) # selecting only numeric columns
sort((abs(cor(na.omit(data[,numericcol]))[,2])),decreasing=T) # correlation with LotFrontage
### Note: The highest abs cor is 0.488045151, which isn't high enough for KNN imputation
### Approach taken, assuming KNN can outperform replacing the missing values with median or mean. Top 3 correlated variables are used.
library("VIM")
data.knn <- kNN(data,k=5,variable="LotFrontage",dist_var=c("1stFlrSF", "LotArea", "GrLivArea"), imp_var=F)

data$LotFrontage <- data.knn$LotFrontage
###############################################################################################################################
# More Cleaning
sort(colSums(is.na(data)),decreasing=T) # still total of 22 observations have NA across 16 predictors.
# For convenience, all observations are dropped.

data <- na.omit(data)

###############################################################################################################################
# Categorical Cleaning

## Changing character variables to factors
data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], 
                                           as.factor)
sapply(data[,unlist(lapply(data,is.factor))], function(x) length(levels(x)))

## HeatingQC, a level = "Po" has only 2 obs, merging with Fa
data[which(data$HeatingQC == "Po"), "HeatingQC"] <- "Fa"

# Exterior1st and Exterior2nd -> remove them due to high level and unproportional

data <- data%>%select(-c(Exterior1st,Exterior2nd))

############################################################################################################################
# Modeling
train <- data[data$Id%in%train$Id,]
test <- data[data$Id%in%test$Id,]
train <- train%>%select(-Id) #drop id
numericcol <- numericcol[-which(names(numericcol)=="Id")] # drop id

numericcol <- colnames(train)[numericcol]
factorcor <- colnames(train)[unlist(lapply(train,is.factor))]

## Categorical Variable Decision
### Some Variables' levels are too high for this dataset, which isn't ideal for regression.
### Reasons are: 1. Interpretation is hard 2. Not enough observations for each levels for a reliable analysis
### Ideal categorical variable should have proportion distribution across levels and their mean of SalePrice should be different

### Checking for Proportions first

proportiontable <- matrix(0,
                          nrow=length(factorcor),
                          ncol=max(sapply(data[,unlist(lapply(data,is.factor))], function(x) length(levels(x)))) )
rownames(proportiontable) <- factorcor



for(i in 1:length(factorcor)){
  probs <- unlist(table(train[,factorcor[i]])/nrow(train)*100)
  for( j in 1:length(probs)){
    proportiontable[i,j] <- probs[j]
  }
}

### Maximum proportion(percentage) of a level for each predictor
apply(proportiontable,1,max)

### Any proportion greater than 70% is considered as skewed proportion thus removed

badproportion <- names(apply(proportiontable,1,max)[apply(proportiontable,1,max)>=70])

train <- train%>%select(-badproportion)


## Correlation Check 
### Multicollinearity only doesn't allow perfect correlation but high correlation(above 0.8 for abs value) is still worth exploring.

numericcol <- names(sapply(train,is.numeric)[sapply(train,is.numeric)])
cortable <- abs(cor(train[,numericcol]))
cortable.names <- matrix(0,
                          nrow=length(numericcol),
                          ncol=length(numericcol) )
rownames(cortable.names) <- numericcol

cortable
for ( i in 1:nrow(cortable)){
  for (j in 1:ncol(cortable)){
    if(cortable[i,j] >= 0.8 & cortable[i,j] < 1){
      cortable.names[i,j] <- colnames(cortable)[j]
    }
  }
}

View(cortable.names)

### After evaluating combinations of predictors with abs. value of correlation higher than 0.8,
### GarageCars is removed as it is redundant with GarageArea
train <- train%>%select(-GarageCars)


## Evaluating numeric variables' relationship with the response variable for higher order terms
numericcol <- colnames(train)[sapply(train[index,], is.numeric)]
plot(train[index,numericcol])
for(i in 1:length(numericcol)){
  plot(y= pull(train[index,"SalePrice"]) , x= pull(train[index,numericcol[i]]) , main= paste("SalePrice vs.", numericcol[i]))
}

remove.numcol <- c( "YrSold", "MoSold", "MiscVal", "PoolArea", "ScreenPorch", "3SsnPorch", "EnclosedPorch", 
                    "OpenPorchSF", "WoodDeckSF", "KitchenAbvGr", "HalfBath", "BsmtHalfBath", "BsmtFullBath",
                    "LowQualFinSF", "BsmtFinSF2")

### Note: remove YrSold b/c it's categorical; remove MoSold b/c no much variation in price for difference in selling month (not 100%);
###       remove MiscVal; remove PoolArea b/c mostly 0; remove ScreenPorch b/c mostly 0; remove 3SsnPorch b/c mostly 0;
###       remove EnclosedPorch(not 100%) b/c mostly 0 and no significant diff.; remove OpenPorchSF b/c not showing significant relationship
###       remove WoodDeckSF b/c not significant relationship; convert Fireplaces as factor; remove KitchenAbvGr as not significant;
###       remove HalfBath as not significant; remove BsmtHalfBath as unproportional; remove BsmtFullBath as not significant;
###       remove LowQualFinSF as unproportional; remove BsmtFinSF2 seems unproportional and no significant;
###       

### Note: OverallCond may be higher order; OverallQual may be higher order; LotArea may be higher order; LotFrontage may be higher order;
###       

train$Fireplaces <- as.factor(train$Fireplaces)
test$Fireplaces <- as.factor(test$Fireplaces)

train <- train%>%select(-remove.numcol)


## Removing additional catetorical variables because some predictors have low counts for a level and causes problem 
## during simulation for not having an observation related to that 

#proportiontable <- matrix(0,
                          #nrow=length(colnames(train)[sapply(train,is.factor)]),
                          #ncol=max(sapply(train[,colnames(train)[sapply(train,is.factor)]], function(x)length(levels(x)))) )
#rownames(proportiontable) <- colnames(train)[sapply(train,is.factor)]


#for(i in 1:length(colnames(train)[sapply(train,is.factor)])){
#  probs <- unlist(table(train[,colnames(train)[sapply(train,is.factor)][i]]))/nrow(train)*100
#   for( j in 1:length(probs)){
    #proportiontable[i,j] <- probs[j]
  #}
#}
#unproportion.cat <- names(apply(proportiontable,1,function(x) max(x)-min(x))[apply(proportiontable,1,function(x) max(x)-min(x)) >=30])

#length(unproportion.cat)
#dim(train)
###########################################################################################################################################
# Outliers
hist(train$SalePrice)
### with eve R automatically setting binwidth, one can easily inference that the distribution of SalePrice is skewed to the right.
### In fact, this is true for most variables related to assets.

### For convenience, 95% CI is derived by rank

nonoutliers <- sort(train$SalePrice,decreasing = F)[floor(nrow(train)*0.05):ceiling(nrow(train)*0.95)]

train <- train[(which(train$SalePrice %in% nonoutliers)),]
hist(train$SalePrice) #still skewed but improved



###################################################################################################################################
# Modeling

set.seed(123456)
index <- sample(1:nrow(train), nrow(train)*.7, replace=F )

## Model 1: MLR

model1 <- lm(SalePrice~., data=train[index,])
summary(model1)
par(mfrow=c(2,2))
plot(model1)

dev.off()

model1.df <- data.frame(fitted=model1$fitted.values, real= train[index,"SalePrice"])



### Note: Normality assumption for residual is not met. But, many variables have extremely high p-value.
###       Because Normality assumption is not met, p-value should not be trusted, but some of them are way too high.


test.data <- train[-index,]

                        
sqrt(mean((predict(model1,newdata=test.data[-index,]) - test.data$SalePrice)^2)) # RMSE = $ 80,731.02

### Note: Model 1 has extremely poor performance.



### Model 2: Applying higher order terms for those variables that are suspected to have higher order relationship with the response
### Note: OverallCond may be higher order; OverallQual may be higher order; LotArea may be higher order; LotFrontage may be higher order;

highorderval <- c( "OverallCond" , "OverallQual", "LotArea", "LotFrontage", "SalePrice" )
model2.call <- c( "poly(OverallCond,2)", "poly(OverallQual,2)", "poly(LotArea,2)", "poly(LotFrontage,2)", colnames(train)[!colnames(train)%in%highorderval])
model2.call <- paste(model2.call, collapse = "+")

model2 <- lm( formula(paste("SalePrice ~ ",model2.call)), data=train[index,])
summary(model2)
par(mfrow=c(2,2))
plot(model2)
dev.off()
sqrt(mean((predict(model2,newdata=test.data) - (test.data$SalePrice))^2)) #test RMSE = $24873.12

### if Polynomial is increased to 3

train <- train%>%select(-Fireplaces)


model2.call <- c( "poly(OverallCond,3)", "poly(OverallQual,3)", "poly(LotArea,3)", "poly(LotFrontage,3)", colnames(train)[!colnames(train)%in%highorderval])
model2.call <- paste(model2.call, collapse = "+")
index.model2 <- train[-which(index %in% c(568,142,542)),]
model2 <- lm( formula(paste("SalePrice ~ ",model2.call)), data= index.model2 )
summary(model2)
par(mfrow=c(2,2))
plot(model2)
dev.off()
sqrt(mean((predict(model2,newdata=test.data) - test.data$SalePrice)^2))  #test RMSE = $17,611.72 or 11% of the median SalePrice


nrow(test)


write.csv(data.frame(Id= test$Id,SalePrice= predict(model2,newdata=test)),"submission.csv", row.names = F)




#############################################################################################################################
# Current Status
# Outliers removed, about 5/8 of variables are removed but still seeing quite high test RMSE
# Potential solutions: 1. Regularization to reduce variance at the cost of bias 2. Fix bias 3. Variable selection to prevent overfitting
# While 1 or 2 can be done, 3 is bit challenging because residuals are not normally distributed.


###############################################################################################################################
# Variable Selection with RandomForest
### RandomForest's making tree with boostrapping with randomly selected predictors can reveal few variables that explain the variance the most

rf <- randomForest(SalePrice~., data=train[index,], mtry=floor((ncol(train)-1)/3), importance=T, ntree=1000)
varImpPlot(rf)
importance(rf)
sqrt(mean((predict(rf,newdata=test.data) - test.data$SalePrice)^2)) # test RMSE = 20257.16 or 13% of the median SalePrice



### From randomforest, top 5 important variables are GrLivArea, Neighborhood, OverallQual, TotalBsmtSF,firstFlrSF.
### Of these, all are quantitative except the neightboord

# Model 3

model3 <- lm(SalePrice ~ poly(GrLivArea,2)+Neighborhood+poly(OverallQual,2)+poly(TotalBsmtSF,2)+poly(firstFlrSF,2), data=train[index,])
sqrt(mean((predict(model3,newdata=test.data) - test.data$SalePrice)^2)) # $24,463.49

### trying with top 15 variables
top15 <- names(sort(importance(rf)[,1],decreasing=T)[1:15])
top15.num <- top15[sapply(train[,top15],is.numeric)]

top15.num.poly <- paste(paste("poly(",top15.num),",2)")

model3.top15 <- lm(formula(paste("SalePrice ~", paste(c(top15.num.poly,"Neighborhood", "ExterQual" ), collapse = "+"))), data=train[index,] )

sqrt(mean((predict(model3.top15,newdata=test.data) - test.data$SalePrice)^2)) # $20,384.66


##############################################################################################################################

### Variable selection with randomForest didn't help at all 
### Measuring variance to see if it's big or not 
set.seed(25256)
rep100 <- replicate(100,sample(nrow(train), nrow(train)*0.7, replace=F))
rmse.table <- matrix(0,nrow=ncol(rep100))

### dropping Fireplace for error in simulation for not having a level in the training set but in the testset
train <- train%>%select(-Fireplaces)

model2.call <- "poly(OverallCond,3)+poly(OverallQual,3)+poly(LotArea,3)+poly(LotFrontage,3)+MSSubClass+Neighborhood+HouseStyle+YearBuilt+YearRemodAdd+MasVnrType+
                MasVnrArea+ExterQual+Foundation+BsmtQual+BsmtExposure+BsmtFinType1+BsmtFinSF1+BsmtUnfSF+TotalBsmtSF+HeatingQC+firstFlrSF+secondFlrSF+GrLivArea+FullBath+
                BedroomAbvGr+KitchenQual+TotRmsAbvGrd+FireplaceQu+GarageType+GarageFinish+GarageArea"

sapply(train[,names(train)[sapply(train,is.factor)]], table)


for ( i in 1:ncol(rep100)){
  fit <- lm( formula(paste("SalePrice ~ ",model2.call)), data= train[rep100[,i],] )
  pred <- predict(fit, newdata=train[-rep100[,i],])
  rmse.table[i,1] <- sqrt(mean((pred - pull(train[-rep100[,i],"SalePrice"]))^2))
}
rmse.table


