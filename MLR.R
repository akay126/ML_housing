data = read.csv('data/my_train.csv', stringsAsFactors =  TRUE)
test = read.csv('data/my_test.csv' , stringsAsFactors = TRUE)

test = test[-1]
data = data[-1]
summary(data)
top = data %>% top_n(10)
a = data[which(data$MSZoning == "C (all)"),]

b = test[which(test$MSZoning == "C (all)"),]

test = test %>% drop_na()


colnames(data)

# data[is.na(data)] <- 'None'
# data <- data %>% mutate_if(is.character,as.factor)

# class(test[,2])
# test <- test %>% mutate_if(is.character,as.factor)
test$MSZoning
data$MSZoning

# data1 = data[c(1:5,7:8,10:71,75:80)]
model.full = lm (SalePrice ~ .,data = data)
# colnames(data[72])
# data[72] = factor(data[72])
# class(data[,72])
# 
# for (x in 1:80){
#   print(paste(x,class(data[,x])))
#   
# }
# data$PoolQC
summary(data1)

summary(model.full)

plot(model.full)

library(car)
influencePlot(model.full)

vif(model.full)

avPlots
model.empty = lm(SalePrice ~ 1, data = data1)
scope = list(lower = formula(model.empty), upper = formula(model.full))

forwardAIC = step(model.empty, scope, direction = "forward", k = 2)
backwardAIC = step(model.full, scope, direction = "backward", k = 2)
bothAIC.empty = step(model.empty, scope, direction = "both", k = 2)
bothAIC.full = step(model.full, scope, direction = "both", k = 2)

summary(forwardAIC)
plot(forwardAIC)
influencePlot(forwardAIC)
vif(forwardAIC)
avPlots(forwardAIC)
confint(forwardAIC)

summary(backwardAIC)

AIC(model.full,    #Model with all variables.
    forwardAIC,        #Model with all variables EXCEPT Illiteracy.
    backwardAIC,
    bothAIC.empty,
    bothAIC.full)

summary(bothAIC.full)

bothAIC.full$fitted.values
modelout = colnames(bothAIC.full$model)
modelout = modelout[-1]
test = test %>% select(.,modelout)
sum(is.na(test1[,1]))

# predict.lm(forwardAIC,  type="response", se.fit=FALSE,interval = "prediction",newdata = test)
predict(bothAIC.full, test,interval = "prediction")

# SalePrice ~ MSZoning + LotArea + Street + LandContour + 
#   LotConfig + LandSlope + Neighborhood + Condition1 + Condition2 + 
#   BldgType + OverallQual + OverallCond + YearBuilt + YearRemodAdd + 
#   RoofStyle + RoofMatl + Exterior1st + ExterCond + Foundation + 
#   BsmtQual + BsmtCond + BsmtExposure + BsmtFinSF1 + BsmtFinSF2 + 
#   BsmtUnfSF + Heating + HeatingQC + CentralAir + X1stFlrSF + 
#   X2ndFlrSF + LowQualFinSF + BsmtFullBath + FullBath + HalfBath + 
#   KitchenAbvGr + KitchenQual + TotRmsAbvGrd + Functional + 
#   Fireplaces + GarageCars + GarageArea + GarageQual + GarageCond + 
#   WoodDeckSF + EnclosedPorch + X3SsnPorch + ScreenPorch + PoolArea + 
#   PoolQC + SaleType + SaleCondition

# SalePrice ~ OverallQual + Neighborhood + GrLivArea + 
#   BsmtFinType1 + GarageCars + OverallCond + RoofMatl + TotalBsmtSF + 
#   YearBuilt + Condition2 + MSZoning + BsmtUnfSF + SaleCondition + 
#   Functional + BldgType + CentralAir + LotArea + KitchenQual + 
#   ScreenPorch + Condition1 + Fireplaces + Heating + BsmtExposure + 
#   Exterior1st + YearRemodAdd + LandSlope + GarageArea + WoodDeckSF + 
#   LotConfig + Foundation + HeatingQC + PoolQC + EnclosedPorch + 
#   SaleType + BsmtFullBath + PoolArea + BsmtQual + GarageCond + 
#   HalfBath + X3SsnPorch + Street + FullBath + KitchenAbvGr + 
#   GarageQual + ExterCond + TotRmsAbvGr
# 
# SalePrice ~ OverallQual + Neighborhood + GrLivArea + GarageCars + 
#   OverallCond + RoofMatl + TotalBsmtSF + YearBuilt + Condition2 + 
#   MSZoning + BsmtUnfSF + SaleCondition + Functional + BldgType + 
#   CentralAir + LotArea + KitchenQual + ScreenPorch + Condition1 + 
#   Fireplaces + Heating + BsmtExposure + Exterior1st + YearRemodAdd + 
#   LandSlope + GarageArea + WoodDeckSF + LotConfig + Foundation + 
#   HeatingQC + PoolQC + EnclosedPorch + SaleType + BsmtFullBath + 
#   PoolArea + X3SsnPorch + BsmtFinSF1 + HalfBath + FullBath + 
#   GarageCond + KitchenAbvGr + Street + ExterCond + TotRmsAbvGrd + 
#   GarageQual + BsmtQual
# 
# SalePrice ~ MSZoning + LotArea + Street + LandContour + LotConfig + 
#   LandSlope + Neighborhood + Condition1 + Condition2 + BldgType + 
#   OverallQual + OverallCond + YearBuilt + YearRemodAdd + RoofStyle + 
#   RoofMatl + Exterior1st + ExterCond + Foundation + BsmtQual + 
#   BsmtCond + BsmtExposure + BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF + 
#   Heating + HeatingQC + CentralAir + X1stFlrSF + X2ndFlrSF + 
#   LowQualFinSF + BsmtFullBath + FullBath + HalfBath + KitchenAbvGr + 
#   KitchenQual + TotRmsAbvGrd + Functional + Fireplaces + GarageCars + 
#   GarageArea + GarageQual + GarageCond + WoodDeckSF + EnclosedPorch + 
#   X3SsnPorch + ScreenPorch + PoolArea + PoolQC + SaleType + 
#   SaleCondition
