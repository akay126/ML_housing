data = read.csv('data/train.csv', stringsAsFactors =  FALSE)
test = read.csv('data/test.csv' , stringsAsFactors = FALSE)

test = test[-1]
data = data[-1]

data$SalePrice = log10(data$SalePrice)
data[is.na(data)] <- 'None'
data1 = data.frame(data,stringsAsFactors = TRUE)
test[is.na(test)] <- 'None'
# class(test[,2])
test <- test %>% mutate_if(is.character,as.factor)
test$MSZoning

class(test[,2])

# data1 = data[c(1:5,7:8,10:71,75:80)]
model.full = lm (SalePrice ~ .,data = data1)
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
modelout = colnames(forwardAIC$model)
modelout = modelout[-1]
test = test[,modelout]
sum(is.na(test1[,1]))

predict(forwardAIC, test)
predict(forwardAIC, test, interval = "prediction")

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
