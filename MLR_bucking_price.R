library(car)
library(dplyr)


intdata = read.csv('data/my_train.csv', stringsAsFactors =  TRUE)
intdata = clean_data(intdata)


set.seed(0)
intdata_sample <- intdata[sample(1:nrow(intdata), nrow(intdata)*0.20, replace=FALSE),]
intdata = intdata[-intdata_sample$Id,]
intdata_sample =  subset(intdata_sample, select=-c(Id))
intdata =  subset(intdata, select=-c(Id))

model_I.full = lm (SalePrice ~ .,data = intdata)
model_I.empty = lm(SalePrice ~ 1, data = intdata)
scope_I = list(lower = formula(model_I.empty), upper = formula(model_I.full))
fwdAIC_I = step(model_I.empty, scope_I, direction = "forward", k = 2)
bkAIC_I = step(model_I.full, scope_I, direction = "backward", k = 2)
bothAIC.empty_I = step(model_I.empty, scope_I, direction = "both", k = 2)
bothAIC.full_I = step(model_I.full, scope_I, direction = "both", k = 2)

AIC(fwdAIC_I,
    bkAIC_I,
    bothAIC.empty_I,
    bothAIC.full_I)

summary(fwdAIC_I)
plot(fwdAIC_I)
influencePlot(fwdAIC_I)
vif(fwdAIC_I)
avPlots(fwdAIC_I)
confint(fwdAIC_I)
vif(fwdAIC_I)
alias(fwdAIC_I)

test_I = select(intdata_sample,-SalePrice)
modelout_I = colnames(fwdAIC_I$model)
modelout_I = modelout_I[-1]
test_I = test_I %>% select(.,modelout_I)
yhat_I = predict(fwdAIC_I, test_I,interval = "prediction")
y_I = intdata_sample['SalePrice']
yhat_I[,1]

sqrt(mean(((yhat_I-y_I)**2)[,1],na.rm = T))

AIC(fwdAIC_I)

###########################################
intdata_nb = read.csv('data/my_train.csv', stringsAsFactors =  TRUE)
intdata_nb = clean_data(intdata_nb)

buck_nb = read.csv('./data/bucketing_houses.csv',header = T)
buck_nb = subset(buck_nb,select = -c(predicted.bucket)) 
# intdata_nb = intdata_nb[-rm_data,]
# buck_nb = buck_nb[-rm_data,]

intdata_nb = subset(intdata_nb, select= c(modelout_I,'Id','SalePrice'))
var_all = paste0(modelout_I,collapse = '+')
intdata_nb = left_join(x = intdata_nb,y = buck_nb, by ='Id' )

cross_val = function(i){
  set.seed(i)
  intdata_sample <- intdata_nb[sample(1:nrow(intdata_nb), nrow(intdata_nb)*0.20, replace=FALSE),]
  intdata_nb = intdata_nb[-intdata_sample$Id,]
  intdata_sample =  subset(intdata_sample, select=-c(Id))
  intdata_nb =  subset(intdata_nb, select=-c(Id))
  
  model_nb.full = lm (SalePrice ~ 
                        pB1*(1+OverallQual+GrLivArea+YearBuilt+OverallCond+BsmtFinSF1+GarageCars+MSSubClass+LotArea+TotalBsmtSF+ScreenPorch+WoodDeckSF+PoolArea+YearRemodAdd+BsmtFinSF2+BedroomAbvGr)+
                        pB2*(1+OverallQual+GrLivArea+YearBuilt+OverallCond+BsmtFinSF1+GarageCars+MSSubClass+LotArea+TotalBsmtSF+ScreenPorch+WoodDeckSF+PoolArea+YearRemodAdd+BsmtFinSF2+BedroomAbvGr)+
                            (1+OverallQual+GrLivArea+YearBuilt+OverallCond+BsmtFinSF1+GarageCars+MSSubClass+LotArea+TotalBsmtSF+ScreenPorch+WoodDeckSF+PoolArea+YearRemodAdd+BsmtFinSF2+BedroomAbvGr)+
                        pB4*(1+OverallQual+GrLivArea+YearBuilt+OverallCond+BsmtFinSF1+GarageCars+MSSubClass+LotArea+TotalBsmtSF+ScreenPorch+WoodDeckSF+PoolArea+YearRemodAdd+BsmtFinSF2+BedroomAbvGr)+
                        pB5*(1+OverallQual+GrLivArea+YearBuilt+OverallCond+BsmtFinSF1+GarageCars+MSSubClass+LotArea+TotalBsmtSF+ScreenPorch+WoodDeckSF+PoolArea+YearRemodAdd+BsmtFinSF2+BedroomAbvGr)-1
                            # (1+OverallQual+GrLivArea+YearBuilt+OverallCond+BsmtFinSF1+GarageCars+MSSubClass+LotArea+TotalBsmtSF+ScreenPorch+WoodDeckSF+PoolArea+YearRemodAdd+BsmtFinSF2+BedroomAbvGr)-1
                      ,data = intdata_nb)
  test_nb = intdata_sample
  modelout_nb = colnames(model_nb.full$model)
  modelout_nb = modelout_nb[-1]
  test_nb = test_nb %>% select(.,modelout_nb)
  yhat_nb = predict(model_nb.full, test_nb)
  y_nb = intdata_sample['SalePrice']

  return(sqrt(mean(((yhat_nb-y_nb)**2)[,1])))
  
}
summary(model_nb.full)
total = c() 
for (i in 1:10){
  total = c(total, cross_val(i))
}

summary(total)
max(total)
median(total)
hist(total)


clean_data = function(intdata){
  intdata = subset(intdata,select = -c(MSZoning,Street,Alley,LotShape,LandContour,Utilities,LotConfig,LandSlope,Neighborhood,Condition1,Condition2,BldgType,HouseStyle,RoofStyle,RoofMatl,Exterior1st,Exterior2nd,MasVnrType,ExterQual,ExterCond,Foundation,BsmtQual,BsmtCond,BsmtExposure,BsmtFinType1,BsmtFinType2,Heating,HeatingQC,CentralAir,Electrical,BsmtFullBath,BsmtHalfBath,FullBath,HalfBath,KitchenAbvGr,KitchenQual,Functional,Fireplaces,FireplaceQu,GarageType,GarageFinish,GarageQual,GarageCond,PavedDrive,PoolQC,Fence,MiscFeature,SaleType,SaleCondition))
  intdata = select_if(intdata, is.numeric)
  
  # par(mfrow = c(1,2))
  # qqnorm(intdata$GrLivArea, main = "GrLivArea" ); qqline(intdata$GrLivArea, lty = 2)
  # qqnorm(intdata$TotalBsmtSF, main = "TotalBsmtSF" ); qqline(intdata$TotalBsmtSF, lty = 2)
  # par(mfrow = c(2,1))
  # hist(intdata$GrLivArea)
  # hist(intdata$TotalBsmtSF)
  
  intdata$GrLivArea = log(intdata$GrLivArea)
  intdata$GrLivArea[which(intdata$GrLivArea == -Inf)] = 0
  
  intdata$FirstFlrSF = log(intdata$FirstFlrSF)
  intdata$FirstFlrSF[which(intdata$FirstFlrSF == -Inf)] = 0
  
  intdata$SecondFlrSF = log(intdata$SecondFlrSF)
  intdata$SecondFlrSF[which(intdata$SecondFlrSF == -Inf)] = 0
  
  intdata$LotFrontage = log(intdata$LotFrontage)
  intdata$LotFrontage[which(intdata$LotFrontage == -Inf)] = 0
  
  intdata$TotalBsmtSF = log(intdata$TotalBsmtSF)
  intdata$TotalBsmtSF[which(intdata$TotalBsmtSF == -Inf)] = 0
  
  
  # # 808,1291,249,631,1299,524,1424,314,250,336,707,441,59,198,31,633,1183,1171,1387,811,452
  # rm_data =c(1299,524,336,314,633,3,250,1424,1183,707,899,496,452,31,1329,54)
  # intdata = intdata[- rm_data,]
  
  return(intdata)
}

