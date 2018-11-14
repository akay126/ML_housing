intdata = read.csv('data/my_train.csv', stringsAsFactors =  TRUE)
intdata = subset(intdata,select = -c(Id,MSZoning,Street,Alley,LotShape,LandContour,Utilities,LotConfig,LandSlope,Neighborhood,Condition1,Condition2,BldgType,HouseStyle,RoofStyle,RoofMatl,Exterior1st,Exterior2nd,MasVnrType,ExterQual,ExterCond,Foundation,BsmtQual,BsmtCond,BsmtExposure,BsmtFinType1,BsmtFinType2,Heating,HeatingQC,CentralAir,Electrical,BsmtFullBath,BsmtHalfBath,FullBath,HalfBath,KitchenAbvGr,KitchenQual,Functional,Fireplaces,FireplaceQu,GarageType,GarageFinish,GarageQual,GarageCond,PavedDrive,PoolQC,Fence,MiscFeature,SaleType,SaleCondition))

intdata = select_if(intdata, is.numeric)

intdata$GrLivArea = log10(intdata$GrLivArea)
intdata = subset(intdata, select = -c(FirstFlrSF,SecondFlrSF))

intdata$TotalBsmtSF = log10(intdata$TotalBsmtSF)
intdata$TotalBsmtSF[which(intdata$TotalBsmtSF == -Inf)] = 0
intdata$BsmtFinSF1 = log10(intdata$BsmtFinSF1)
intdata$BsmtFinSF1[which(intdata$BsmtFinSF1 == -Inf)] = 0
intdata$BsmtFinSF2 = log10(intdata$BsmtFinSF2)
intdata$BsmtFinSF2[which(intdata$BsmtFinSF2 == -Inf)] = 0

hist(intdata$TotalBsmtSF)
# 808,1291,249,631,1299,524,1424,314,250,336,707,441,59,198,31,633,1183,1171,1387,811,452
rm_data =c(1299,524,336,314,633,3,250,1424,1183,707,899,496,452,31,1329)
intdata = intdata[- rm_data,]
 

set.seed(0)
intdata_sample <- intdata[sample(1:nrow(intdata), nrow(intdata)*0.20, replace=FALSE),]

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
a = alias(fwdAIC_I)

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
intdata_nb = subset(intdata_nb,select = 
                      -c(MSZoning,Street,Alley,LotShape,LandContour,Utilities,LotConfig,LandSlope,Neighborhood,Condition1,Condition2,BldgType,HouseStyle,RoofStyle,RoofMatl,Exterior1st,Exterior2nd,MasVnrType,ExterQual,ExterCond,Foundation,BsmtQual,BsmtCond,BsmtExposure,BsmtFinType1,BsmtFinType2,Heating,HeatingQC,CentralAir,Electrical,BsmtFullBath,BsmtHalfBath,FullBath,HalfBath,KitchenAbvGr,KitchenQual,Functional,Fireplaces,FireplaceQu,GarageType,GarageFinish,GarageQual,GarageCond,PavedDrive,PoolQC,Fence,MiscFeature,SaleType,SaleCondition))

intdata_nb = select_if(intdata_nb, is.numeric)

intdata_nb$GrLivArea = log10(intdata_nb$GrLivArea)
intdata_nb = subset(intdata_nb, select = -c(FirstFlrSF,SecondFlrSF))

intdata_nb$TotalBsmtSF = log10(intdata_nb$TotalBsmtSF)
intdata_nb$TotalBsmtSF[which(intdata_nb$TotalBsmtSF == -Inf)] = 0
intdata_nb$BsmtFinSF1 = log10(intdata_nb$BsmtFinSF1)
intdata_nb$BsmtFinSF1[which(intdata_nb$BsmtFinSF1 == -Inf)] = 0
intdata_nb$BsmtFinSF2 = log10(intdata_nb$BsmtFinSF2)
intdata_nb$BsmtFinSF2[which(intdata_nb$BsmtFinSF2 == -Inf)] = 0

hist(intdata_nb$BsmtFinSF2)

buck_nb = read.csv('./data/bucketing_houses.csv',header = T)
buck_nb = subset(buck_nb,select = -c(predicted.bucket)) 
intdata_nb = intdata_nb[-rm_data,]
buck_nb = buck_nb[-rm_data,]

# buck_nb = buck_nb %>% transmute(., Id, I1 = I1 , I2= I2+I3 , I3= I4+I5)

intdata_nb = subset(intdata_nb, select= c(modelout_I,'Id','SalePrice'))
var_all = paste0(modelout_I,collapse = '+')
intdata_nb = left_join(x = intdata_nb,y = buck_nb, by ='Id' )



cross_val = function(i){
  intdata_sample <- intdata_nb[sample(1:nrow(intdata_nb), nrow(intdata_nb)*0.20, replace=FALSE),]
  intdata_nb = intdata_nb[-intdata_sample$Id,]
  intdata_sample =  subset(intdata_sample, select=-c(Id))
  
  
  model_nb.full = lm (SalePrice ~ 
                        pB1*(1+OverallQual+GrLivArea+YearBuilt+BsmtFinSF1+LotArea+OverallCond+GarageArea+MSSubClass+BedroomAbvGr+YearRemodAdd+TotalBsmtSF+ScreenPorch+WoodDeckSF+GarageCars+BsmtUnfSF+YrSold+EnclosedPorch+MasVnrArea+OpenPorchSF+TotRmsAbvGrd+LotFrontage)+
                        (1+OverallQual+GrLivArea+YearBuilt+BsmtFinSF1+LotArea+OverallCond+GarageArea+MSSubClass+BedroomAbvGr+YearRemodAdd+TotalBsmtSF+ScreenPorch+WoodDeckSF+GarageCars+BsmtUnfSF+YrSold+EnclosedPorch+MasVnrArea+OpenPorchSF+TotRmsAbvGrd+LotFrontage)+
                        pB3*(1+OverallQual+GrLivArea+YearBuilt+BsmtFinSF1+LotArea+OverallCond+GarageArea+MSSubClass+BedroomAbvGr+YearRemodAdd+TotalBsmtSF+ScreenPorch+WoodDeckSF+GarageCars+BsmtUnfSF+YrSold+EnclosedPorch+MasVnrArea+OpenPorchSF+TotRmsAbvGrd+LotFrontage)+
                        pB4*(1+OverallQual+GrLivArea+YearBuilt+BsmtFinSF1+LotArea+OverallCond+GarageArea+MSSubClass+BedroomAbvGr+YearRemodAdd+TotalBsmtSF+ScreenPorch+WoodDeckSF+GarageCars+BsmtUnfSF+YrSold+EnclosedPorch+MasVnrArea+OpenPorchSF+TotRmsAbvGrd+LotFrontage)+
                        pB5*(1+OverallQual+GrLivArea+YearBuilt+BsmtFinSF1+LotArea+OverallCond+GarageArea+MSSubClass+BedroomAbvGr+YearRemodAdd+TotalBsmtSF+ScreenPorch+WoodDeckSF+GarageCars+BsmtUnfSF+YrSold+EnclosedPorch+MasVnrArea+OpenPorchSF+TotRmsAbvGrd+LotFrontage)-1
                      ,data = intdata_nb)
  test_nb = intdata_sample
  modelout_nb = colnames(model_nb.full$model)
  modelout_nb = modelout_nb[-1]
  test_nb = test_nb %>% select(.,modelout_nb)
  yhat_nb = predict(model_nb.full, test_nb,interval = "prediction")
  y_nb = intdata_sample['SalePrice']
  
  return(sqrt(mean(((yhat_nb-y_nb)**2)[,1],na.rm = T)))
  
}
summary(model_nb.full)
total = c() 
for (i in 1:1000){
  total = c(total, cross_val(i))
}

mean(total)
hist(total)
summary(total)

