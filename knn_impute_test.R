setwd('D:/Code/ML_housing')
library(Hmisc)
library(deldir)
library(kknn)
library(caret)
library(VIM)
library(Hmisc) 
library(deldir)
library(kknn)

test = read.csv('./data/my_test.csv', header = T)
data = read.csv('./data/my_train.csv', header = T)
buck_nb = read.csv('./data/bucketing_houses.csv',header = T)
buck_nb = subset(buck_nb,select = -c(predicted.bucket)) 

test_na = test[!complete.cases(test), ]
test_na$Id
test = kNN(test, k=1)[1:80]

test = subset(test,select = 
                      -c(MSZoning,Street,Alley,LotShape,LandContour,Utilities,LotConfig,LandSlope,Neighborhood,Condition1,Condition2,BldgType,HouseStyle,RoofStyle,RoofMatl,Exterior1st,Exterior2nd,MasVnrType,ExterQual,ExterCond,Foundation,BsmtQual,BsmtCond,BsmtExposure,BsmtFinType1,BsmtFinType2,Heating,HeatingQC,CentralAir,Electrical,BsmtFullBath,BsmtHalfBath,FullBath,HalfBath,KitchenAbvGr,KitchenQual,Functional,Fireplaces,FireplaceQu,GarageType,GarageFinish,GarageQual,GarageCond,PavedDrive,PoolQC,Fence,MiscFeature,SaleType,SaleCondition))
data = subset(data,select = 
                -c(SalePrice,MSZoning,Street,Alley,LotShape,LandContour,Utilities,LotConfig,LandSlope,Neighborhood,Condition1,Condition2,BldgType,HouseStyle,RoofStyle,RoofMatl,Exterior1st,Exterior2nd,MasVnrType,ExterQual,ExterCond,Foundation,BsmtQual,BsmtCond,BsmtExposure,BsmtFinType1,BsmtFinType2,Heating,HeatingQC,CentralAir,Electrical,BsmtFullBath,BsmtHalfBath,FullBath,HalfBath,KitchenAbvGr,KitchenQual,Functional,Fireplaces,FireplaceQu,GarageType,GarageFinish,GarageQual,GarageCond,PavedDrive,PoolQC,Fence,MiscFeature,SaleType,SaleCondition))


test = test[-1]
test$Id = NA
data = data[-81]
test = test[,c(80,1:79)]
colnames(data)

all = dplyr::bind_rows(data,test)

all = kNN(all, k=1)
clean_test = all[1461:nrow(all),(1:31)]

model_list = model_map[,c(1,4)]
clean_test = left_join(clean_test,model_list, by = "Id")
clean_test = subset(clean_test,select = 
                      -c(MSZoning,Street,Alley,LotShape,LandContour,Utilities,LotConfig,LandSlope,Neighborhood,Condition1,Condition2,BldgType,HouseStyle,RoofStyle,RoofMatl,Exterior1st,Exterior2nd,MasVnrType,ExterQual,ExterCond,Foundation,BsmtQual,BsmtCond,BsmtExposure,BsmtFinType1,BsmtFinType2,Heating,HeatingQC,CentralAir,Electrical,BsmtFullBath,BsmtHalfBath,FullBath,HalfBath,KitchenAbvGr,KitchenQual,Functional,Fireplaces,FireplaceQu,GarageType,GarageFinish,GarageQual,GarageCond,PavedDrive,PoolQC,Fence,MiscFeature,SaleType,SaleCondition))

clean_test = select_if(clean_test, is.numeric)

clean_test$GrLivArea = log10(clean_test$GrLivArea)
clean_test = subset(clean_test, select = -c(FirstFlrSF,SecondFlrSF))

clean_test$TotalBsmtSF = log10(clean_test$TotalBsmtSF)
clean_test$TotalBsmtSF[which(clean_test$TotalBsmtSF == -Inf)] = 0
clean_test$YearBuilt = clean_test$YearBuilt**2

clean_test = left_join(clean_test,buck_nb, by = "Id")



clean_test[-1]

##############need to join nb with
result = data.frame(Id = (1461:2919), SalePrice = NA)

for (i in 1:1459){
  result[i,2] = exp(predict_hosuing(clean_test$model[i],subset(clean_test[i,],select= -c(Id,model))))
  # result[i,2] = predict_hosuing(model_list$model[i],subset(intdata_nb[i,],select= -c(Id,SalePrice)))
}
i=1
result_na = result[!complete.cases(result), ]
summary(result$SalePrice)
a = (log(submission$SalePrice) - log(result$SalePrice))
summary(a)

b = a[which((a**2)<0.099 & (a**2) > -0.099)]
sqrt(mean(abs(b)))


result_out = result[which(result$SalePrice < 1e5 | result$SalePrice > 1e6 ),]

predict_hosuing(clean_test$model[1],subset(clean_test[1,],select= -c(Id,model)))

intdata_nb = read.csv('data/my_train.csv', stringsAsFactors =  TRUE)
intdata_nb = subset(intdata_nb,select = 
                      -c(MSZoning,Street,Alley,LotShape,LandContour,Utilities,LotConfig,LandSlope,Neighborhood,Condition1,Condition2,BldgType,HouseStyle,RoofStyle,RoofMatl,Exterior1st,Exterior2nd,MasVnrType,ExterQual,ExterCond,Foundation,BsmtQual,BsmtCond,BsmtExposure,BsmtFinType1,BsmtFinType2,Heating,HeatingQC,CentralAir,Electrical,BsmtFullBath,BsmtHalfBath,FullBath,HalfBath,KitchenAbvGr,KitchenQual,Functional,Fireplaces,FireplaceQu,GarageType,GarageFinish,GarageQual,GarageCond,PavedDrive,PoolQC,Fence,MiscFeature,SaleType,SaleCondition))

intdata_nb = select_if(intdata_nb, is.numeric)

intdata_nb$GrLivArea = log10(intdata_nb$GrLivArea)
intdata_nb = subset(intdata_nb, select = -c(FirstFlrSF,SecondFlrSF))

intdata_nb$TotalBsmtSF = log10(intdata_nb$TotalBsmtSF)
intdata_nb$TotalBsmtSF[which(intdata_nb$TotalBsmtSF == -Inf)] = 0
intdata_nb$YearBuilt = intdata_nb$YearBuilt**2

buck_nb = read.csv('./data/bucketing_houses.csv',header = T)
buck_nb = subset(buck_nb,select = -c(predicted.bucket)) 
# intdata_nb = intdata_nb[-rm_data,]
# buck_nb = buck_nb[-rm_data,]

# buck_nb = buck_nb %>% transmute(., Id, I1 = I1 , I2= I2+I3 , I3= I4+I5)

intdata_nb = subset(intdata_nb, select= c(modelout_I,'Id','SalePrice'))
var_all = paste0(modelout_I,collapse = '+')

intdata_nb = left_join(x = intdata_nb,y = buck_nb, by ='Id' )



predict_hosuing = function(i,df){
  set.seed(i)
  intdata_sample <- intdata_nb[sample(1:nrow(intdata_nb), nrow(intdata_nb)*0.20, replace=FALSE),]
  intdata_nb = intdata_nb[-intdata_sample$Id,]
  # intdata_sample =  subset(intdata_sample, select=-c(Id))
  intdata_nb =  subset(intdata_nb, select=-c(Id))
  
  model_nb.full = lm (SalePrice ~ 
                        pB1*(1+OverallQual+GrLivArea+YearBuilt+OverallCond+BsmtFinSF1+GarageCars+MSSubClass+LotArea+TotalBsmtSF+ScreenPorch+WoodDeckSF+YearRemodAdd+BedroomAbvGr+BsmtFinSF2+PoolArea+YrSold+EnclosedPorch)+
                            (1+OverallQual+GrLivArea+YearBuilt+OverallCond+BsmtFinSF1+GarageCars+MSSubClass+LotArea+TotalBsmtSF+ScreenPorch+WoodDeckSF+YearRemodAdd+BedroomAbvGr+BsmtFinSF2+PoolArea+YrSold+EnclosedPorch)+
                        pB3*(1+OverallQual+GrLivArea+YearBuilt+OverallCond+BsmtFinSF1+GarageCars+MSSubClass+LotArea+TotalBsmtSF+ScreenPorch+WoodDeckSF+YearRemodAdd+BedroomAbvGr+BsmtFinSF2+PoolArea+YrSold+EnclosedPorch)+
                        pB4*(1+OverallQual+GrLivArea+YearBuilt+OverallCond+BsmtFinSF1+GarageCars+MSSubClass+LotArea+TotalBsmtSF+ScreenPorch+WoodDeckSF+YearRemodAdd+BedroomAbvGr+BsmtFinSF2+PoolArea+YrSold+EnclosedPorch)+
                        pB5*(1+OverallQual+GrLivArea+YearBuilt+OverallCond+BsmtFinSF1+GarageCars+MSSubClass+LotArea+TotalBsmtSF+ScreenPorch+WoodDeckSF+YearRemodAdd+BedroomAbvGr+BsmtFinSF2+PoolArea+YrSold+EnclosedPorch)
                      ,data = intdata_nb)
  modelout_nb = colnames(model_nb.full$model)
  modelout_nb = modelout_nb[-1]
  df = df %>% select(.,modelout_nb)
  yhat_nb = predict(model_nb.full, df)
  
  
  return(yhat_nb)
  
}


var_ = paste0(modelout_nb,collapse = ',')


