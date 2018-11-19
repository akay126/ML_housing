library(caret)
library(dplyr)

data = read.csv('data/my_train.csv', stringsAsFactors =  TRUE)
test = read.csv('data/my_test_imputed.csv', stringsAsFactors =  TRUE)
data = clean_data(data)
test = clean_data(test)
intdata = data.frame(scale(data))
inttest =  data.frame(scale(test))
intdata$Id = as.numeric(row.names(intdata))
inttest$Id = 1461:2919
intdata$SalePrice = data$SalePrice

buck_nb = read.csv('./data/bucketing_houses.csv',header = T)
buck_test = read.csv('./data/bucketing_test_houses.csv',header = T)

buck_nb =  buck_nb %>% select(., c(1:6))
buck_test = buck_test %>% select(., c(1:6))

intdata = left_join(intdata,buck_nb,by= "Id") ; intdata = subset(intdata, select = -Id)
inttest = left_join(inttest,buck_test,by= "Id") ; inttest = subset(inttest, select = -Id)

x = model.matrix(SalePrice ~ ., intdata) [,-1]
y = data$SalePrice

set.seed(0)
train = sample(1:nrow(x), 7*nrow(x)/10)
test = (-train)
y.test = y[test]

cont_enet = trainControl(method = "cv",number = 10)
tune_enet = expand.grid(alpha = seq(.95,1.05,.01),lambda = seq(0.005,0.03,0.005))

train_enet = train(x[train,],y[train],
                   trControl = cont_enet,
                   # tuneGrid = tune_enet,
                   tuneLength = 300,
                   method = "glmnet")

enet_predict = predict(train_enet,newdata = x[test,])

MSE = mean((enet_predict - y.test)^2)
sqrt(MSE)

 ridge.models.train = glmnet(x[train, ], y[train], alpha = 0, lambda = grid)
ridge.lambda5 = predict(ridge.models.train, s = 5, newx = x[test, ])
mean((ridge.lambda5 - y.test)^2)





cont_enet = trainControl(method = "cv",number = 10)
tune_enet = expand.grid(alpha = seq(.75,0.85,.01),lambda = seq(.025,0.05,.0001))

set.seed(0)

train_enet = train(SalePrice ~ .,
                   data = intdata,
                   trControl = cont_enet,
                   # tuneGrid = tune_enet,
                   tuneLength = 10000,
                   method = "glmnet")

pred_enet = predict(train_enet,newdata = inttest)
home_alone = data.frame(Id= 1461:2919, SalePrice = exp(pred_enet))
write.csv(home_alone, './data/homealone.csv',row.names = FALSE)


train_enet$bestTune$alpha
submission = read.csv('./data/submission.csv',header = T)

sqrt(mean((pred_enet - log(submission[,2]))^2))


train_enet$results$alpha[which(train_enet$results$alpha == 0.9578595),]


set.seed(0)
train_control = trainControl(method = 'cv', number=10)
tune.grid = expand.grid(lambda = grid, alpha=c(0))
ridge.caret = train(x[train, ], y[train],
                    method = 'glmnet',
                    trControl = train_control, tuneGrid = tune.grid)

### Plot the tuning object:
plot(ridge.caret, xTrans=log)





clean_data = function(intdata){
  intdata = subset(intdata,select = -c(MSZoning,Street,Alley,LotShape,LandContour,Utilities,LotConfig,LandSlope,Neighborhood,Condition1,Condition2,BldgType,HouseStyle,RoofStyle,RoofMatl,Exterior1st,Exterior2nd,MasVnrType,ExterQual,ExterCond,Foundation,BsmtQual,BsmtCond,BsmtExposure,BsmtFinType1,BsmtFinType2,Heating,HeatingQC,CentralAir,Electrical,BsmtFullBath,BsmtHalfBath,FullBath,HalfBath,KitchenAbvGr,KitchenQual,Functional,Fireplaces,FireplaceQu,GarageType,GarageFinish,GarageQual,GarageCond,PavedDrive,PoolQC,Fence,MiscFeature,SaleType,SaleCondition))
  intdata = select_if(intdata, is.numeric)
  
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

  return(intdata)
}

