data = read.csv('data/my_train.csv', stringsAsFactors =  TRUE)
test = read.csv('data/my_test.csv' , stringsAsFactors = TRUE)

summary(test)

test = test[-1]
data = data[-1]
summary(data)
test = test %>% drop_na()

model.full = lm (SalePrice ~ .,data = data)


summary(model.full)

plot(model.full)

library(car)
influencePlot(model.full)

vif(model.full)

model.empty = lm(SalePrice ~ 1, data = data)
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
alias(forwardAIC)

summary(backwardAIC)

AIC(model.full,    #Model with all variables.
    forwardAIC,        #Model with all variables EXCEPT Illiteracy.
    backwardAIC,
    bothAIC.empty,
    bothAIC.full)

summary(forwardAIC)

forwardAIC$fitted.values
modelout = colnames(forwardAIC$model)
modelout = modelout[-1]
test = test %>% select(.,modelout)
sum(is.na(test1[,1]))

# predict.lm(forwardAIC,  type="response", se.fit=FALSE,interval = "prediction",newdata = test)
predict(forwardAIC, test,interval = "prediction")


###################################### Data Cleaning ####################################


mydata = read.csv('data/my_train.csv', stringsAsFactors =  TRUE)

# mydata$TotalLivSF = mydata$GrLivArea + mydata$TotalBsmtSF
# mydata$TotalLivSF = log10(mydata$TotalLivSF)
mydata = subset(mydata, select = -c(FirstFlrSF,SecondFlrSF,PoolArea))
# mydata$BsmtAll = c(mydata$BsmtFinType1, mydata$BsmtFinType2)


# mydata$GrLivArea = log10(mydata$GrLivArea)
# mydata$TotalBsmtSF = log10(mydata$TotalBsmtSF)
# mydata$TotalBsmtSF[which(mydata$TotalBsmtSF == -Inf)] = 0
mydata$BsmtFinSF1 = log10(mydata$BsmtFinSF1)
mydata$BsmtFinSF1[which(mydata$BsmtFinSF1 == -Inf)] = 0
mydata$BsmtFinSF2 = log10(mydata$BsmtFinSF2)
mydata$BsmtFinSF2[which(mydata$BsmtFinSF2 == -Inf)] = 0
summary(mydata$MiscFeature)


rm_ = c(121,251,524,633,826,463,969,971,282, 403, 632, 635, 677, 794)
mydata = mydata[-rm_,]

set.seed(2)
mysample <- mydata[sample(1:nrow(mydata), nrow(mydata)*0.30, replace=FALSE),]
mydata = mydata[-mysample$Id,]
mydata = subset(mydata, select = -c(Id))


model_S.full = lm (SalePrice ~ .,data = mydata)
model_S.empty = lm(SalePrice ~ 1, data = mydata)
scope_S = list(lower = formula(model_S.empty), upper = formula(model_S.full))
fwdAIC_S = step(model_S.empty, scope_S, direction = "forward", k = 2)

summary(fwdAIC_S)
plot(fwdAIC_S)
influencePlot(fwdAIC_S)
vif(fwdAIC_S)
avPlots(fwdAIC_S)
confint(fwdAIC_S)
alias(fwdAIC_S)

test_S = select(mysample,-SalePrice)
modelout_S = colnames(fwdAIC_S$model)
modelout_S = modelout_S[-1]
test_S = test_S %>% select(.,modelout_S)
yhat_S = predict(fwdAIC_S, test_S,interval = "prediction")
y = mysample['SalePrice']

sqrt(mean(((yhat_S-y)**2)[,1],na.rm = T))

# total1 = c() 
# for (i in 1:1000){
#   total1 = c(total1, cross_var1(i))
# }



############### Bucketing #######################


se <- sqrt(sum(fwdAIC_S$residuals^2) / fwdAIC_S$df.residual)  ## Pearson residual standard error
hii <- lm.influence(fwdAIC_S, do.coef = FALSE)$hat  ## leverage
std.resi <- fwdAIC_S$residuals / (se * sqrt(1 - hii))  ## standardized residuals
## these three lines can be replaced by: std.resi <- rstandard(fwdAIC_S)
std.resi = std.resi[which(std.resi < 100 & std.resi > -100)]
summary(std.resi)

par(mfrow = c(1,2))
qqnorm(std.resi, main = "my Q-Q" ); qqline(std.resi, lty = 2)
plot(fwdAIC_S, which = 2)  ## only display Q-Q plot
hist(std.resi)

buck1 = which(std.resi < quantile(std.resi)[2])
buck2 = which(std.resi>= quantile(std.resi)[2] & std.resi <= quantile(std.resi)[4])
buck3 = which(std.resi > quantile(std.resi)[4])

buck1 = which(std.resi < -1)
buck2 = which(std.resi >= -1 & std.resi <= 1)
buck3 = which(std.resi > 1)


#################### Using Naiive Bayves  ################
intdata = read.csv('data/my_train.csv', stringsAsFactors =  TRUE)
intdata = subset(intdata,select = 
                      -c(MSZoning,Street,Alley,LotShape,LandContour,Utilities,LotConfig,LandSlope,Neighborhood,Condition1,Condition2,BldgType,HouseStyle,RoofStyle,RoofMatl,Exterior1st,Exterior2nd,MasVnrType,ExterQual,ExterCond,Foundation,BsmtQual,BsmtCond,BsmtExposure,BsmtFinType1,BsmtFinType2,Heating,HeatingQC,CentralAir,Electrical,BsmtFullBath,BsmtHalfBath,FullBath,HalfBath,KitchenAbvGr,KitchenQual,Functional,Fireplaces,FireplaceQu,GarageType,GarageFinish,GarageQual,GarageCond,PavedDrive,PoolQC,Fence,MiscFeature))

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

rm_data =c(808,1291,249,631,1299,524,1424,314,250,336,707,441,59,198,31,633,1183,1171,1387,811)
intdata = intdata[- rm_data,]

set.seed(0)
intdata_sample <- intdata[sample(1:nrow(intdata), nrow(intdata)*0.20, replace=FALSE),]
intdata_sample =  subset(intdata_sample, select=-c(Id,B1,B2,B3,B4,B5))

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

############################# NB based on price ##################################

intdata_nb = read.csv('data/my_train.csv', stringsAsFactors =  TRUE)
intdata_nb = subset(intdata_nb,select = 
                   -c(MSZoning,Street,Alley,LotShape,LandContour,Utilities,LotConfig,LandSlope,Neighborhood,Condition1,Condition2,BldgType,HouseStyle,RoofStyle,RoofMatl,Exterior1st,Exterior2nd,MasVnrType,ExterQual,ExterCond,Foundation,BsmtQual,BsmtCond,BsmtExposure,BsmtFinType1,BsmtFinType2,Heating,HeatingQC,CentralAir,Electrical,BsmtFullBath,BsmtHalfBath,FullBath,HalfBath,KitchenAbvGr,KitchenQual,Functional,Fireplaces,FireplaceQu,GarageType,GarageFinish,GarageQual,GarageCond,PavedDrive,PoolQC,Fence,MiscFeature))

intdata_nb = select_if(intdata_nb, is.numeric)

intdata_nb$GrLivArea = log10(intdata_nb$GrLivArea)
intdata_nb = subset(intdata_nb, select = -c(FirstFlrSF,SecondFlrSF))

intdata_nb$TotalBsmtSF = log10(intdata_nb$TotalBsmtSF)
intdata_nb$TotalBsmtSF[which(intdata_nb$TotalBsmtSF == -Inf)] = 0
intdata_nb$BsmtFinSF1 = log10(intdata_nb$BsmtFinSF1)
intdata_nb$BsmtFinSF1[which(intdata_nb$BsmtFinSF1 == -Inf)] = 0
intdata_nb$BsmtFinSF2 = log10(intdata_nb$BsmtFinSF2)
intdata_nb$BsmtFinSF2[which(intdata_nb$BsmtFinSF2 == -Inf)] = 0

hist(intdata_nb$BsmtFinSF1)

buck_nb = read.csv('./data/bucketing_houses_price.csv',header = T)
buck_nb = subset(buck_nb,select = -c(B1,B2,B3,B4,B5)) 
intdata_nb = intdata_nb[-rm_data,]
buck_nb = buck_nb[-rm_data,]

buck_nb = buck_nb %>% transmute(., Id, I1 = I1 , I2= I2+I3 , I3= I4+I5)

# a = samlple(1:99, 1460, replace = TRUE)/100
# b = sample(1:99, 1460, replace = TRUE)/100
# c = sample(1:99, 1460, replace = TRUE)/100
# d = sample(1:99, 1460, replace = TRUE)/100
# e = sample(1:99, 1460, replace = TRUE)/100
# buck = data.frame(Id = 1:1460,B1 = a,B2= b,B3= c,B4= d, B5 = e)
intdata_nb = subset(intdata_nb, select= c(modelout_I,'Id','SalePrice'))
var_all = paste0(modelout_I,collapse = '+')
intdata_nb = left_join(x = intdata_nb,y = buck_nb, by ='Id' )

cross_val = function(i){
  intdata_sample <- intdata_nb[sample(1:nrow(intdata_nb), nrow(intdata_nb)*0.20, replace=FALSE),]
  intdata_nb = intdata_nb[-intdata_sample$Id,]
  intdata_sample =  subset(intdata_sample, select=-c(Id))
  
  
  model_nb.full = lm (SalePrice ~ 
                        I1*(1+OverallQual+GrLivArea+YearBuilt+BsmtFinSF1+LotArea+OverallCond+GarageArea+MSSubClass+BedroomAbvGr+TotalBsmtSF+YearRemodAdd+ScreenPorch+WoodDeckSF+GarageCars+MasVnrArea+YrSold+EnclosedPorch+BsmtUnfSF+TotRmsAbvGrd)+
                        I2*(1+OverallQual+GrLivArea+YearBuilt+BsmtFinSF1+LotArea+OverallCond+GarageArea+MSSubClass+BedroomAbvGr+TotalBsmtSF+YearRemodAdd+ScreenPorch+WoodDeckSF+GarageCars+MasVnrArea+YrSold+EnclosedPorch+BsmtUnfSF+TotRmsAbvGrd)+
                        (1+OverallQual+GrLivArea+YearBuilt+BsmtFinSF1+LotArea+OverallCond+GarageArea+MSSubClass+BedroomAbvGr+TotalBsmtSF+YearRemodAdd+ScreenPorch+WoodDeckSF+GarageCars+MasVnrArea+YrSold+EnclosedPorch+BsmtUnfSF+TotRmsAbvGrd)-1
                        # I4*(1+OverallQual+GrLivArea+YearBuilt+BsmtFinSF1+OverallCond+TotalBsmtSF+LotArea+GarageCars+MSSubClass+YearRemodAdd+ScreenPorch+BedroomAbvGr+WoodDeckSF+EnclosedPorch+YrSold+LowQualFinSF+GarageArea+TotRmsAbvGrd+BsmtFinSF2)+
                        # (1+OverallQual+GrLivArea+YearBuilt+BsmtFinSF1+OverallCond+TotalBsmtSF+LotArea+GarageCars+MSSubClass+YearRemodAdd+ScreenPorch+BedroomAbvGr+WoodDeckSF+EnclosedPorch+YrSold+LowQualFinSF+GarageArea+TotRmsAbvGrd+BsmtFinSF2)-1
                      ,data = intdata_nb)
  test_nb = intdata_sample
  modelout_nb = colnames(model_nb.full$model)
  modelout_nb = modelout_nb[-1]
  test_nb = test_nb %>% select(.,modelout_nb)
  yhat_nb = predict(model_nb.full, test_nb,interval = "prediction")
  y_nb = intdata_sample['SalePrice']
  
  return(sqrt(mean(((yhat_nb-y_nb)**2)[,1],na.rm = T)))
  
}

total = c() 
for (i in 1:1000){
  total = c(total, cross_val(i))
}

mean(total)
hist(total)
summary(total)
################################################################
set.seed(0)
intdata_sample <- test
  = read.csv('./data/bucketing_houses_price.csv',header = T)
buck_nb = subset(buck_nb,select = -c(B1,B2,B3,B4,B5)) 
intdata_nb = intdata_nb[-rm_data,]
buck_nb = buck_nb[-rm_data,]

buck_nb = buck_nb %>% transmute(., Id, I1 = I1 , I2= I2+I3 , I3= I4+I5)

model_nb.full = lm (SalePrice ~ 
                     I1*(1+OverallQual+GrLivArea+YearBuilt+BsmtFinSF1+OverallCond+TotalBsmtSF+LotArea+GarageCars+MSSubClass+YearRemodAdd+ScreenPorch+BedroomAbvGr+WoodDeckSF+EnclosedPorch+YrSold+LowQualFinSF+GarageArea+TotRmsAbvGrd+BsmtFinSF2)+
                     I2*(1+OverallQual+GrLivArea+YearBuilt+BsmtFinSF1+OverallCond+TotalBsmtSF+LotArea+GarageCars+MSSubClass+YearRemodAdd+ScreenPorch+BedroomAbvGr+WoodDeckSF+EnclosedPorch+YrSold+LowQualFinSF+GarageArea+TotRmsAbvGrd+BsmtFinSF2)+
                     (1+OverallQual+GrLivArea+YearBuilt+BsmtFinSF1+OverallCond+TotalBsmtSF+LotArea+GarageCars+MSSubClass+YearRemodAdd+ScreenPorch+BedroomAbvGr+WoodDeckSF+EnclosedPorch+YrSold+LowQualFinSF+GarageArea+TotRmsAbvGrd+BsmtFinSF2)-1
                     # I4*(1+OverallQual+GrLivArea+YearBuilt+BsmtFinSF1+OverallCond+TotalBsmtSF+LotArea+GarageCars+MSSubClass+YearRemodAdd+ScreenPorch+BedroomAbvGr+WoodDeckSF+EnclosedPorch+YrSold+LowQualFinSF+GarageArea+TotRmsAbvGrd+BsmtFinSF2)+
                     # (1+OverallQual+GrLivArea+YearBuilt+BsmtFinSF1+OverallCond+TotalBsmtSF+LotArea+GarageCars+MSSubClass+YearRemodAdd+ScreenPorch+BedroomAbvGr+WoodDeckSF+EnclosedPorch+YrSold+LowQualFinSF+GarageArea+TotRmsAbvGrd+BsmtFinSF2)-1
                     ,data = intdata_nb)

# model_nb.empty = lm(SalePrice ~ 1, data = intdata_nb)
# scope_nb = list(lower = formula(model_nb.empty), upper = formula(model_nb.full))
# fwdAIC_nb = step(model_nb.empty, scope_nb, direction = "forward", k = 2)
# bkAIC_nb = step(model_nb.full, scope_nb, direction = "backward", k = 2)
# bothAIC.empty_nb = step(model_nb.empty, scope_nb, direction = "both", k = 2)
# bothAIC.full_nb = step(model_nb.full, scope_nb, direction = "both", k = 2)

# summary(model_nb.full)
# AIC(model_nb.full,
#     fwdAIC_nb,
#     bkAIC_nb
#     )

test_nb = intdata_sample
modelout_nb = colnames(model_nb.full$model)
modelout_nb = modelout_nb[-1]
test_nb = test_nb %>% select(.,modelout_nb)
yhat_nb = predict(model_nb.full, test_nb,interval = "prediction")
y_nb = intdata_sample['SalePrice']

sqrt(mean(((yhat_nb-y_nb)**2)[,1],na.rm = T))

summary(model_nb.full)
AIC(model_nb.full)
plot(model_nb.full)
influencePlot(model_nb.full)
vif(model_nb.full)
avPlots(model_nb.full)
confint(model_nb.full)
vif(model_nb.full)
alias(model_nb.full)

test = read.csv('data/my_test.csv' , stringsAsFactors = TRUE)
submission =  read.csv('./data/submission.csv',stringsAsFactors = FALSE)

