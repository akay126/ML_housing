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

mydata = mydata[-c(823,523),]
mydata = mydata %>% drop_na()
set.seed(0)
mysample <- mydata[sample(1:nrow(mydata), nrow(mydata)*0.20, replace=FALSE),]
mydata = mydata[-mysample$Id,]


model_S.full = lm (SalePrice ~ .,data = mydata)
model_S.empty = lm(SalePrice ~ 1, data = mydata)
scope_S = list(lower = formula(model_S.empty), upper = formula(model_S.full))
fwdAIC_S = step(model_S.empty, scope_S, direction = "forward", k = 2)

summary(fwdAIC_S)
summary(fwdAIC_S)
plot(fwdAIC_S)
influencePlot(fwdAIC_S)
vif(fwdAIC_S)
avPlots(fwdAIC_S)
confint(fwdAIC_S)
vif(fwdAIC_S)
a = alias(fwdAIC_S)

test_S = select(mysample,-SalePrice)
modelout_S = colnames(fwdAIC_S$model)
modelout_S = modelout_S[-1]
test_S = test_S %>% select(.,modelout_S)
yhat_S = predict(fwdAIC_S, test_S,interval = "prediction")
y = mysample['SalePrice']
yhat_S[,1]

sqrt(mean(((yhat_S-y)**2)[,1],na.rm = T))

AIC(fwdAIC_S)


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



