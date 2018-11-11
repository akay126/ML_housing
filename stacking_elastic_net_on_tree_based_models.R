######## TREE BASED MODEL #######
####### FITTING A RANDOM FOREST TO THE RESIDUALS OF AN ELASTIC NET ######

# LOAD THE DATAFRAME CONTAINING THE RESIDUALS
h.res.data <- fread(file = "./data/lm_pred_v1.csv", stringsAsFactors = TRUE)

# CHECK NORMALITY WITH HISTOGRAM
h.res.data %>% 
  ggplot() +
  geom_histogram(aes(x=residuals), bins = 70, fill ="steel blue")

# SEPARATE TRAINING SET
train = sample(1:nrow(h.res.data), 7*nrow(h.res.data)/10)

h.res.data <- h.res.data %>%
  select(-MSSubClass, -LotFrontage, -LotArea, -OverallQual, -OverallCond, -YearBuilt, -YearRemodAdd, -MasVnrArea,
         -BsmtFinSF1, -BsmtFinSF2, -BsmtUnfSF, -TotalBsmtSF, -FirstFlrSF, -SecondFlrSF, -LowQualFinSF, -GrLivArea,
         -BsmtFullBath, -BsmtHalfBath, -FullBath, -HalfBath, -BedroomAbvGr, -KitchenAbvGr, -TotRmsAbvGrd, -Fireplaces,
         -GarageYrBlt, -GarageCars, -GarageArea, -WoodDeckSF, -OpenPorchSF, -EnclosedPorch, -SsnPorch, -ScreenPorch, -PoolArea,
         -MiscVal, -MoSold, -YrSold)

##################### RANDOM FOREST #################
library(randomForest)
rf.res = randomForest(residuals ~ . -V1 -SalePrice -y_hat -Utilities, data = h.res.data, subset = train, importance = TRUE)
rf.res
#The MSE and percent variance explained are based on out-of-bag estimates,
#yielding unbiased error estimates. The model reports that mtry = 4, which is
#the number of variables randomly chosen at each split. Since we have 13 overall
#variables, we could try all 13 possible values of mtry. We will do so, record
#the results, and make a plot.

#Varying the number of variables used at each step of the random forest procedure.
oob.err = numeric(7)
for (mtry in 1:7) {
  fit = randomForest(residuals ~ . -V1 -SalePrice -y_hat -Utilities, data = h.res.data[train,], mtry = mtry)
  oob.err[mtry] = fit$mse[500]
  cat("We're performing iteration", mtry, "\n")
}

# par(mfrow = c(1, 1))
# par(mar=c(5, 4, 2, 2) + 0.1)
#?par
#Visualizing the OOB error.
plot(1:7, oob.err, pch = 16, type = "b",
     xlab = "Variables Considered at Each Split",
     ylab = "OOB Mean Squared Error",
     main = "Random Forest OOB Error Rates\nby # of Variables")

#Can visualize a variable importance plot.
# importance(rf.res)
# write.csv(importance(rf.res), file = "variable_importance_random_forest.csv")
varImpPlot(rf.res)

yhat = predict(rf.res, newdata = h.res.data[-train, ])
house.test = h.res.data[-train, "residuals"]

plot(yhat, house.test$residuals)
abline(0, 1, col="red")
sqrt(mean((yhat - house.test)^2))

sqrt(mean((yhat+h.res.data[-train,"y_hat"] - h.res.data[-train,"SalePrice"])^2))
# vs 
sqrt(mean((h.res.data[-train,"residuals"])^2))

################# BOOSTING ################
library(gbm)

# set.seed(0)
boost.res = gbm(residuals ~ . -V1 -SalePrice -y_hat -Utilities, data = h.res.data[train, ],
                   distribution = "gaussian",
                   n.trees = 10000,
                   interaction.depth = 4,
                   shrinkage = 0.001)
predmat2 = predict(boost.res, newdata = h.res.data[-train, ], n.trees = n.trees)

berr2 = with(h.res.data[-train, ], apply((predmat2 - residuals)^2, 2, mean))
plot(n.trees, berr2, pch = 16, col="steel blue",
     ylab = "Mean Squared Error",
     xlab = "# Trees",
     main = "Boosting Test Error")
abline(h = min(berr2), col = "red")

n.trees.min <- n.trees[berr2 == min(berr2)]

yhat = predict(boost.res, newdata = h.res.data[-train, ],n.trees = n.trees.min)
# yhat
house.test = h.res.data[-train, "residuals"]
# house.test
plot(yhat, house.test$residuals)
abline(0, 1, col="red")
mean((yhat - house.test)^2)
sqrt(mean((yhat - house.test)^2))

sqrt(mean((yhat+h.res.data[-train,"y_hat"] - h.res.data[-train,"SalePrice"])^2))
# vs
sqrt(mean((h.res.data[-train,"residuals"])^2))

