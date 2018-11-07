################ INITIAL MODELS ################

model.first <- lm(log(SalePrice) ~ YearBuilt + YearRemodAdd + YearsLastRemod + Remod,
                  data = my.house.data)

summary(model.first)
library(car) #Companion to applied regression.
influencePlot(model.first)

vif(model.first) #Assessing the variance inflation factors for the variables in our model.

#Added variable plots for assessing the contribution of each additional variable.
avPlots(model.first)

my.house.data %>%
  select(SalePrice,MSZoning,LotArea,OverallQual,OverallCond,YearBuilt,TotalBsmtSF,
         FullBath,KitchenQual,TotRmsAbvGrd) %>% 
  scatterplotMatrix(., diagonal=list(method ="histogram"), col= "steel blue",
                    ellipse=FALSE, smooth = FALSE,var.labels = colnames(.))

model.first <- lm(log(SalePrice) ~ MSZoning + LotArea +
                    OverallQual + OverallCond + YearBuilt + TotalBsmtSF + 
                    FullBath + KitchenQual + TotRmsAbvGrd,
                  data = my.house.data)

summary(model.first)
plot(model.first)

vif(model.first)

avPlots(model.first)

##################### REGRESSION TREE #################
library(tree)
library(MASS)

#Creating a training set on 70% of the data.
set.seed(0)
train = sample(1:nrow(my.house.data), 7*nrow(my.house.data)/10)

#Training the tree to predict the median value of owner-occupied homes (in $1k).
tree.house = tree(SalePrice ~ ., my.house.data, subset = train)
summary(tree.house)

#Visually inspecting the regression tree.
plot(tree.house)
text(tree.house, pretty = 0)
tree.house

#Performing cross-validation.
set.seed(0)
cv.house = cv.tree(tree.house)
par(mfrow = c(1, 2))
plot(cv.house$size, cv.house$dev, type = "b",
     xlab = "Terminal Nodes", ylab = "RSS")
plot(cv.house$k, cv.house$dev, type  = "b",
     xlab = "Alpha", ylab = "RSS")

#Pruning the tree to have 4 terminal nodes.
prune.house = prune.tree(tree.house, best = 4)
par(mfrow = c(1, 1))
plot(prune.house)
text(prune.house, pretty = 0)

#Calculating and assessing the MSE of the test data on the overall tree.
yhat = predict(tree.house, newdata = my.house.data[-train, ])
yhat
house.test = my.house.data[-train, "SalePrice"]
house.test
plot(yhat, house.test)
abline(0, 1)
mean((yhat - house.test)^2)

#Calculating and assessing the MSE of the test data on the pruned tree.
yhat = predict(prune.house, newdata = my.house.data[-train, ])
yhat
plot(yhat, house.test)
abline(0, 1)
mean((yhat - house.test)^2)

##################### RANDOM FOREST #################

library(randomForest)
rf.house = randomForest(SalePrice ~ ., data = my.house.data, subset = train, importance = TRUE)
rf.house

oob.err = numeric(9)
for (mtry in 1:9) {
  fit = randomForest(SalePrice ~ ., data = my.house.data[train,], mtry = mtry)
  oob.err[mtry] = fit$mse[500]
  cat("We're performing iteration", mtry, "\n")
}

# par(mfrow = c(1, 1))
par(mar=c(5, 4, 2, 2) + 0.1)
#?par
#Visualizing the OOB error.
plot(1:15, oob.err, pch = 16, type = "b",
     xlab = "Variables Considered at Each Split",
     ylab = "OOB Mean Squared Error",
     main = "Random Forest OOB Error Rates\nby # of Variables")

#Can visualize a variable importance plot.
importance(rf.house)
varImpPlot(rf.house)

################# BOOSTING ################
library(gbm)

#Fitting 10,000 trees with a depth of 4.
set.seed(0)
boost.house = gbm(SalePrice ~ ., data = my.house.data[train, ],
                   distribution = "gaussian",
                   n.trees = 10000,
                   interaction.depth = 4)

#Inspecting the relative influence.
par(mfrow = c(1, 1))
summary(boost.house)

#Let’s make a prediction on the test set. With boosting, the number of trees is
#a tuning parameter; having too many can cause overfitting. In general, we should
#use cross validation to select the number of trees. Instead, we will compute the
#test error as a function of the number of trees and make a plot for illustrative
#purposes.
n.trees = seq(from = 100, to = 10000, by = 10)
predmat = predict(boost.house, newdata = my.house.data[-train, ], n.trees = n.trees)

#Produces 100 different predictions for each of the 152 observations in our
#test set.
dim(predmat)

#Calculating the boosted errors.
par(mfrow = c(1, 1))
berr = with(my.house.data[-train, ], apply((predmat - SalePrice)^2, 2, mean))
plot(n.trees, berr, pch = 16,
     ylab = "Mean Squared Error",
     xlab = "# Trees",
     main = "Boosting Test Error")

#Include the best OOB error from the random forest.
abline(h = min(berr), col = "red")

set.seed(0)
boost.house2 = gbm(SalePrice ~ ., data = my.house.data[train, ],
                    distribution = "gaussian",
                    n.trees = 10000,
                    interaction.depth = 4,
                    shrinkage = 0.04)
predmat2 = predict(boost.house2, newdata = my.house.data[-train, ], n.trees = n.trees)

berr2 = with(my.house.data[-train, ], apply((predmat2 - SalePrice)^2, 2, mean))
plot(n.trees, berr2, pch = 16, col="steel blue",
     ylab = "Mean Squared Error",
     xlab = "# Trees",
     main = "Boosting Test Error")
abline(h = min(berr2), col = "red")
