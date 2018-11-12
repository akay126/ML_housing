####### TREE BASED MODELS ######

##################### REGRESSION TREE #################
library(tree)
library(MASS)

#Creating a training set on 70% of the data.
# set.seed(0)
train = sample(1:nrow(my.house.data), 7*nrow(my.house.data)/10)

#Training the tree to predict the median value of owner-occupied homes (in $1k).
tree.house = tree(SalePrice ~ ., my.house.data, subset = train)
summary(tree.house)

#Visually inspecting the regression tree.
plot(tree.house)
text(tree.house, pretty = 0)
tree.house

#Performing cross-validation.
# set.seed(0)
cv.house = cv.tree(tree.house)
par(mfrow = c(1, 2))
plot(cv.house$size, cv.house$dev, type = "b",
     xlab = "Terminal Nodes", ylab = "RSS")
plot(cv.house$k, cv.house$dev, type  = "b",
     xlab = "Alpha", ylab = "RSS")

#Calculating and assessing the MSE of the test data on the overall tree.
yhat = predict(tree.house, newdata = my.house.data[-train, ])
# yhat
house.test = my.house.data[-train, "SalePrice"]
# house.test
plot(yhat, house.test)
abline(0, 1)
mean((yhat - house.test)^2)

#Pruning the tree to have 4 terminal nodes.
prune.house = prune.tree(tree.house, best = 4)
par(mfrow = c(1, 1))
plot(prune.house)
text(prune.house, pretty = 0)

#Calculating and assessing the MSE of the test data on the pruned tree.
yhat = predict(prune.house, newdata = my.house.data[-train, ])
# yhat
plot(yhat, house.test)
abline(0, 1)
mean((yhat - house.test)^2)

##################### RANDOM FOREST #################
library(randomForest)
rf.house = randomForest(SalePrice ~ . -Id, data = my.house.data, subset = train, importance = TRUE)
rf.house
#The MSE and percent variance explained are based on out-of-bag estimates,
#yielding unbiased error estimates. The model reports that mtry = 4, which is
#the number of variables randomly chosen at each split. Since we have 13 overall
#variables, we could try all 13 possible values of mtry. We will do so, record
#the results, and make a plot.

#Varying the number of variables used at each step of the random forest procedure.
oob.err = numeric(9)
for (mtry in 1:9) {
  fit = randomForest(SalePrice ~ ., data = my.house.data[train,], mtry = mtry)
  oob.err[mtry] = fit$mse[500]
  cat("We're performing iteration", mtry, "\n")
}

# par(mfrow = c(1, 1))
# par(mar=c(5, 4, 2, 2) + 0.1)
#?par
#Visualizing the OOB error.
plot(1:9, oob.err, pch = 16, type = "b",
     xlab = "Variables Considered at Each Split",
     ylab = "OOB Mean Squared Error",
     main = "Random Forest OOB Error Rates\nby # of Variables")

#Can visualize a variable importance plot.
rf.var.importance <- importance(rf.house)
colnames(rf.var.importance)[1] <- c("IncMSE")
rf.var.importance <- rf.var.importance %>% 
  as.data.frame(.) %>% 
  mutate(xname = row.names(.)) %>% 
  arrange(desc(IncMSE))

write.csv(importance(rf.house), file = "variable_importance_random_forest.csv")
varImpPlot(rf.house)

yhat = predict(rf.house, newdata = my.house.data[-train, ])
house.test = my.house.data[-train, "SalePrice"]

plot(yhat, house.test)
abline(0, 1, col="red")
sqrt(mean((yhat - house.test)^2))

################# BOOSTING ################
library(gbm)

# set.seed(0)
boost.house2 = gbm(SalePrice ~ . -Id, data = my.house.data[train, ],
                   distribution = "gaussian",
                   n.trees = 10000,
                   interaction.depth = 4,
                   shrinkage = 0.03)
predmat2 = predict(boost.house2, newdata = my.house.data[-train, ], n.trees = n.trees)

berr2 = with(my.house.data[-train, ], apply((predmat2 - SalePrice)^2, 2, mean))
plot(n.trees, berr2, pch = 16, col="steel blue",
     ylab = "Mean Squared Error",
     xlab = "# Trees",
     main = "Boosting Test Error")
abline(h = min(berr2), col = "red")

n.trees.min <- n.trees[berr2 == min(berr2)]

yhat = predict(boost.house2, newdata = my.house.data[-train, ],n.trees = n.trees.min)
house.test = my.house.data[-train, "SalePrice"]

plot(yhat, house.test)
abline(0, 1, col="red")
mean((yhat - house.test)^2)
sqrt(mean((yhat - house.test)^2))


