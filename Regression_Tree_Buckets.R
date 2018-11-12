################ Regression Tree ##########

data = read.csv('data/my_train.csv', stringsAsFactors =  TRUE)
data = data[-1]

library(tree)
train = sample(1:nrow(data), 7*nrow(data)/10)
tree.price = tree(SalePrice ~ . ,data,subset = train)
summary(tree.price)
plot (tree.price)
text(tree.price, pretty = 0)

prune.price = prune.tree(tree.price, best = 4)
summary(prune.price)

par(mfrow = c(1,2))
qqnorm(data$SalePrice, main = "my Q-Q" ); qqline(data$SalePrice, lty = 2)

plot(prune.price)
text(prune.price, pretty = 0)

############################################
