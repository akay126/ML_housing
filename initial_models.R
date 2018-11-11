################ INITIAL MODELS ################

model.first <- lm(SalePrice ~ YearBuilt + YearRemodAdd + YearsLastRemod + Remod,
                  data = my.house.data)

summary(model.first)
library(car) #Companion to applied regression.
influencePlot(model.first)

vif(model.first) #Assessing the variance inflation factors for the variables in our model.

#Added variable plots for assessing the contribution of each additional variable.
avPlots(model.first)

# From variable importance in random forest, extract only variables which when removed,
# cause an increase in MSE
x.best <- rf.var.importance %>% 
  top_n(20, IncMSE)
  # filter(IncMSE > 0)

reduced.house <- my.house.data %>% 
  select(SalePrice, x.best$xname) %>% 
  mutate(SalePrice = exp(SalePrice))
model.best.tree <- lm(SalePrice ~ ., data = reduced.house)

summary(model.best.tree)

library(car)
bc = boxCox(model.best.tree,lambda = c(-1:1))
#Automatically plots a 95% confidence interval for the lambda
#value that maximizes the likelihhood of transforming to
#normality.

lambda = bc$x[which(bc$y == max(bc$y))] #Extracting the best lambda value.

reduced.bc.house <- reduced.house %>% 
  mutate(SalePrice = (SalePrice^lambda - 1)/lambda) #Applying the Box-Cox transformation.

reduced.bc.house %>% 
  ggplot() +
  # geom_histogram(aes(x=SalePrice))
  geom_qq(aes(sample=SalePrice), col="steel blue") +
  geom_qq_line(aes(sample=SalePrice), col= 'red')

reduced.bc.house %>% 
  ggplot() +
  geom_point(aes(x=reduced.house$SalePrice, y=SalePrice), col="steel blue")

model.best.tree <- lm(SalePrice ~ ., data = reduced.bc.house) #Creating a new regression based on the
#transformed variable.

summary(model.best.tree)

sqrt(mean(model.best.tree$residuals^2))
