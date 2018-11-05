library(data.table)
library(plotly)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(stringr)
library(scales)

# LOAD THE DATA
house.data <- fread(file = "./data/train.csv", stringsAsFactors = TRUE)

house.data %>% summary()

# CHECKOUT THE TARGET VARIABLE
house.data %>% 
  ggplot() +
  geom_histogram(aes(x=SalePrice))
  # geom_qq(aes(sample=SalePrice), distribution = qnorm) +
  # geom_qq_line(aes(sample=SalePrice), distribution = qnorm,
               # col= 'red')

# CHECKOUT THE TARGET VARIABLE VS ANOTHER DISTRIBUTION
house.data %>% 
  ggplot() +
  geom_qq(aes(sample=SalePrice), distribution = qexp) +
  geom_qq_line(aes(sample=SalePrice), distribution = qexp,
               col= 'red')

# cHECKOUT THE TRANSFORMED TARGET VARIABLE
house.data %>% 
  mutate(SalePrice = log(SalePrice)) %>% 
  ggplot() +
  # geom_histogram(aes(x=SalePrice))
  geom_qq(aes(sample=SalePrice)) +
  geom_qq_line(aes(sample=SalePrice), col= 'red')

# SUMMARY OF THE ENTIRE DATASET
ggplotly(
  house.data %>% 
    ggplot(aes(YrSold,SalePrice)) + 
    geom_point()
)

# INITIAL MODEL

model.first <- lm(log(SalePrice) ~ MSZoning + LotFrontage + LotArea + OverallQual + 
                    OverallCond + YearBuilt + YearRemodAdd + TotalBsmtSF + 
                    FullBath + KitchenQual + TotRmsAbvGrd,
                  data = house.data)

summary(model.first)
plot(model.first)

house.data %>% 
  ggplot() +
  geom_point(aes(x=YearBuilt, y = YearRemodAdd))







