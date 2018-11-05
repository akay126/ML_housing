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

# CHECKOUT THE TRANSFORMED TARGET VARIABLE
house.data %>% 
  mutate(SalePrice = log(SalePrice)) %>% 
  ggplot() +
  # geom_histogram(aes(x=SalePrice))
  geom_qq(aes(sample=SalePrice)) +
  geom_qq_line(aes(sample=SalePrice), col= 'red')

house.data %>% 
  ggplot(aes(YearBuilt,SalePrice)) + 
  geom_point()

# SUMMARY OF THE ENTIRE DATASET
column.names <- colnames(house.data)
column.classes <- sapply(house.data[,-81], class)

select(house.data,2,81) %>% 
  head(.)



i <- 3
for (i in 2:5){
  if (column.classes[i] == "factor") {
    select(house.data,i,81) %>% 
      ggplot() +
      geom_boxplot(aes(x=column.names[i],y=house.data$SalePrice))
    print("fact")
  }
  if (column.classes[i] == "integer") {
    print("int")
  }
  print(i)
}

house.data[,2]

ggplot(house.data) + geom_point(aes())

house.data %>% 
  ggplot() +
  geom_point(aes(x=colnames(house.data)[i],y=SalePrice)) +
  labs(title = paste0(colnames(house.data)[i]))


