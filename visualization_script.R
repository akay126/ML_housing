library(data.table)
library(plotly)
# install.packages('tidyr')
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(stringr)
library(scales)

# LOAD THE DATA
house.data <- fread(file = "./data/train.csv", stringsAsFactors = TRUE)

house.data %>% summary()

# First look at  THE TARGET VARIABLE
house.data %>% 
  ggplot() +
  geom_histogram(aes(x=SalePrice))
  # geom_qq(aes(sample=SalePrice), distribution = qnorm) +
  # geom_qq_line(aes(sample=SalePrice), distribution = qnorm,
               # col= 'red')

# CHECKOUT THE TRANSFORMED TARGET VARIABLE
house.data %>% 
  mutate(SalePrice = log(SalePrice)) %>%
  ggplot() +
  # geom_histogram(aes(x=SalePrice))
  geom_qq(aes(sample=SalePrice)) +
  geom_qq_line(aes(sample=SalePrice), col= 'red')

p1 <- my.house.data %>% 
  ggplot() +
  geom_point(aes(x=scale(YearBuilt), y=SalePrice , col=SalePrice), alpha = 0.3) +
  scale_color_gradient(low="blue",high="red") + 
  theme(legend.position = "bottom")

ggExtra::ggMarginal(
  p = p1,
  type = 'density',
  margins = 'both',
  size = 4,
  colour = 'black',
  fill = '#EBA1F5'
)

my.house.data %>% 
  ggplot(aes(x=as.character(YrSold),y=SalePrice)) + 
  geom_boxplot() +
  # geom_dotplot(binaxis = "y", stackdir ="center",dotsize = 0.3,alpha = 0.2,stackratio = 0.3,binwidth = 3) +
  # geom_violin(scale = "area", alpha = 0.25) +
  scale_color_gradient(low="blue",high="red")

# Scatterplot YearBuilt vs Sale Price by MS Zoning 
my.house.data %>% 
  mutate(SalePrice = exp(SalePrice)) %>% 
  group_by(YearBuilt,Neighborhood) %>% 
  summarise(GrLivArea = sum(GrLivArea), SalePrice = mean(SalePrice), n = n()) %>% 
  ggplot() +
  geom_point(aes(x=GrLivArea, y=SalePrice , col=Neighborhood, size = n), alpha = 0.5) +
  # scale_color_gradient(low="blue",high="red") + 
  theme(legend.position = "bottom")

# Scatterplot YearBuilt vs Sale Price by Neighborhood
my.house.data %>% 
  mutate(SalePrice = exp(SalePrice)) %>% 
  group_by(YearBuilt,Neighborhood) %>% 
  summarise(SalePrice = mean(SalePrice), n = n()) %>% 
  ggplot() +
  geom_point(aes(x=YearBuilt, y=SalePrice , col=Neighborhood, size = n), alpha = 0.5) +
  theme(legend.position = "bottom")

my.house.data %>% 
  mutate(SalePrice = exp(SalePrice)) %>% 
  group_by(YearBuilt,Neighborhood) %>% 
  summarise(SalePrice = sum(SalePrice), n = n(),
            GrLivArea = mean(GrLivArea),LotArea = sum(LotArea)) %>% 
  ggplot() +
  geom_point(aes(x=YearBuilt, y=reorder(Neighborhood, YearBuilt), 
                 col=SalePrice, 
                 size = n), 
             alpha = 0.3)  +
  scale_color_gradient(low="blue",high="red") +
  theme(legend.position = "bottom")

# Scatterplot LotFrontage vs Sale Price by Neighborhood
my.house.data %>% 
  mutate(SalePrice = exp(SalePrice)) %>% 
  group_by(YearBuilt,Neighborhood) %>% 
  summarise(SalePrice = mean(SalePrice), LotArea = sum(LotArea), n = n()) %>% 
  ggplot() +
  geom_point(aes(x=LotArea, y=SalePrice , col=YearBuilt, size = n), alpha = 0.5) +
  scale_color_gradient(low="blue",high="red") +
  # coord_cartesian(xlim = c(0, 20000)) +
  coord_cartesian(xlim = c(0, 200000)) +
  theme(legend.position = "bottom")

# Year in which 80% of the houses where built per Neighborhood
ggplotly(
  my.house.data %>% 
    mutate(SalePrice = exp(SalePrice)) %>% 
    group_by(Neighborhood) %>% 
    summarise(YearBuilt = quantile(YearBuilt,0.8),
              SalePrice = mean(SalePrice), n = n()) %>% 
    ggplot() +
    geom_point(aes(x=YearBuilt, y=SalePrice , col=Neighborhood, size = n), alpha = 0.5) +
    theme(legend.position = "bottom")
)
  
library(car)

which(sapply(house.data, class)=="integer")

my.house.data %>%
  select(which(sapply(house.data, class)=="integer")) %>% 
  select(2:9) %>% 
  scatterplotMatrix(., diagonal=list(method ="histogram"), col= "steel blue",
                    ellipse=FALSE, smooth = FALSE,var.labels = colnames(.))

sapply(select(my.house.data,which(sapply(my.house.data, class)=="integer")),
       function(x) list(sum(is.na(x)),
                        mean(x,na.rm=TRUE),
                        sd(x,na.rm=TRUE)))

avPlots(lm(log(SalePrice) ~ . -Id -MSSubClass, 
           data = select(my.house.data,which(sapply(my.house.data, class)=="integer"))))




