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

####### First look at THE TARGET VARIABLE ######
my.house.data %>%
  ggplot() +
  # geom_histogram(aes(x=SalePrice), fill = "blue", alpha = 0.4, bins = 5)
  geom_qq(aes(sample=SalePrice), distribution = qnorm) +
  geom_qq_line(aes(sample=SalePrice), distribution = qnorm,col= 'red')

my.house.data %>% 
  # mutate(LivArea = TotalBsmtSF + FirstFlrSF + SecondFlrSF) %>% 
  ggplot() +
  # geom_histogram(aes(x=exp(SalePrice)), fill = "red", alpha = 0.5)
  geom_qq(aes(sample=exp(SalePrice)), distribution = qnorm) +
  geom_qq_line(aes(sample=exp(SalePrice)), distribution = qnorm,col= 'red')

# CHECKOUT THE TRANSFORMED TARGET VARIABLE
house.data %>% 
  mutate(SalePrice = log(SalePrice)) %>%
  ggplot() +
  geom_histogram(aes(x=SalePrice))
  geom_qq(aes(sample=SalePrice)) +
  geom_qq_line(aes(sample=SalePrice), col= 'red')

######## EXPLORING THE FEATURES ##########
  
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

# getPalette = colorRampPalette(brewer.pal(25, "RdBu"))

my.house.data %>% 
  # filter(Neighborhood %in% c("NAmes","NWAmes","CollgCr")) %>% 
  # mutate(SalePrice = exp(SalePrice)) %>% 
  # group_by(YearBuilt,Neighborhood) %>%
  # summarise(SalePrice = sum(SalePrice), n = n()) %>% 
  ggplot() +
  geom_boxplot(aes(x=reorder(Neighborhood, YearBuilt,median),y=YearBuilt, 
                   fill=reorder(Neighborhood, SalePrice,median)))  +
  scale_fill_brewer(palette = "RdBu") +
  # RColorBrewer::brewer.pal(25, "RdBu")
  coord_flip() +
  theme(legend.position = "bottom")

# Scatterplot GrLivArea vs Sale Price by Neighborhood
my.house.data %>% 
  # mutate(SalePrice = exp(SalePrice)) %>% 
  # group_by(YearBuilt,Neighborhood) %>% 
  # summarise(SalePrice = mean(SalePrice), GrLivArea = sum(GrLivArea), n = n()) %>% 
  ggplot() +
  geom_point(aes(x=OverallQual, y=SalePrice , col=BedroomAbvGr), alpha = 0.5) +
  # scale_color_gradient(low="blue",high="red") +
  # coord_cartesian(xlim = c(0, 20000)) +
  theme(legend.position = "bottom")

# Year in which 80% of the houses where built per Neighborhood
ggplotly(
  my.house.data %>% 
    filter(!(GrLivArea > 4000 & SalePrice < 300000)) %>%
    mutate(SalePrice = exp(SalePrice)) %>%
    # group_by(Neighborhood) %>%
    # summarise(YearBuilt = quantile(YearBuilt,0.8),
    #           SalePrice = mean(SalePrice), n = n()) %>% 
    ggplot() +
    geom_point(aes(x=GrLivArea, y=SalePrice , col=Neighborhood), alpha = 0.5) +
    theme(legend.position = "bottom")
)

my.house.data %>% 
  filter(TotalBsmtSF < 4000) %>% 
  mutate(SalePrice = exp(SalePrice)) %>%
  group_by(YearBuilt,Neighborhood) %>%
  summarise(GrLivArea = mean(GrLivArea), TotalBsmtSF = mean(TotalBsmtSF),
            SalePrice = mean(SalePrice), n = n()) %>%
  gather(GrLivArea,TotalBsmtSF,SalePrice,
         key="Feature",value="Value") %>% 
  ggplot() +
  geom_point(aes(x=YearBuilt, y=Value , col=Neighborhood, size = n), alpha = 0.5) +
  facet_grid(Feature ~ ., scales = "free") +
  theme(legend.position = "right")

############ BINNING ############

my.house.data %>% 
  ggplot() +
  geom_histogram(aes(x=SalePrice),fill="steel blue",alpha=0.8) +
  geom_vline(aes(xintercept=temp.cuts[1]-0.09, col = "red")) +
  geom_vline(aes(xintercept=temp.cuts[2], col = "red")) +
  geom_vline(aes(xintercept=temp.cuts[3], col = "red")) +
  geom_vline(aes(xintercept=temp.cuts[4], col = "red")) +
  geom_vline(aes(xintercept=temp.cuts[5], col = "red")) +
  geom_vline(aes(xintercept=temp.cuts[6]+0.1, col = "red")) +
  labs(title="Histogram of Log Sale Price",
       y="Frequency",
       x="Log Sale Price")

########### 3D PLOTS #############
library(plotly)
 
my.house.data %>% 
  filter(!(GrLivArea > 4000 & SalePrice < 300000)) %>%
  mutate(SalePrice = exp(SalePrice)) %>%
  plot_ly(x = ~GrLivArea, y = ~SalePrice, z = ~reorder(as.numeric(Neighborhood),SalePrice,mean),
          color = ~Neighborhood) %>%
  add_markers()

my.house.data %>% 
  filter(TotalBsmtSF < 4000) %>% 
  mutate(SalePrice = exp(SalePrice)) %>%
  group_by(YearBuilt,Neighborhood) %>%
  summarise(GrLivArea = mean(GrLivArea), TotalBsmtSF = mean(TotalBsmtSF),
            SalePrice = mean(SalePrice), n = n()) %>%
  plot_ly(x = ~YearBuilt, y = ~SalePrice, z = ~GrLivArea,
          color = ~Neighborhood, size = ~n,
          marker = list(symbol = 'circle', sizemode = 'diameter'), sizes = c(2,40)) %>%
  add_markers()

########## SCATTER MATRIX #############

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

####### GRAPH EXPORTS FOR THE MARKDOWN ########

vis.data <- fread(file = "./data/my_train.csv", stringsAsFactors = TRUE)


vis.data %>%
  ggplot() +
  geom_histogram(aes(x=exp(SalePrice)), fill = "red", alpha = 0.6, bins = 50) +
  labs(title="Histogram of Sale Price",
       y="Sale Price")
vis.data %>%
  ggplot() +
  geom_histogram(aes(x=SalePrice), fill = "blue", alpha = 0.6, bins = 50) +
  labs(title="Histogram of Log Sale Price")

vis.data %>%
  ggplot() +
  geom_qq(aes(sample=exp(SalePrice)), distribution = qnorm) +
  geom_qq_line(aes(sample=exp(SalePrice)), distribution = qnorm,col="red") +
  labs(title="QQ Plot of Sale Price",
       y="Sale Price")
vis.data %>%
  ggplot() +
  geom_qq(aes(sample=SalePrice), distribution = qnorm) +
  geom_qq_line(aes(sample=SalePrice), distribution = qnorm,col="red") +
  labs(title="QQ Plot of Sale Price")





