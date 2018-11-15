
#######################################################################################################################
#######################################################################################################################
#- DATA CLEANING - TEST SET -------------------------------------------------------------------------------------------

'%!in%' <- function(x,y)!('%in%'(x,y))
house.data <- read.csv(file = "./data/test.csv", stringsAsFactors = TRUE)

my.house.data <- house.data %>% 
  # mutate(YearsLastRemod = YrSold-YearRemodAdd,
  #        YearsFirstRemod = YearRemodAdd-YearBuilt) %>%
  # mutate(Remod = factor(ifelse(YearBuilt == YearRemodAdd,0,1))) %>% 
  rename(FirstFlrSF = 'X1stFlrSF',
         SecondFlrSF = 'X2ndFlrSF',
         SsnPorch = 'X3SsnPorch') %>% 
  # select(-MoSold, -YrSold, -SaleType, -SaleCondition) %>% 
  mutate(SalePrice = log(SalePrice)) %>% 
  # mutate(SalePrice = (SalePrice^lambda - 1)/lambda) %>% 
  mutate(GarageYrBlt = ifelse(is.na(GarageYrBlt),YearBuilt,GarageYrBlt),
         LotFrontage = ifelse(is.na(LotFrontage),0,LotFrontage),
         MasVnrArea = ifelse(is.na(MasVnrArea),0,MasVnrArea)) %>% 
  # filter(!is.na(MasVnrType),
  #        !is.na(Electrical)) %>%
  # filter(!(GrLivArea > 4000 & SalePrice < 300000)) %>%
  mutate(Alley = factor(ifelse(is.na(Alley),"NoAlley",as.character(Alley))),
         BsmtQual = factor(ifelse(is.na(BsmtQual),"NoBsmt",as.character(BsmtQual))),
         BsmtCond = factor(ifelse(is.na(BsmtCond),"NoBsmt",as.character(BsmtCond))),
         BsmtExposure = factor(ifelse(is.na(BsmtExposure),"NoBsmt",as.character(BsmtExposure))),
         BsmtFinType1 = factor(ifelse(is.na(BsmtFinType1),"NoBsmt",as.character(BsmtFinType1))),
         BsmtFinType2 = factor(ifelse(is.na(BsmtFinType2),"NoBsmt",as.character(BsmtFinType2))),
         FireplaceQu = factor(ifelse(is.na(FireplaceQu),"NoFire",as.character(FireplaceQu))),
         GarageType = factor(ifelse(is.na(GarageType),"NoGarage",as.character(GarageType))),
         GarageFinish = factor(ifelse(is.na(GarageFinish),"NoGarage",as.character(GarageFinish))),
         GarageQual = factor(ifelse(is.na(GarageQual),"NoGarage",as.character(GarageQual))),
         GarageCond = factor(ifelse(is.na(GarageCond),"NoGarage",as.character(GarageCond))),
         PoolQC = factor(ifelse(is.na(PoolQC),"NoPool",as.character(PoolQC))),
         Fence = factor(ifelse(is.na(Fence),"NoFence",as.character(Fence))),
         MiscFeature = factor(ifelse(is.na(MiscFeature),"None",as.character(MiscFeature)))) %>% 
  mutate(BsmtFullBath = factor(BsmtFullBath),
         BsmtHalfBath = factor(BsmtHalfBath),
         FullBath = factor(FullBath),
         HalfBath = factor(HalfBath),
         BedroomAbvGr = factor(BedroomAbvGr),
         KitchenAbvGr = factor(KitchenAbvGr),
         KitchenQual = factor(KitchenQual),
         Fireplaces = factor(Fireplaces),
         GarageCars = factor(GarageCars),
         YrSold = factor(YrSold))

write.csv(my.house.data, file = "./data/my_train.csv", row.names = FALSE)

test.house.data <- fread(file = "./data/test.csv", stringsAsFactors = TRUE)

test.house.data <- test.house.data %>% 
  rename(FirstFlrSF = '1stFlrSF',
         SecondFlrSF = '2ndFlrSF',
         SsnPorch = '3SsnPorch') %>% 
  mutate(GarageYrBlt = ifelse(is.na(GarageYrBlt),YearBuilt,GarageYrBlt),
         LotFrontage = ifelse(is.na(LotFrontage),0,LotFrontage),
         MasVnrArea = ifelse(is.na(MasVnrArea),0,MasVnrArea)) %>% 
  mutate(Alley = factor(ifelse(is.na(Alley),"NoAlley",as.character(Alley))),
         BsmtQual = factor(ifelse(is.na(BsmtQual),"NoBsmt",as.character(BsmtQual))),
         BsmtCond = factor(ifelse(is.na(BsmtCond),"NoBsmt",as.character(BsmtCond))),
         BsmtExposure = factor(ifelse(is.na(BsmtExposure),"NoBsmt",as.character(BsmtExposure))),
         BsmtFinType1 = factor(ifelse(is.na(BsmtFinType1),"NoBsmt",as.character(BsmtFinType1))),
         BsmtFinType2 = factor(ifelse(is.na(BsmtFinType2),"NoBsmt",as.character(BsmtFinType2))),
         FireplaceQu = factor(ifelse(is.na(FireplaceQu),"NoFire",as.character(FireplaceQu))),
         GarageType = factor(ifelse(is.na(GarageType),"NoGarage",as.character(GarageType))),
         GarageFinish = factor(ifelse(is.na(GarageFinish),"NoGarage",as.character(GarageFinish))),
         GarageQual = factor(ifelse(is.na(GarageQual),"NoGarage",as.character(GarageQual))),
         GarageCond = factor(ifelse(is.na(GarageCond),"NoGarage",as.character(GarageCond))),
         PoolQC = factor(ifelse(is.na(PoolQC),"NoPool",as.character(PoolQC))),
         Fence = factor(ifelse(is.na(Fence),"NoFence",as.character(Fence))),
         MiscFeature = factor(ifelse(is.na(MiscFeature),"None",as.character(MiscFeature)))) %>% 
  mutate(BsmtFullBath = factor(BsmtFullBath),
         BsmtHalfBath = factor(BsmtHalfBath),
         FullBath = factor(FullBath),
         HalfBath = factor(HalfBath),
         BedroomAbvGr = factor(BedroomAbvGr),
         KitchenAbvGr = factor(KitchenAbvGr),
         KitchenQual = factor(KitchenQual),
         Fireplaces = factor(Fireplaces),
         GarageCars = factor(GarageCars),
         YrSold = factor(YrSold))

write.csv(test.house.data, file = "./data/my_test.csv", row.names = FALSE)

# SHOULD WE IMPUTE VALUES FOR MasVnrType + MasVnrArea ??
house.data %>% 
  filter(is.na(MasVnrType)) %>% 
  summary()




