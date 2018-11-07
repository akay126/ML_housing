#################### DATA MUTATION ##################

my.house.data <- house.data %>% 
  # mutate(YearsLastRemod = YrSold-YearRemodAdd,
  #        Remod = factor(ifelse(YearBuilt == YearRemodAdd,0,1)),
  #        YearsFirstRemod = YearRemodAdd-YearBuilt) %>% 
  rename(FirstFlrSF = '1stFlrSF',
         SecondFlrSF = '2ndFlrSF',
         SsnPorch = '3SsnPorch') %>% 
  mutate(SalePrice = log(SalePrice)) %>% 
  mutate(GarageYrBlt = ifelse(is.na(GarageYrBlt),YearBuilt,GarageYrBlt),
         LotFrontage = ifelse(is.na(LotFrontage),0,LotFrontage),
         MasVnrArea = ifelse(is.na(MasVnrArea),0,MasVnrArea)) %>% 
  filter(!is.na(MasVnrType),
         !is.na(Electrical)) %>%
  mutate(Alley = factor(ifelse(is.na(Alley),"NoAlley",Alley)),
         BsmtQual = factor(ifelse(is.na(BsmtQual),"NoBsmt",BsmtQual)),
         BsmtCond = factor(ifelse(is.na(BsmtCond),"NoBsmt",BsmtCond)),
         BsmtExposure = factor(ifelse(is.na(BsmtExposure),"NoBsmt",BsmtExposure)),
         BsmtFinType1 = factor(ifelse(is.na(BsmtFinType1),"NoBsmt",BsmtFinType1)),
         BsmtFinType2 = factor(ifelse(is.na(BsmtFinType2),"NoBsmt",BsmtFinType2)),
         FireplaceQu = factor(ifelse(is.na(FireplaceQu),"NoFire",FireplaceQu)),
         GarageType = factor(ifelse(is.na(GarageType),"NoGarage",GarageType)),
         GarageFinish = factor(ifelse(is.na(GarageFinish),"NoGarage",GarageFinish)),
         GarageQual = factor(ifelse(is.na(GarageQual),"NoGarage",GarageQual)),
         GarageCond = factor(ifelse(is.na(GarageCond),"NoGarage",GarageCond)),
         PoolQC = factor(ifelse(is.na(PoolQC),"NoPool",PoolQC)),
         Fence = factor(ifelse(is.na(Fence),"NoFence",Fence)),
         MiscFeature = factor(ifelse(is.na(MiscFeature),"None",MiscFeature))) %>% 
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
  

# SHOULD WE IMPUTE VALUES FOR MasVnrType + MasVnrArea ??
house.data %>% 
  filter(is.na(MasVnrType)) %>% 
  summary()
