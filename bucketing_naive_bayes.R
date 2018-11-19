########### BUCKETING SALE PRICE ##########

library(tm)
library(SnowballC)
library(e1071)


########## PREPARING THE DATA ######

temp.cat.house <- house.data %>% 
  rename(FirstFlrSF = '1stFlrSF',
         SecondFlrSF = '2ndFlrSF',
         SsnPorch = '3SsnPorch') %>% 
  mutate(SalePrice = log(SalePrice)) %>% 
  mutate(GarageYrBlt = ifelse(is.na(GarageYrBlt),YearBuilt,GarageYrBlt),
         LotFrontage = ifelse(is.na(LotFrontage),0,LotFrontage),
         MasVnrArea = ifelse(is.na(MasVnrArea),0,MasVnrArea)) %>% 
  mutate(HouseStyle =  factor(gsub("[.]", "",as.character(HouseStyle))),
         MSZoning = factor(gsub("[ ()]", "",as.character(MSZoning)))) %>% 
  mutate(Alley = factor(ifelse(is.na(Alley),"",as.character(Alley))),
         BsmtQual = factor(ifelse(is.na(BsmtQual),"",as.character(BsmtQual))),
         BsmtCond = factor(ifelse(is.na(BsmtCond),"",as.character(BsmtCond))),
         BsmtExposure = factor(ifelse(is.na(BsmtExposure),"",as.character(BsmtExposure))),
         BsmtFinType1 = factor(ifelse(is.na(BsmtFinType1),"",as.character(BsmtFinType1))),
         BsmtFinType2 = factor(ifelse(is.na(BsmtFinType2),"",as.character(BsmtFinType2))),
         FireplaceQu = factor(ifelse(is.na(FireplaceQu),"",as.character(FireplaceQu))),
         GarageType = factor(ifelse(is.na(GarageType),"",as.character(GarageType))),
         GarageFinish = factor(ifelse(is.na(GarageFinish),"",as.character(GarageFinish))),
         GarageQual = factor(ifelse(is.na(GarageQual),"",as.character(GarageQual))),
         GarageCond = factor(ifelse(is.na(GarageCond),"",as.character(GarageCond))),
         PoolQC = factor(ifelse(is.na(PoolQC),"",as.character(PoolQC))),
         Fence = factor(ifelse(is.na(Fence),"",as.character(Fence))),
         MiscFeature = factor(ifelse(is.na(MiscFeature),"",as.character(MiscFeature)))) %>% 
  mutate(BsmtFullBath = factor(BsmtFullBath),
         BsmtHalfBath = factor(BsmtHalfBath),
         FullBath = factor(FullBath),
         HalfBath = factor(HalfBath),
         BedroomAbvGr = factor(BedroomAbvGr),
         KitchenAbvGr = factor(KitchenAbvGr),
         KitchenQual = factor(KitchenQual),
         Fireplaces = factor(Fireplaces),
         GarageCars = factor(GarageCars),
         YrSold = factor(YrSold),
         YearBuilt = factor(YearBuilt),
         MSSubClass = factor(MSSubClass))

temp.x.class <- as.data.frame(sapply(temp.cat.house,class))
colnames(temp.x.class)[1] <- c("x.class")
temp.x.class <- temp.x.class %>% 
  mutate(x.name = row.names(.))

for (i in temp.x.class$x.name[as.character(temp.x.class$x.class) == "factor"]){
  temp.cat.house[,i] = factor(ifelse(as.character(temp.cat.house[,i]) == "" | temp.cat.house[,i] == 0,
                                     "",
                                     paste0(i,"",as.character(temp.cat.house[,i]))))
}

cat.house.data <- temp.cat.house[,c(1:ncol(temp.cat.house))[as.character(temp.x.class$x.class) == "factor"]] %>% 
  select(-GarageCars, -BedroomAbvGr) %>%
  select(-YrSold)

ak.cat.house <- ak.test.data %>% 
  # rename(FirstFlrSF = '1stFlrSF',
  #        SecondFlrSF = '2ndFlrSF',
  #        SsnPorch = '3SsnPorch') %>% 
  # mutate(SalePrice = log(SalePrice)) %>% 
  mutate(GarageYrBlt = ifelse(is.na(GarageYrBlt),YearBuilt,GarageYrBlt),
         LotFrontage = ifelse(is.na(LotFrontage),0,LotFrontage),
         MasVnrArea = ifelse(is.na(MasVnrArea),0,MasVnrArea)) %>% 
  mutate(HouseStyle =  factor(gsub("[.]", "",as.character(HouseStyle))),
         MSZoning = factor(gsub("[ ()]", "",as.character(MSZoning)))) %>% 
  mutate(Alley = factor(ifelse(is.na(Alley),"",as.character(Alley))),
         BsmtQual = factor(ifelse(is.na(BsmtQual),"",as.character(BsmtQual))),
         BsmtCond = factor(ifelse(is.na(BsmtCond),"",as.character(BsmtCond))),
         BsmtExposure = factor(ifelse(is.na(BsmtExposure),"",as.character(BsmtExposure))),
         BsmtFinType1 = factor(ifelse(is.na(BsmtFinType1),"",as.character(BsmtFinType1))),
         BsmtFinType2 = factor(ifelse(is.na(BsmtFinType2),"",as.character(BsmtFinType2))),
         FireplaceQu = factor(ifelse(is.na(FireplaceQu),"",as.character(FireplaceQu))),
         GarageType = factor(ifelse(is.na(GarageType),"",as.character(GarageType))),
         GarageFinish = factor(ifelse(is.na(GarageFinish),"",as.character(GarageFinish))),
         GarageQual = factor(ifelse(is.na(GarageQual),"",as.character(GarageQual))),
         GarageCond = factor(ifelse(is.na(GarageCond),"",as.character(GarageCond))),
         PoolQC = factor(ifelse(is.na(PoolQC),"",as.character(PoolQC))),
         Fence = factor(ifelse(is.na(Fence),"",as.character(Fence))),
         MiscFeature = factor(ifelse(is.na(MiscFeature),"",as.character(MiscFeature)))) %>% 
  mutate(BsmtFullBath = factor(BsmtFullBath),
         BsmtHalfBath = factor(BsmtHalfBath),
         FullBath = factor(FullBath),
         HalfBath = factor(HalfBath),
         BedroomAbvGr = factor(BedroomAbvGr),
         KitchenAbvGr = factor(KitchenAbvGr),
         KitchenQual = factor(KitchenQual),
         Fireplaces = factor(Fireplaces),
         GarageCars = factor(GarageCars),
         YrSold = factor(YrSold),
         YearBuilt = factor(YearBuilt),
         MSSubClass = factor(MSSubClass))

ak.x.class <- as.data.frame(sapply(ak.cat.house,class))
colnames(ak.x.class)[1] <- c("x.class")
ak.x.class <- ak.x.class %>% 
  mutate(x.name = row.names(.))

for (i in ak.x.class$x.name[as.character(ak.x.class$x.class) == "factor"]){
  ak.cat.house[,i] = factor(ifelse(as.character(ak.cat.house[,i]) == "" | ak.cat.house[,i] == 0,
                                   "",
                                   paste0(i,"",as.character(ak.cat.house[,i]))))
}

ak.cat.house <- ak.cat.house[,c(1:ncol(ak.cat.house))[as.character(ak.x.class$x.class) == "factor"]] %>% 
  select(-GarageCars, -BedroomAbvGr) %>%
  select(-YrSold)

raw.house.data <- unite(cat.house.data, col = "house", sep = " ")
ak.raw.house.data <- unite(ak.cat.house, col = "house", sep = " ")

convert_counts = function(x) {
  x = ifelse(x > 0, "Yes", "No")
}

house_corpus = Corpus(VectorSource(c(raw.house.data$house,ak.raw.house.data$house)))
house_dtm = DocumentTermMatrix(house_corpus, control = list(
  tolower = TRUE,
  stemming = TRUE
))

realtest_house_dtm <- house_dtm[-(1:nrow(raw.house.data)),]
house_dtm <- house_dtm[1:nrow(raw.house.data),]

train = sample(1:nrow(my.house.data), 0.7*nrow(my.house.data))
house_dtm_train = house_dtm[train, ]
house_dtm_test = house_dtm[-train, ]
house_dtm_freq_train = removeSparseTerms(house_dtm_train, sparse = 0.999)
house_freq_words = findFreqTerms(house_dtm_train, 5)
house_dtm_freq_train = house_dtm_train[, house_freq_words]
house_dtm_freq_test = house_dtm_test[, house_freq_words]
house_train = apply(house_dtm_freq_train, 2, convert_counts)
house_test = apply(house_dtm_freq_test, 2, convert_counts)

realtest_house_total = removeSparseTerms(realtest_house_dtm, sparse = 0.999)
realtest_house_dtm_freq_words = findFreqTerms(realtest_house_total, 5)
realtest_house_dtm_freq = realtest_house_total[, realtest_house_dtm_freq_words]

house_total = removeSparseTerms(house_dtm, sparse = 0.999)
house_dtm_freq_words = findFreqTerms(house_total, 5)
house_dtm_freq = house_total[, house_dtm_freq_words]
house_total = apply(house_dtm_freq, 2, convert_counts)

keep.from.realtest <- colnames(realtest_house_total)[colnames(realtest_house_total) %in% colnames(house_total)]
realtest_house_dtm_freq = realtest_house_total[, keep.from.realtest]
realtest_house_total = apply(realtest_house_dtm_freq, 2, convert_counts)

realtest_house_total <- cbind(realtest_house_total,"No","No","No","No","No","No","No")
colnames(realtest_house_total)[(ncol(realtest_house_total)-6):ncol(realtest_house_total)] <- colnames(house_total)[colnames(house_total) %!in% colnames(realtest_house_total)]

############### PREPARING THE BUCKETS #############

raw.house.data <- raw.house.data %>% 
  # mutate(Target = my.house.data$YearBuilt) # CONTINUOUS
  # mutate(Target = factor(my.house.data$OverallQual)) # CATEGORICAL
  mutate(Target = my.house.data$OverallQual) # CATEGORICAL

# FOR CONTINUOUS VARIABLES ####
quantile(my.house.data$YearBuilt)
Inf.b <- quantile(my.house.data$YearBuilt)[-5]
Sup.b <- quantile(my.house.data$YearBuilt)[-1]

bucket <- data.frame(c("B1","B2","B3","B4"),
                     Inf.b,
                     Sup.b,
                     rep(0,4))
colnames(bucket) <- c("Bucket","Inf","Sup","accuracy")

# FOR CATEGORICAL VARIABLES ####
# k.target <- levels(raw.house.data$Target)
k.target <- unique(sort(as.numeric(as.character((raw.house.data$Target)))))[-1]

bucket <- data.frame(paste0("Greater ",k.target),
                     rep(0,length(k.target)),rep(0,length(k.target)),rep(0,length(k.target)))
colnames(bucket) <- c("bucket","TPR","FPR","accuracy")

i <- 1
for (k in k.target) {
  raw.house.data <- raw.house.data %>% 
    # mutate(Type = factor(ifelse(Target >= bucket[i,2] & Target < bucket[i,3],
    #                             as.character(bucket[i,1]),paste0("Not ",bucket[i,1]))))
    # mutate(Type = factor(ifelse(Target == k,
    #                             k,paste0("Not ",k))))
    mutate(Type = factor(ifelse(Target >= k,
                                paste0("Greater ",k),paste0("Less ",k))))
    
  house_train_labels = raw.house.data[train, ]$Type
  house_test_labels  = raw.house.data[-train, ]$Type
  
  house_classifier = naiveBayes(house_train, house_train_labels)
  house_test_pred = predict(house_classifier, house_test)
  
  bayes.confusion <- table(house_test_pred, house_test_labels)
  # CONTINUOUS
  # bucket[i,4] <- (bayes.confusion[1,1])/(bayes.confusion[1,1]+bayes.confusion[1,2])
  # bucket[i,5] <- (bayes.confusion[2,1])/(bayes.confusion[2,1]+bayes.confusion[2,2])
  # bucket[i,6] <- (bayes.confusion[1,1]+bayes.confusion[2,2])/sum(bayes.confusion)
  # CATEGORICAL
  bucket[i,2] <- (bayes.confusion[1,1])/(bayes.confusion[1,1]+bayes.confusion[1,2])
  bucket[i,3] <- (bayes.confusion[2,1])/(bayes.confusion[2,1]+bayes.confusion[2,2])
  bucket[i,4] <- (bayes.confusion[1,1]+bayes.confusion[2,2])/sum(bayes.confusion)
  
  # print(paste0("---- ",as.character(bucket[i,1]),": ",bucket[i,4]," --------"))
  print(paste0("---- ",k,": ",bucket[i,4]," --------"))
  
  i = i + 1
}

# PLOT THE RESULTS
bucket %>%
  gather(TPR,FPR,accuracy,
         key = metric, value = m.value) %>% 
  ggplot() + 
  geom_col(aes(x=bucket,y=m.value,fill=m.value)) +
  geom_text(aes(x=bucket,y=m.value,label=percent(m.value))) +
  facet_grid(metric ~ .) + 
  scale_fill_gradient(low = "red", high = "green") +
  labs(title = "Naïve Bayes classification accuracy",
       x = "Bucket",
       y = "Variable name")

bucketing <- data.frame(my.house.data$Id,0,0,0,0,0,0,0,0,0,0)
colnames(bucketing) <- c("Id","B1","B2","B3","B4","B5",
                         "I1","I2","I3","I4","I5")

for (i in 1:nrow(bucket)) {
  raw.house.data <- raw.house.data %>% 
    mutate(PriceType = factor(ifelse(SalePrice >= bucket[i,2] & SalePrice < bucket[i,3],
                                     as.character(bucket[i,1]),paste0("Not ",bucket[i,1]))))
  
  house_total_labels = raw.house.data$PriceType
  
  house_classifier = naiveBayes(house_total, house_total_labels)
  bucketing[,i+1] = predict(house_classifier, house_total, type = "raw")[,1]
  # bucketing[,i+1+5] = predict(house_classifier, house_total, type = "class")
  
  print("---------------")
  print(as.character(bucket[i,1]))
  
}

bucketing <- bucketing %>% 
  mutate(max.bucket = pmax(B1,B2,B3,B4,B5)) %>% 
  mutate(I1 = ifelse(B1 == max.bucket,1,0),
         I2 = ifelse(B2 == max.bucket,1,0),
         I3 = ifelse(B3 == max.bucket,1,0),
         I4 = ifelse(B4 == max.bucket,1,0),
         I5 = ifelse(B5 == max.bucket,1,0)) %>% 
  select(-max.bucket)

write.csv(bucketing, file = "./data/bucketing_houses.csv", row.names = FALSE)

head(bucketing)

################### PREPARING TO LOOP THE BUCKETIZING PROCESS #############

x.bucketize <- x.class$x.name[as.character(x.class$x.class) != "factor"][-c(1:3,6,8:12,15,16,18,19,21:28)]

bucket.3D <- list()

for (j in x.bucketize) {
  
  quantile(my.house.data[,j])
  Inf.b <- quantile(my.house.data[,j])[-5]
  Sup.b <- quantile(my.house.data[,j])[-1]
  
  Inf.b[1] <- Inf.b[1]-1
  Sup.b[4] <- Sup.b[4]+1
  
  bucket <- data.frame(c("B1","B2","B3","B4"),
                       Inf.b,
                       Sup.b,
                       rep(0,4))
  colnames(bucket) <- c("Bucket","Inf.b","Sup.b","accuracy")
  # bucket <- filter(bucket, Inf.b != Sup.b)
  raw.house.data <- raw.house.data %>% 
    mutate(Target = my.house.data[,j])
  
  print(paste0("---- ",j," --------"))
  
  i <- 1
  for (i in 1:nrow(bucket)) {
    raw.house.data <- raw.house.data %>%
      mutate(Type = factor(ifelse(Target >= bucket[i,2] & Target < bucket[i,3],
                                  as.character(bucket[i,1]),paste0("Not ",bucket[i,1]))))

    house_train_labels = raw.house.data[train, ]$Type
    house_test_labels  = raw.house.data[-train, ]$Type

    house_classifier = naiveBayes(house_train, house_train_labels)
    house_test_pred = predict(house_classifier, house_test)

    bayes.confusion <- table(house_test_pred, house_test_labels)
    bucket[i,4] <- (bayes.confusion[1,1]+bayes.confusion[2,2])/sum(bayes.confusion)

    print(paste0("---- ",as.character(bucket[i,1]),": ",percent(bucket[i,4])," ----"))

  }
  
  bucket.3D[[j]] <- bucket
  
}

## Choosing which variable is better to bucketize

x.bucket.accuracy <- data.frame(x.bucketize,0,0,0,0)
colnames(x.bucket.accuracy) <- c("x.name","B1","B2","B3","B4")

k <- 1
for (j in x.bucketize) {
  x.bucket.accuracy[k,-1] <- bucket.3D[[j]][,4]
  k = k + 1
}

bucket.3D$SalePrice

x.bucket.accuracy %>%
  gather(B1,B2,B3,B4,
         key = bucket, value = accuracy) %>% 
  ggplot() + 
  geom_tile(aes(x=bucket,y=reorder(x.name,accuracy,min),fill=accuracy)) +
  geom_text(aes(x=bucket,y=x.name,label=percent(accuracy))) +
  scale_fill_gradient(low = "red", high = "green") +
  labs(title = "Naïve Bayes classification accuracy",
       x = "Bucket",
       y = "Variable name")

## After choosing, get the predicted buckets for the best bucketized variable

quantile(my.house.data$YearBuilt)
Inf.b <- quantile(my.house.data$YearBuilt)[-5]
Sup.b <- quantile(my.house.data$YearBuilt)[-1]

bucket <- data.frame(c("B1","B2","B3","B4"),
                     Inf.b,
                     Sup.b,
                     rep(0,4))
colnames(bucket) <- c("Bucket","Inf.b","Sup.b","accuracy")

raw.house.data <- raw.house.data %>% 
  mutate(Target = my.house.data$YearBuilt)

bucketing <- data.frame(my.house.data$Id,0,0,0,0,0,0,0,0)
colnames(bucketing) <- c("Id","B1","B2","B3","B4",
                         "I1","I2","I3","I4")

for (i in 1:nrow(bucket)) {
  raw.house.data <- raw.house.data %>% 
    mutate(Type = factor(ifelse(Target >= bucket[i,2] & Target < bucket[i,3],
                                     as.character(bucket[i,1]),paste0("Not ",bucket[i,1]))))
  
  house_total_labels = raw.house.data$Type
  
  house_classifier = naiveBayes(house_total, house_total_labels)
  bucketing[,i+1] = predict(house_classifier, house_total, type = "raw")[,1]
  
  print("---------------")
  print(as.character(bucket[i,1]))
  
}

bucketing.house <- bucketing %>% 
  mutate(max.bucket = pmax(B1,B2,B3,B4,B5)) %>% 
  mutate(I1 = ifelse(B1 == max.bucket,1,0),
         I2 = ifelse(B2 == max.bucket,1,0),
         I3 = ifelse(B3 == max.bucket,1,0),
         I4 = ifelse(B4 == max.bucket,1,0)) %>% 
  select(-max.bucket)

write.csv(bucketing, file = "./data/bucketing_houses.csv", row.names = FALSE)

head(bucketing,10)








############ FINDING THE BEST WAY TO BUCKET SALES PRICE ############
# Update target for the bucketizing
library(pROC)

raw.house.data <- raw.house.data %>% 
  mutate(Target = my.house.data$SalePrice)

bucket.3D <- list()
k.SalePrice <- 30
k <- 30

for (k in k.SalePrice) {
  
  discr.SalePrice = discretize(my.house.data$SalePrice, method = "cluster", breaks = k)
  
  temp.discr <- discretize(my.house.data$SalePrice, method = "cluster", breaks = k, onlycuts = TRUE)
  
  cut.b <- temp.discr[-1]
  
  bucket <- data.frame(paste0("Less ",round(cut.b,2)),
                       cut.b,
                       rep(0,k),rep(0,k),rep(0,k),rep(0,k))
  colnames(bucket) <- c("Range","Sup.b","sensitivity","specificity","accuracy","auc")
  bucket <- bucket[-k,]
  
  print(paste0("---- ",k-1," breaks"," -----"))
  
  i <- 1
  for (i in 1:(k-1)) {
    raw.house.data <- raw.house.data %>%
      # mutate(Type = factor(ifelse(Target >= bucket[i,2] & Target < bucket[i,3],
      #                             as.character(bucket[i,1]),paste0("Not ",bucket[i,1]))))
      mutate(Type = factor(ifelse(Target < bucket[i,2],
                                  as.character(bucket[i,1]),paste0("Greater ",round(bucket[i,2],2)))))
    
    house_train_labels = raw.house.data[train, ]$Type
    house_test_labels  = raw.house.data[-train, ]$Type
    
    house_classifier = naiveBayes(house_train, house_train_labels)
    house_test_pred = predict(house_classifier, house_test)
    
    bayes.confusion <- table(house_test_pred, house_test_labels)
    bucket[i,3] <- (bayes.confusion[2,2])/(bayes.confusion[2,1]+bayes.confusion[2,2]) # sensitivity
    bucket[i,4] <- (bayes.confusion[1,1])/(bayes.confusion[1,1]+bayes.confusion[1,2]) # specificity
    bucket[i,5] <- (bayes.confusion[1,1]+bayes.confusion[2,2])/sum(bayes.confusion) # accuracy
    bucket[i,6] <- auc(house_test_labels,predict(house_classifier, house_test,type="raw")[,1]) # auc
    
    print(paste0("---- ",as.character(bucket[i,1]),": ",percent(bucket[i,6])))
    
  }
  
  bucket.3D[[k]] <- bucket
  
}

## Choosing which variable is better to bucketize

x.bucket.accuracy <- data.frame(rep(k.SalePrice,k.SalePrice-1),0,0,0,0,0,0)
colnames(x.bucket.accuracy) <- c("breaks","bucket","cut.b","sensitivity","specificity","accuracy","auc")

k <- 30
for (k in k.SalePrice) {
  x.bucket.accuracy[which(x.bucket.accuracy$breaks == k),-1] <- bucket.3D[[k]]
}

# head(x.bucket.accuracy)
# temp <- bucket.3D[[10]]

x.bucket.accuracy %>%
  ggplot() + 
  geom_point(aes(x=specificity,y=sensitivity), col="steel blue",size=2) +
  geom_abline(intercept = 1,slope = 1,col="red",linetype = "dashed") +
  scale_x_reverse() +
  labs(title = "Naïve Bayes classification accuracy",
       x = "specificity",
       y = "sensitivity")

library(plotly)

x.bucket.accuracy %>%
  plot_ly(x = ~specificity, y = ~sensitivity, z = ~accuracy, color = ~accuracy) %>%
  add_markers()

## After choosing, get the predicted buckets for the best bucketized SalePrice
k <- 15
discr.SalePrice = discretize(my.house.data$SalePrice, method = "cluster", breaks = k)

temp.discr <- discretize(my.house.data$SalePrice, method = "cluster", breaks = k, onlycuts = TRUE)

Sup.b <- temp.discr[-1]

bucket <- data.frame(levels(discr.SalePrice),
                     Sup.b,
                     rep(0,k))
colnames(bucket) <- c("Bucket","Inf.b","Sup.b","TPR")

raw.house.data <- raw.house.data %>% 
  mutate(Target = my.house.data$SalePrice)

bucketing <- data.frame(my.house.data$Id)
colnames(bucketing) <- c("Id")

for (i in 1:nrow(bucket)) {
  raw.house.data <- raw.house.data %>% 
    mutate(Type = factor(ifelse(Target >= bucket[i,2] & Target < bucket[i,3],
                                as.character(bucket[i,1]),paste0("Not ",bucket[i,1]))))
  
  house_total_labels = raw.house.data$Type
  
  house_classifier = naiveBayes(house_total, house_total_labels)
  bucketing = cbind(bucketing,predict(house_classifier, house_total, type = "raw")[,1])
  
  print("---------------")
  print(as.character(bucket[i,1]))
  
}

bucketing.house <- bucketing %>% 
  mutate(max.bucket = pmax(B1,B2,B3,B4,B5)) %>% 
  mutate(I1 = ifelse(B1 == max.bucket,1,0),
         I2 = ifelse(B2 == max.bucket,1,0),
         I3 = ifelse(B3 == max.bucket,1,0),
         I4 = ifelse(B4 == max.bucket,1,0)) %>% 
  select(-max.bucket)

write.csv(bucketing, file = "./data/bucketing_houses.csv", row.names = FALSE)

head(bucketing)




####################################################################
############ FINDING THE BEST WAY TO BUCKET SALES PRICE ############
####################################################################
# Update target for the bucketizing
library(pROC)

raw.house.data <- raw.house.data %>%
  mutate(Type = discretize(my.house.data$SalePrice, method = "frequency", breaks = 5))

temp.cuts <- discretize(my.house.data$SalePrice, breaks = 5, onlycuts = TRUE)



raw.house.data %>% 
  mutate(SalePrice = my.house.data$SalePrice) %>% 
  group_by(Type) %>% 
  # summarise(SalePrice=mean(SalePrice),n=n()) %>% 
  ggplot() +
  geom_point(aes(y=Type,x=SalePrice,col=SalePrice),
             alpha=0.2,position="jitter") +
  labs(title="Binning Sale Price",
       x="Bin",
       y="Log Sale Price") +
  guides(fill = "none") +
  scale_color_gradient(low="blue",high="red")

house_train_labels = raw.house.data[train, ]$Type
house_test_labels  = raw.house.data[-train, ]$Type

house_classifier = naiveBayes(house_train, house_train_labels)
house_test_pred = predict(house_classifier, house_test)

bayes.confusion <- table(house_test_pred, house_test_labels)

bayes.confusion %>% 
  as.data.frame() %>% 
  ggplot() +
  geom_tile(aes(x=house_test_labels,y=house_test_pred,fill=Freq/sum(Freq))) +
  geom_text(aes(x=house_test_labels,y=house_test_pred,label=percent(Freq/sum(Freq),accuracy = 2)),size = 4) + 
  scale_fill_gradient(low="red",high="green",
                      name = "Relative freq") +
  theme(axis.text=element_text(size=9),
        legend.position = "bottom") +
  labs(title="Naive Bayes Classifier Accuracy",
       x="Observed bin",
       y="Predicted bin")

sum(diag(bayes.confusion))/sum(bayes.confusion)

############ ENSEMBLING ############
k.SalePrice <- 5

raw.house.data <- raw.house.data %>%
  mutate(Type = discretize(my.house.data$SalePrice, method = "frequency", breaks = k.SalePrice))

nb.splits <- split(my.house.data, sample(1:5, nrow(my.house.data), replace=TRUE))

nb.accuracy <- data.frame(c(1:5),rep(0,k.SalePrice))
colnames(nb.accuracy) <- c("fold","accuracy")

nb.output <- data.frame(my.house.data$Id,0,0,0,0,0,0)
colnames(nb.output) <- c("Id","pB1","pB2","pB3","pB4","pB5","predicted.bucket")

k <- 1
for (k in 1:5) {
  
  house_dtm_train = house_dtm[-nb.splits[[k]][,1], ]
  house_dtm_test = house_dtm[nb.splits[[k]][,1], ]
  house_dtm_freq_train = removeSparseTerms(house_dtm_train, sparse = 0.999)
  house_freq_words = findFreqTerms(house_dtm_train, 5)
  house_dtm_freq_train = house_dtm_train[, house_freq_words]
  house_dtm_freq_test = house_dtm_test[, house_freq_words]
  house_train = apply(house_dtm_freq_train, 2, convert_counts)
  house_test = apply(house_dtm_freq_test, 2, convert_counts)
  
  house_train_labels = raw.house.data[-nb.splits[[k]][,1], ]$Type
  house_test_labels  = raw.house.data[nb.splits[[k]][,1], ]$Type
  
  house_classifier = naiveBayes(house_train, house_train_labels)
  house_test_pred = predict(house_classifier, house_test)
  
  bayes.confusion <- table(house_test_pred, house_test_labels)
  
  nb.accuracy[k,2] <- sum(diag(bayes.confusion))/sum(bayes.confusion)
  
  nb.output[nb.output$Id %in% nb.splits[[k]][,1],7] <- house_test_pred
  nb.output[nb.output$Id %in% nb.splits[[k]][,1],-c(1,7)] <- predict(house_classifier, house_test,type="raw")
  
  print(paste0("--- ",k,": ",percent(nb.accuracy[k,2])," ---"))
}



head(nb.output)
write.csv(nb.output, file = "./data/bucketing_houses.csv", row.names = FALSE)




########## TEST BUCKET PREDICTION ########

# Fit final classifier
house_total_classifier = naiveBayes(house_total, raw.house.data$Type)

realtest_house_total_pred = predict(house_total_classifier, realtest_house_total,type="raw")
colnames(realtest_house_total_pred) <- c("pB1","pB2","pB3","pB4","pB5")
nb.output.test.data<- data.frame(cbind(ak.test.data$Id,realtest_house_total_pred,0))
colnames(nb.output.test.data)[c(1,7)] <- c("Id","predicted.bucket")
nb.output.test.data$predicted.bucket <- as.character(predict(house_total_classifier, realtest_house_total))

u.buckets <- data.frame(cbind(sort(unique(nb.output.test.data$predicted.bucket)),c(1,2,3,4,5)))
colnames(u.buckets) <- c("predicted.bucket","bucket.id")

nb.output.test.data <- nb.output.test.data %>% 
  left_join(.,u.buckets,by="predicted.bucket") %>% 
  mutate(predicted.bucket = bucket.id) %>% 
  select(-bucket.id)

write.csv(nb.output.test.data, file = "./data/bucketing_test_houses.csv", row.names = FALSE)

