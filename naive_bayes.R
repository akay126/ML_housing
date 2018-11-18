library(arules)
library(SnowballC)
library(e1071)
# library(tm)
library(wordcloud)

######## PREPARING THE DATA #########

# TRANSFORM THE DATA TO BE ABLE TO READ AS A SPARSE MATRIX
x.class <- as.data.frame(sapply(my.house.data,class))
colnames(x.class)[1] <- c("x.class")
x.class <- x.class %>% 
  mutate(x.name = row.names(.))

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
         YearBuilt = factor(YearBuilt))

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

colnames(cat.house.data)
head(cat.house.data)

"BsmtFullBath,
BsmtHalfBath,
FullBath,
HalfBath,
BedroomAbvGr,
KitchenAbvGr,
Fireplaces,
GarageCars" 

paste(colnames(cat.house.data),collapse = ",")

write.table(cat.house.data, file = "./data/cat_house_data.csv",sep=",",row.names = FALSE)

#####################################
#####Tools for Association Rules#####
#####################################

#Load the Association Rules library and store the groceries data in a sparse
#matrix.
library(arules)
categorical.house.data = read.transactions("./data/cat_house_data.csv", 
                                           sep = ",", skip = 1)

#Inspecting the categorical.house.data object we just created.
categorical.house.data
class(categorical.house.data)
dim(categorical.house.data)
colnames(categorical.house.data)
rownames(categorical.house.data)

#Gathering summary information for the categorical.house.data object we just created.
summary(categorical.house.data)

#Inspecting the distribution of transaction size.
# size(categorical.house.data)
hist(size(categorical.house.data))

#Using the inspect() function to look at the actual contents of the sparse
#matrix. In particular, looking at each tranasction.
inspect(categorical.house.data[1:2])

#Using the itemFrequency() function to look at the actual contents of the sparse
#matrix. In particular, looking at each item.
itemFrequency(categorical.house.data[, 1:5], type = "relative") #Default
itemFrequency(categorical.house.data[, 1:5], type = "absolute")

#Using the itemFrequencyPlot() function to visualize item frequencies.
itemFrequencyPlot(categorical.house.data)
itemFrequencyPlot(categorical.house.data, support = 0.1)
itemFrequencyPlot(categorical.house.data, topN = 20)

#Visualizing the binary relationship among transactions and items.
# set.seed(0)
# image(sample(categorical.house.data, 100))

#Using the apriori() function to look for association rules using the Apriori
#algorithm.
apriori(categorical.house.data,
        parameter = list(support = 0.09,
                         confidence = 0.8,
                         minlen = 2,
                         maxlen = 4,
                         maxtime = 10))

#Creating some rules by lowering the support and confidence.
houserules = apriori(categorical.house.data,
                       parameter = list(support = 0.02,
                                        confidence = 0.8,
                                        minlen = 2,
                                        maxlen = 4,
                                        maxtime = 10))

#Investigating summary information about the rule object.
houserules
class(houserules)
summary(houserules)

#Inspecting specific information about rules.
inspect(houserules[1:5])

#Sorting the rules by various metrics.
inspect(sort(houserules, by = "support")[1:5])
inspect(sort(houserules, by = "confidence")[1:5])
inspect(sort(houserules, by = "lift")[1:5])

milkrules = subset(houserules, support > 0.05)
inspect(sort(milkrules, by = "lift")[1:7])
#Finding subsets of rules based on a particular item.

berryrules = subset(houserules,  items %pin% "YearBuilt" & lift > 2)
inspect(sort(berryrules, by = "support")[1:5])

berryrules2 = subset(berryrules,  items %!in% "YearBuilt2006")
inspect(sort(berryrules2, by = "lift")[1:5])

herbrules = subset(houserules, subset = lhs %pin% "Neighborhood" &
                     lift > 2)
inspect(sort(herbrules, by = "support")[1:5])

herbrules2 = subset(herbrules, items %!in% "NeighborhoodSomerst")
inspect(sort(herbrules2, by = "support")[1:5])

###############################
#####Tools for Naïve Bayes#####
###############################
#Reading in the raw data into a data frame; ensuring that the strings
#aren't converted to factors.
# SalePrice.median <- quantile(my.house.data$SalePrice,0.5)
LotArea.groups <- quantile(my.house.data$LotArea)
LotArea.median <- quantile(my.house.data$LotArea,0.5)

raw.house.data <- cat.house.data %>% 
  select(-MSZoning) %>% 
  unite(.,col = "house", sep = " ") %>%
  # mutate(PriceType = my.house.data$SalePrice) %>%
  # mutate(PriceType = ifelse(PriceType > SalePrice.median,"High","Low"))
  mutate(LotArea = my.house.data$LotArea) %>%
  mutate(LotArea = ifelse(LotArea > LotArea.median,"High","Low"))

#Overwriting the type variable to convert it to a factor.
# raw.house.data$PriceType = as.factor(raw.house.data$PriceType)

#Inspecting the new PriceType variable.
# table(raw.house.data$PriceType)
table(raw.house.data$LotArea)

#Installing the Text Mining package for the purpose of processing text data
#for analysis; installing the SnoballC library for stemming purposes.
library(tm)
library(SnowballC)
library(RColorBrewer)
#Creating a corpus with the text message data; VectorSource() interprets each
#element of the vector that it is passed as an individual document.
house_corpus = Corpus(VectorSource(raw.house.data$house))

#Examining the overall contents of the SMS corpus.
house_corpus

#Subsetting the data into spam and ham groups.
High = subset(raw.house.data, LotArea == "High")
Low = subset(raw.house.data, LotArea == "Low")

#The wordcloud() function is versatile enough to automatically apply some text
#transformation and tokenization processes to raw data; we will transform our
#raw data for modeling purposes in a moment.
# library(wordcloud)
# pal2 <- brewer.pal(8,"Dark2")
# par(mfrow=c(1,2))
# wordcloud(High$house,min.freq = 30)
# wordcloud(Low$house,min.freq = 30)
# par(mfrow=c(1,1))
#Cleaning up the SMS corpus by performing transformations of the text data; converting
#all characters to lowercase, removing numbers, removing stopwords, removing punctuation,
#and stemming words. Creating a sparse document term matrix; this is called tokenization.
house_dtm = DocumentTermMatrix(house_corpus, control = list(
  tolower = TRUE,
  # removeNumbers = TRUE,
  # stopwords = function(x) { removeWords(x, stopwords()) },
  # removePunctuation = TRUE,
  stemming = TRUE
))

#Creating training and test sets with a 75% - 25% split; the observations are
#listed in random order.
train = sample(1:nrow(my.house.data), 0.7*nrow(my.house.data))
house_dtm_train = house_dtm[train, ]
house_train_labels = raw.house.data[train, ]$LotArea
house_dtm_test = house_dtm[-train, ]
house_test_labels  = raw.house.data[-train, ]$LotArea

#Checking that the proportion of spam and non-spam messages is similar among
#the training and test sets.
prop.table(table(house_train_labels))
prop.table(table(house_test_labels))

#Removing terms that are extremely sparse; terms that do not appear in 99.9% of
#the data.
house_dtm_freq_train = removeSparseTerms(house_dtm_train, sparse = 0.999)
house_dtm_train #Before
house_dtm_freq_train #After

#Displaying indicator features for frequent words (those that appear in at
#least approximately 0.1% of the text messages); saving the terms as a character
#vector.
# findFreqTerms(house_dtm_train, 5)
house_freq_words = findFreqTerms(house_dtm_train, 5)
str(house_freq_words)

#Create sparse document term matrices with only the frequent terms.
house_dtm_freq_train = house_dtm_train[, house_freq_words]
house_dtm_freq_test = house_dtm_test[, house_freq_words]

#Since the Naïve Bayes classifier is typically trained on data with categorical
#features, we need to change each of the counts to indicators.
convert_counts = function(x) {
  x = ifelse(x > 0, "Yes", "No")
}

#Using the apply() function to convert the counts to indicators in the columns
#of both the training and the test data.
house_train = apply(house_dtm_freq_train, 2, convert_counts)
house_test = apply(house_dtm_freq_test, 2, convert_counts)

#Inspecting the final matrices.
# head(house_train)
summary(house_train)

#Loading the e1071 library in order to implement the Naïve Bayes classifier.
library(e1071)

#Applying the naiveBayes() classifier function to the training data.
house_classifier = naiveBayes(house_train, house_train_labels)
# house_classifier

#Evaluating the model performance by predicting the test observations.
house_test_pred = predict(house_classifier, house_test)

#Creating a confusion matrix of the actual and predicted labels.
bayes.confusion <- table(house_test_pred, house_test_labels)
bayes.confusion
(bayes.confusion[1,1]+bayes.confusion[2,2])/sum(bayes.confusion)

#Directly out-of-the-box, the Naïve Bayes classifier performs extremely well,
#even when the assumptions are quite unrealistic. We only have an error rate
#of about 2.6%!

#Applying the Laplace estimator and inspecting the accuracy.
house_classifier2 = naiveBayes(house_train, house_train_labels, laplace = 1)
house_test_pred2 = predict(house_classifier2, house_test)
bayes.confusion <- table(house_test_pred2, house_test_labels)
bayes.confusion
(bayes.confusion[1,1]+bayes.confusion[2,2])/sum(bayes.confusion)

#Using the Laplace estimator, the error rate decreases slightly to about 2.4%;
#there was a slight reduction in both LotAreas of errors.