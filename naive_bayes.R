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
         YrSold = factor(YrSold))

for (i in x.class$x.name[as.character(x.class$x.class) == "factor"]){
  temp.cat.house[,i] = factor(ifelse(as.character(temp.cat.house[,i]) == "" | temp.cat.house[,i] == 0,
                                     "",
                                     paste0(i,"",as.character(temp.cat.house[,i]))))
  # cat.house.data[,i] = paste0(i," ",cat.house.data[,i])
}

colnames(temp.cat.house)

cat.house.data <- temp.cat.house[,c(1:ncol(temp.cat.house))[as.character(x.class$x.class) == "factor"]] %>% 
  select(-Utilities, -Street, -Condition2, -RoofMatl,
         -Heating, -KitchenAbvGr, -LandSlope, -CentralAir,
         -Exterior2nd, -BsmtFinType2) %>% 
  select(-YrSold, -SaleType, -SaleCondition)

head(cat.house.data)

"BsmtFullBath,
BsmtHalfBath,
FullBath,
HalfBath,
BedroomAbvGr,
KitchenAbvGr,
Fireplaces,
GarageCars" 

View(cat.house.data)

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
        parameter = list(support = .25,     #Default minimum support.
                         confidence = .7,  #Default minimum confidence.
                         minlen = 2,       #Default minimum set size.
                         maxlen = 35))     #Default maximum set size.

#Creating some rules by lowering the support and confidence.
houserules = apriori(categorical.house.data,
                       parameter = list(support = 0.25,
                                        confidence = 0.5,
                                        minlen = 2,
                                        maxlen = 35))

#Investigating summary information about the rule object.
houserules
class(houserules)
summary(houserules)

#Inspecting specific information about rules.
inspect(houserules[1:5])

#Sorting the rules by various metrics.
inspect(sort(houserules, by = "support")[1:5])
inspect(sort(houserules, by = "confidence")[1:5])
inspect(sort(houserules, by = "lift")[6:10])

#Finding subsets of rules based on a particular item.

berryrules = subset(houserules, items %in% "FullBath-1" & 
                      items %!in% c("ExterQual-Gd","Foundation-PConc","Foundation-CBlock","BsmtQual-TA"))
inspect(sort(berryrules, by = "lift")[1:5])

herbs <- as.character(sort(unique(cat.house.data$Neighborhood)))
herbrules = subset(houserules, items %in% "FullBath-2")
# inspect(herbrules)
inspect(sort(herbrules, by = "lift")[1:5])

###############################
#####Tools for Naïve Bayes#####
###############################
#Reading in the raw data into a data frame; ensuring that the strings
#aren't converted to factors.
raw.house.data <- unite(cat.house.data, col = "house", sep = " ")

#Examining the structure of the data; two columns, one of the actual text itself
#and one displaying whether or not the observation is spam.
str(raw.house.data)

#Overwriting the type variable to convert it to a factor.
raw.house.data$type = as.factor(raw.house.data$type)

#Inspecting the new type variable.
str(raw.house.data$type)
table(raw.house.data$type)

#Installing the Text Mining package for the purpose of processing text data
#for analysis; installing the SnoballC library for stemming purposes.
library(tm)
library(SnowballC)

#Creating a corpus with the text message data; VectorSource() interprets each
#element of the vector that it is passed as an individual document.
house_corpus = Corpus(VectorSource(raw.house.data$house))

#Examining the overall contents of the SMS corpus.
house_corpus

#Examining the specific contents of the SMS corpus; converting the plain text
#documents to character strings.
tm::inspect(house_corpus[1:3])
lapply(house_corpus[1:3], as.character)

#Loading the wordcloud library to help visualize our corpus data.
library(wordcloud)
wordcloud(house_corpus, min.freq = 50) #Freq. of about 1% of the documents.
wordcloud(house_corpus, min.freq = 50, random.order = FALSE) #Order by frequency.

#Subsetting the data into spam and ham groups.
spam = subset(raw.house.data, type == "spam")
ham = subset(raw.house.data, type == "ham")

#The wordcloud() function is versatile enough to automatically apply some text
#transformation and tokenization processes to raw data; we will transform our
#raw data for modeling purposes in a moment.
wordcloud(spam$text, max.words = 40)
wordcloud(ham$text, max.words = 40)

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
house_dtm_train = house_dtm[1:1150, ]
sms_train_labels = raw.house.data[1:4169, ]$type
house_dtm_test = house_dtm[4170:5559, ]
sms_test_labels  = raw.house.data[4170:5559, ]$type

#Checking that the proportion of spam and non-spam messages is similar among
#the training and test sets.
prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))

#Removing terms that are extremely sparse; terms that do not appear in 99.9% of
#the data.
house_dtm_freq_train = removeSparseTerms(house_dtm_train, sparse = 0.999)
house_dtm_train #Before
house_dtm_freq_train #After

#Displaying indicator features for frequent words (those that appear in at
#least approximately 0.1% of the text messages); saving the terms as a character
#vector.
findFreqTerms(house_dtm_train, 5)
sms_freq_words = findFreqTerms(house_dtm_train, 5)
str(sms_freq_words)

#Create sparse document term matrices with only the frequent terms.
house_dtm_freq_train = house_dtm_train[, sms_freq_words]
house_dtm_freq_test = house_dtm_test[, sms_freq_words]

#Since the Naïve Bayes classifier is typically trained on data with categorical
#features, we need to change each of the counts to indicators.
convert_counts = function(x) {
  x = ifelse(x > 0, "Yes", "No")
}

#Using the apply() function to convert the counts to indicators in the columns
#of both the training and the test data.
sms_train = apply(house_dtm_freq_train, 2, convert_counts)
sms_test = apply(house_dtm_freq_test, 2, convert_counts)

#Inspecting the final matrices.
head(sms_train)
summary(sms_train)

#Loading the e1071 library in order to implement the Naïve Bayes classifier.
library(e1071)

#Applying the naiveBayes() classifier function to the training data.
sms_classifier = naiveBayes(sms_train, sms_train_labels)
sms_classifier

#Evaluating the model performance by predicting the test observations.
sms_test_pred = predict(sms_classifier, sms_test)
sms_test_pred

#Creating a confusion matrix of the actual and predicted labels.
table(sms_test_pred, sms_test_labels)
(1201 + 153)/1390

#Directly out-of-the-box, the Naïve Bayes classifier performs extremely well,
#even when the assumptions are quite unrealistic. We only have an error rate
#of about 2.6%!

#Applying the Laplace estimator and inspecting the accuracy.
sms_classifier2 = naiveBayes(sms_train, sms_train_labels, laplace = 1)
sms_test_pred2 = predict(sms_classifier2, sms_test)
table(sms_test_pred2, sms_test_labels)
(1202 + 155)/1390

#Using the Laplace estimator, the error rate decreases slightly to about 2.4%;
#there was a slight reduction in both types of errors.