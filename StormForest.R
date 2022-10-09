rm(list=ls())
library(tidyverse)
library(caTools)
library(randomForest)

data <- read_csv("hugeDataset_tidy.csv")

head(data)
summary(data)
names(data)

# Dropping columns
data <- subset(data, select=-c(CATEGORY, EVENT_ID, EPISODE_ID, SOURCE, DATA_SOURCE, kilo))
data <- subset(data, select=-c(EPISODE_NARRATIVE, EVENT_NARRATIVE))
names(data)

# Drop casualty columns
data <- subset(data, select=-c(INJURIES_DIRECT, INJURIES_INDIRECT, DEATHS_DIRECT, DEATHS_INDIRECT))

onlyCas <- data %>%
  filter(casualty %in% (1:10))

# make labels
# 1 - no deaths
# 2 - 1-10
# 3 - 11-50
# 4 - 51-250
# 5 - 250+

severity <- ifelse(data$casualty <= 0,
                1, ifelse(data$casualty > 0 & data$casualty <= 10, 2, 
                          ifelse(data$casualty > 10 & data$casualty <= 50, 3,
                                 ifelse(data$casualty > 50 & data$casualty <= 250, 4, 5))))
dataLabeled <- mutate(data, severity)

dataDrop <- subset(dataLabeled, select = -c(casualty, FLOOD_CAUSE, TOR_F_SCALE, TOR_LENGTH,
                                            TOR_WIDTH, TOR_OTHER_WFO, TOR_OTHER_CZ_STATE, TOR_OTHER_CZ_FIPS,
                                            TOR_OTHER_CZ_NAME, MAGNITUDE_TYPE, END_RANGE, BEGIN_RANGE,
                                            BEGIN_LAT, BEGIN_LON, END_LAT, END_LON))

dataDrop$MAGNITUDE[is.na(dataDrop$MAGNITUDE)]=0
summary(dataDrop)
dataFinal=na.omit(subset(dataDrop,select=-c(STATE_FIPS)))

# ---------- Random Forest ----------
# Splitting data in train and test data
set.seed(8888)

split=sample(1:700947, round(0.7*700947),rep=F)
train <- dataFinal[split,]
test <- dataFinal[-split,]

names(test)


# Fitting Random Forest to the train dataset
set.seed(8888)  # Setting seed
classifier_RF = randomForest(x = train[-27],
                             y = as.factor(train$severity),
                             ntree = 100
                             )
classifier_RF

# Predicting the Test set results
y_pred = predict(classifier_RF, newdata = test[-27], type = "response")

y_pred = round(y_pred, digits=0)
y_pred=ifelse(y_pred<1,1,ifelse(y_pred>5,5,y_pred))
tail(y_pred)

# Confusion Matrix
library(caret)
confusion_mtx = confusionMatrix(as.factor(test[, 27]), as.factor(y_pred))
confusion_mtx

# Plotting model
plot(classifier_RF)

# Importance plot
importance(classifier_RF)

# Variable importance plot
varImpPlot(classifier_RF, n.var=15)












onlyCas <- data %>%
  filter(casualty %in% (1))

# make labels
# 1 - 1
# 2 - 2-10
# 3 - 10-100
# 4 - 100+
# Change month from letters to numbers
data$BEGIN_DATE_TIME <- str_replace_all(data$BEGIN_DATE_TIME, "JAN", "01")
data$BEGIN_DATE_TIME <- str_replace_all(data$BEGIN_DATE_TIME, "FEB", "02")
data$BEGIN_DATE_TIME <- str_replace_all(data$BEGIN_DATE_TIME, "MAR", "03")
data$BEGIN_DATE_TIME <- str_replace_all(data$BEGIN_DATE_TIME, "APR", "04")
data$BEGIN_DATE_TIME <- str_replace_all(data$BEGIN_DATE_TIME, "MAY", "05")
data$BEGIN_DATE_TIME <- str_replace_all(data$BEGIN_DATE_TIME, "JUN", "06")

# Change month from letters to numbers
data$END_DATE_TIME <- str_replace_all(data$END_DATE_TIME, "JAN", "01")
data$END_DATE_TIME <- str_replace_all(data$END_DATE_TIME, "FEB", "02")
data$END_DATE_TIME <- str_replace_all(data$END_DATE_TIME, "MAR", "03")
data$END_DATE_TIME <- str_replace_all(data$END_DATE_TIME, "APR", "04")
data$END_DATE_TIME <- str_replace_all(data$END_DATE_TIME, "MAY", "05")
data$END_DATE_TIME <- str_replace_all(data$END_DATE_TIME, "JUN", "06")

typeof(data$END_DATE_TIME[1])
difftime(as.POSIXct(data$END_DATE_TIME[1]), as.POSIXct(data$BEGIN_DATE_TIME[1]), units="mins")
         
nonZeroCasualty=data %>%
  filter(casualty>0)

noCas <- data %>%
  filter(casualty<=0)

nonZeroCasualty <- rbind(nonZeroCasualty, noCas[sample(1:dim(noCas)[1], 200, replace=F),])
# Randomly sample 8k from 

severity <- ifelse(nonZeroCasualty$casualty <= 1,
                   1, ifelse(nonZeroCasualty$casualty <= 5,  2,3))
dataLabeled <- mutate(nonZeroCasualty, severity)

dataDrop <- subset(dataLabeled, select = -c(casualty, FLOOD_CAUSE, TOR_F_SCALE, TOR_LENGTH,
                                            TOR_WIDTH, TOR_OTHER_WFO, TOR_OTHER_CZ_STATE, TOR_OTHER_CZ_FIPS,
                                            TOR_OTHER_CZ_NAME, MAGNITUDE_TYPE, END_RANGE, BEGIN_RANGE,
                                            BEGIN_LAT, BEGIN_LON, END_LAT, END_LON))

# noCas <- subset(noCas, select = -c(casualty, FLOOD_CAUSE, TOR_F_SCALE, TOR_LENGTH,
#                                             TOR_WIDTH, TOR_OTHER_WFO, TOR_OTHER_CZ_STATE, TOR_OTHER_CZ_FIPS,
#                                             TOR_OTHER_CZ_NAME, MAGNITUDE_TYPE, END_RANGE, BEGIN_RANGE,
#                                             BEGIN_LAT, BEGIN_LON, END_LAT, END_LON))

dataDrop$MAGNITUDE[is.na(dataDrop$MAGNITUDE)]=0
summary(dataDrop)
dataFinal=na.omit(subset(dataDrop,select=-c(STATE_FIPS)))
#dataFinal <- subset(dataFinal, samp)

dataFinal2 <- dataFinal%>%
  arrange(severity, decreasing=FALSE)

dataFinal3=dataFinal2[-c(1:3000),]

hist(dataFinal3$severity)

# ---------- Random Forest ----------
# Splitting data in train and test data
set.seed(123)

split=sample(1:5428, round(0.7*5428),rep=F)
train <- dataFinal3[split,]
test <- dataFinal3[-split,]

names(test)


# Fitting Random Forest to the train dataset
set.seed(888)  # Setting seed
classifier_RF = randomForest(x = train[-27],
                             y = as.factor(train$severity),
                             ntree = 300)
classifier_RF

# Predicting the Test set results
y_pred = predict(classifier_RF, newdata = test[-27], type = "response")

# y_pred = round(y_pred, digits=0)
tail(y_pred)
summary(y_pred)

# Confusion Matrix
library(caret)
confusion_mtx = confusionMatrix(as.factor(y_pred),as.factor(test$severity))
confusion_mtx
summary(test[,27])

varImpPlotGood <- varImpPlot(classifier_RF, n.var=20)
ggsave(varImpPlotGood, "varImpPlotGood.png", "png")
