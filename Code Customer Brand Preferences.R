#caret package instalation
install.packages("caret", dependencies = c("Depends", "Suggests"))
library(caret)

#two datasets import: Complete (DS) and Incomplete (SI)
getwd()
library(readxl)
DS <- read_excel("Complete.xlsx", sheet = "Survey Results Complete")
View(DS)
library(readr)
SI <- read.csv("SurveyIncomplete.csv")
View(SI)

#data check: Complete dataset (DS) 
head(DS)
tail(DS)
str(DS)
summary(DS)
sum(is.na(DS))

#variables as factors (DS)
DS$brand <- as.factor(DS$brand)
DS$elevel <- as.factor(DS$elevel)
DS$zipcode <- as.factor(DS$zipcode)
DS$car <- as.factor(DS$car)
str(DS)

#data check: Incomplete dataset (SI)
head(SI)
tail(SI)
str(SI)
summary(SI)
anyNA(SI)

#variables as factors (SI)
SI$brand <- as.factor(SI$brand)
SI$elevel <- as.factor(SI$elevel)
SI$zipcode <- as.factor(SI$zipcode)
SI$car <- as.factor(SI$car)
str(SI)

#data slicing
set.seed(123)
TrainSize <- createDataPartition(y = DS$brand, p = .75, list = FALSE)
training <- DS[TrainSize,] 
testing <- DS[- TrainSize,] 
dim(training)
dim(testing)

#10 fold cross validation
#ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3, verboseIter = TRUE)

#KNN
# accuracy 69%
#knn <- train(brand ~., data = training, method = "knn",  trControl = ctrl, tuneLength = 50, preProc = "range")  
#knn
#plot(knn)

#Random Forest 3 versions of parameters adjustment (search: default, random, grid)

#1 Random Forest - default parameters
# accuracy 72%
#ctrl1 <- trainControl(method = "repeatedcv", number = 10, repeats = 3,  verboseIter = TRUE)
#mtry <- sqrt(ncol(training))
#tunegrid <- expand.grid(.mtry = mtry)
#rf_default <- train(brand~., data = training, method = "rf", metric = "Accuracy", tuneGrid = tunegrid, trControl = ctrl1)
#rf_default

#2 Random Forest - random search
# accuracy 92%
ctrl2 <- trainControl(method = "repeatedcv", number = 10, repeats = 3, verboseIter = TRUE, search = "random")
rf_random <- train(brand~., data = training, method = "rf", metric = "Accuracy", trControl = ctrl2, tuneLength = 15)
rf_random
plot(rf_random)

#3 Random Forest - grid search
# accuracy 90%
#ctrl3 <- trainControl(method = "repeatedcv", number = 10, repeats = 3, verboseIter = TRUE, search = "grid")
#mtry <- sqrt(ncol(training))
#tunegrid <- expand.grid(.mtry = c(1:6))
#rf_grid <- train(brand~., data = training, method = "rf", metric = "Accuracy", tuneGrid = tunegrid, trControl = ctrl3)
#rf_grid
#plot(rf_grid)

#Prediction
#2 Random Forest (random search)
predRF <- predict(rf_random, newdata = testing)
predRF
summary(predRF)
postRF <- postResample(predRF, testing$brand)
postRF

SIpred <- predict(rf_random, newdata = SI)
SIpred
summary(SIpred)

#import of the prediction to excel 
SI$brand <- SIpred
View(SI)
head(training)



#plot
ggplot(SI, aes(brand)) +
  geom_bar()


save.image()

