##################################
### Step.2 : Preparing the data###
##################################
# load the credit dataset
# 데이터 읽기와 구조 확인
credit <- read.csv("credit.csv")
head(credit)


# create a random sample for training and test data
# use set.seed to use the same random number sequence as the tutorial
set.seed(456)
credit_rand <- credit[order(runif(1000)),]
head(credit_rand)


# Data Frame split
credit_train <- credit_rand[1:900,]
credit_test <- credit_rand[901:1000,]


############################################
### Step.3 : Training a model on the data###
############################################

## Simple decision tree model ----
# build the simplest decision tree
install.packages("C50")
library(C50)
credit_model <- C5.0(credit_train[-17], credit_train$default)

credit_model

# create a factor vector of predictions on test data
credit_pred <- predict(credit_model, credit_test)



############################################
### Step.4 : Evaluating model performance###
############################################
install.packages("gmodels")
library(gmodels)
CrossTable(credit_test$default, credit_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predict default'))

############################################
### Step.5 : Improving model performance ###
############################################
##  Bagging
############################################

# Using the ipred bagged decision trees
install.packages("ipred")
library(ipred)
set.seed(300)

# 25개의 의사결정 트리 사용
mybag <- bagging(default ~ ., data = credit_train, nbagg = 25)

credit_pred <- predict(mybag, credit_test)
CrossTable(credit_test$default, credit_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predict default'))

############################################
### Step.5 : Improving model performance ###
############################################
##  Adaboost
############################################
install.packages("adabag")
library(adabag)
set.seed(300)

m_adaboost <- boosting(default ~., data = credit_train)
p_adaboost <- predict.boosting(m_adaboost, credit_test)
CrossTable(credit_test$default, p_adaboost$class,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predict default'))


############################################
### Step.5 : Improving model performance ###
############################################
##  Random Forest
############################################
install.packages("randomForest")
library(randomForest)
set.seed(300)

# random forest with default settings
# 분류기 구축
# rf <- randomForest(credit_train[-17], credit_train$default)과 같음
rf <- randomForest(default ~., data = credit_train)
rf

credit_pred <- predict(rf, credit_test)
CrossTable(credit_test$default, credit_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predict default'))
