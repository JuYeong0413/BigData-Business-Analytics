##########################################
# 과목명 : 빅데이터와 비즈니스 애널리틱스#
# 과제명 : 7 - Ensemble                  #  
# 이름 : 이주영                          #
# 학번 : 2016111540                      #
# 학과 : 경영학부                        #
##########################################
rm(list = ls()) # object remove
##########################################

##########################################################
########## step.1 preparing the data (문제 1) ###########
##########################################################

# 문제 1-1
credit <- read.csv("credittw.csv")

# 문제 1-2
set.seed(38)
credit_rand <- credit[order(runif(10000)),]

# 문제 1-3
credit_rand$default <- factor(credit_rand$default, levels = c(0, 1), labels = c(0, 1))

# 문제 1-4
credit_train <- credit_rand[1:6000, -c(1)]
credit_test <- credit_rand[6001:10000, -c(1)]

##########################################################
########### Step.2 Training a model (문제 2) #############
##########################################################

# 데이터를 적용해 모델  훈련하기
# Bagging, Boosting, Randomforest 중 택 1
RNGversion("3.6.1");set.seed(777)

# Randomforest
library(randomForest)

# 분류기 구축
rf <- randomForest(default ~ ., data = credit_train)
rf_pred <- predict(rf, credit_test)



##########################################################
######### Step.3 Making a crosstable (문제 3) ############
##########################################################

# 라이브러리 
library(gmodels)
# Cross table 만들기 
CrossTable(credit_test$default, rf_pred,
           prop.chisq = FALSE, prop.r = FALSE, prop.c = FALSE,
           dnn = c('actual default', 'predict default'))
