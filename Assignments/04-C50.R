##########################################
# 과목명 : 빅데이터와 비즈니스 애널리틱스#
# 과제명 : 4 - 의사결정나무              #  
# 이름 : 이주영                          #
# 학번 : 2016111540                      #
# 학과 : 경영학부                        #
##########################################
rm(list = ls()) # object remove
##########################################

##########################################################
########## step.1 preparing the data (문제 1) ############
##########################################################

# 문제 1-1
credit <- read.csv("credittw.csv")

# 문제 1-2
credit$default <- factor(credit$default, levels = c(0, 1), labels = c(0, 1))

# 문제 1-3
credit_rand <- credit[,-c(1, 3, 4, 5, 13, 14, 15, 16, 17, 18)]

# 문제 1-4
# 훈련과 테스트 데이터에 대한 무작위 샘플 생성
# 예제와 같은 무작위 수열을 사용하기 위해 set.seed 사용
RNGversion("3.6.2");set.seed(325)
train_sample <- sample(10000, 6000)

# 문제 1-5
# 데이터 쪼개기 : Data Frame split
credit_train <- credit_rand[train_sample, ]
credit_test <- credit_rand[-train_sample, ]


# default 개수 확인
table(credit_train$default)
table(credit_test$default)

##########################################################
###### Step.2 Training / Evaluating a model (문제 2) #####
##########################################################

# 가장 단순한 결정 트리 생성
library(C50)
credit_model <- C5.0(credit_train[-15],
                     credit_train$default)

# 트리 정보 출력
summary(credit_model)

# 테스트 데이터에 대한 예측 팩터 벡터 생성
credit_test_pred <- predict(credit_model, credit_test)

##########################################################
########## Step.3  Making a crosstable (문제 3) ##########
##########################################################

# 예측과 실제 분류의 교차표
library(gmodels)
CrossTable(credit_test$default, credit_test_pred,
           prop.chisq = FALSE, prop.r = FALSE, prop.t = FALSE,
           dnn = c('actual default', 'predict default'))

##########################################################
##### Step.4  Improving model performance 1 (문제 4) #####
##########################################################

# 문제 4-1
# 부스팅 추가하기 (trials = 3)
Boost3 <- C5.0(credit_train[-15], credit_train$default,
               trials = 3)
Pred3 <- predict(Boost3, credit_test)
CrossTable(credit_test$default, Pred3,
           prop.chisq = FALSE, prop.r = FALSE, prop.t = FALSE,
           dnn = c('actual default', 'predict default'))

# 문제 4-2 
# 부스팅 추가하기 (trials = 30)
Boost30 <- C5.0(credit_train[-15], credit_train$default,
                trials = 30)
Pred30 <- predict(Boost30, credit_test)
CrossTable(credit_test$default, Pred30,
           prop.chisq = FALSE, prop.r = FALSE, prop.t = FALSE,
           dnn = c('actual default', 'predict default'))
