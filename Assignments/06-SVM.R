##########################################
# 과목명 : 빅데이터와 비즈니스 애널리틱스#
# 과제명 : 6 - Support Vector Machine    #  
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
# CSV file import
credit <- read.csv("credittw.csv")

# 문제 1-2
# 훈련과 테스트 데이터에 대한 무작위 샘플 생성
# 예제와 같은 무작위 수열을 사용하기 위해 set.seed 사용
set.seed(33)
credit_rand <- credit[order(runif(10000)),]

# 문제 1-3
#  정규화 함수
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

credit_norm <- as.data.frame(lapply(credit_rand[,-c(25)], normalize))
credit_norm <- cbind(credit_norm, default=credit$default)

# 문제 1-4
# 종속변수를 factor으로 생성
credit_rand$default <- factor(credit_rand$default, levels = c(0, 1), labels = c(0, 1))

# 문제 1-5
# 데이터 쪼개기 : Data Frame split
credit_train <- credit_norm[1:6000, -c(1)]
credit_test <- credit_norm[6001:10000, -c(1)]

# 문제 1-6
# 데이터 다시 만들기  
credit_train <- cbind(credit_train, default=credit_rand[1:6000, 25])
credit_test <- cbind(credit_train, default=credit_rand[6001:10000, 25])


##########################################################
########### Step.2 Training a model (문제 2) #############
##########################################################
library(kernlab)
set.seed(123)

credit_classifier <- ksvm(default ~ .,
                          data = credit_train,
                          kernel = "rbfdot")


##########################################################
#### Step.3 Evaluating model performance (문제 3) ########
##########################################################
# 테스트 데이터셋에 대한 예측
credit_pred <- predict(credit_classifier, credit_test, type = "response")


##########################################################
########## Step.4  Making a CrossTable (문제 4) ##########
##########################################################
# 예측과 실제 분류의 교차표
library(gmodels)
CrossTable(credit_pred, credit_test$default,
           prop.chisq = FALSE, prop.r = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'default'))


##########################################################
#################### 보너스  #############################
##########################################################
rm(list = ls()) # object remove
##########################################################
# 문제 1-1
# CSV file import
credit <- read.csv("credittw.csv")

# 문제 1-2
# 훈련과 테스트 데이터에 대한 무작위 샘플 생성
# 예제와 같은 무작위 수열을 사용하기 위해 set.seed 사용
set.seed(33)
credit_rand <- credit[order(runif(10000)),]

# 문제 1-3
#  정규화 함수
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

credit_norm <- as.data.frame(lapply(credit[,-c(25)], normalize))
credit_norm <- cbind(credit_norm, default=credit$default)

# 문제 1-4
# 종속변수를 factor으로 생성
credit_rand$default <- factor(credit_rand$default, levels = c(0, 1), labels = c(0, 1))

# 문제 1-5
# 데이터 쪼개기 : Data Frame split
credit_train <- credit_norm[1:6000, -c(1)]
credit_test <- credit_norm[6001:10000, -c(1)]

# 문제 1-6
# 데이터 다시 만들기  
credit_train$default <- credit_rand[1:6000, 25]
credit_test$default <- credit_rand[6001:10000, 25]

set.seed(813)

credit_classifier_bonus <- ksvm(default ~ SEX +
                                          PAY_6 + 
                                          PAY_AMT6,
                          data = credit_train,
                          kernel = "rbfdot")

credit_pred_bonus <- predict(credit_classifier_bonus, credit_test, type = "response")
  
CrossTable(credit_pred_bonus, credit_test$default,
           prop.chisq = FALSE, prop.r = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'default'))
