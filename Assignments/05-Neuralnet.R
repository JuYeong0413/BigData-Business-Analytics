##########################################
# 과목명 : 빅데이터와 비즈니스 애널리틱스#
# 과제명 : 5 - 신경망 ( Neuralnet )      #  
# 이름 : 이주영                          #
# 학번 : 2016111540                      #
# 학과 : 경영학부                        #
##########################################
rm(list = ls()) # object remove
##########################################

##########################################################
########## STEP.1 preparing the data (문제 1) ###########
##########################################################
# 문제 1-1 
# CSV file import
credit <- read.csv("credittw.csv")


# 문제 1-2
# 종속변수를 numeric으로 생성(신경망은 numeric)
credit$default <- as.numeric(credit$default)


# 문제 1-3
#  정규화 함수
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}


# 전체 데이터 프레임에 정규화 적용
credit_nm <- as.data.frame(lapply(credit, normalize))


# 문제 1-4
set.seed(325)
credit_rand <- credit[order(runif(10000)),]


# 문제 1-5
credit_train <- credit_rand[1:6000, -c(1)]
credit_test <- credit_rand[6001:10000, -c(1)]



##########################################################
########### STEP.2 Training a model (문제 2) #############
##########################################################

# 문제 2
library(neuralnet)
set.seed(23006)
credit_model <- neuralnet(default ~ LIMIT_BAL + AGE + PAY_1 + PAY_2 + PAY_4 + PAY_5 + PAY_6 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + PAY_AMT5 + PAY_AMT6, data = credit_train, hidden = 4)
plot(credit_model)



##########################################################
########### STEP.3 Computing a model (문제 3) ############
##########################################################

# 문제 3-1
# 모델 결과
model_result <- compute(credit_model, credit_test[1:23])


# 문제 3-2
credit_pr <- model_result$net.result


# 문제 3-3
# 배포된 반올림 코드
credit_pr <- sapply(credit_pr, round, digits = 0.1)


# 문제 3-4
credit_test$default <- factor(credit_test$default, levels = c(0, 1), labels = c(0, 1))
credit_pr <- factor(credit_pr, levels = c(0, 1), labels = c(0, 1))

 
##########################################################
########## STEP.4  Making a crosstable (문제 4) ##########
##########################################################

# 예측과 실제 분류의 교차표
library(gmodels)
CrossTable(credit_pr, credit_test$default,
           prop.chisq = FALSE, prop.r = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'default'))

