##########################################
# 과목명 : 빅데이터와 비즈니스 애널리틱스#
# 과제명 : 3 - 나이브베이즈              #  
# 이름 : 이주영                          #
# 학번 : 2016111540                      #
# 학과 : 경영학부                        #
##########################################
rm(list = ls()) # object remove
##########################################

##########################################################
########## step.1 preparing the data (문제 1,2) ##########
##########################################################
# 문제 1-1
credit <- read.csv("credittw.csv", stringsAsFactors = FALSE)

# 문제 1-2
credit$default <- factor(credit$default, levels = c(0, 1), labels = c(0, 1))

# 문제 1-3
# 훈련과 테스트 데이터에 대한 무작위 샘플 생성
# 예제와 같은 무작위 수열을 사용하기 위해 set.seed 사용
set.seed(325)
credit <- credit[order(runif(10000)),]

# 문제 1-4
credit_rand <- credit[,c(3, 4, 5, 7, 8, 9, 10, 11, 12)]


# 문제 2-1
# 데이터 쪼개기 : Data Frame split
credit_train <- credit_rand[1:6000, ]
credit_test <- credit_rand[6001:10000, ]

# 문제 2-2
# 훈련 데이터와 테스트 데이터에 대한 라벨 생성
credit_train_labels <- credit[1:6000, 25]
credit_test_labels <- credit[6001:10000, 25]

# 문제 2-3
# 분류 변수의 비율 확인
prop.table(table(credit_train_labels))
prop.table(table(credit_test_labels))


##########################################################
###### Step.2 Training / Evaluating a model (문제 3) #####
##########################################################
library(e1071)
credit_classifier <- naiveBayes(credit_train, credit_train_labels)
credit_test_pred <- predict(credit_classifier, credit_test)


##########################################################
########## Step.3  Making a crosstable (문제 4) ##########
##########################################################
library(gmodels)
CrossTable(credit_test_pred, credit_test_labels,
           prop.chisq = FALSE, prop.r = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'actual'))


##########################################################
############################ 보너스 ######################
##########################################################
credit_rand_bonus <- credit[,c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 20)]

credit_train_bonus <- credit_rand_bonus[1:6000, ]
credit_test_bonus <- credit_rand_bonus[6001:10000, ]

credit_classifier_bonus <- naiveBayes(credit_train_bonus, credit_train_labels)
credit_test_pred_bonus <- predict(credit_classifier_bonus, credit_test_bonus)

CrossTable(credit_test_pred_bonus, credit_test_labels,
           prop.chisq = FALSE, prop.r = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'actual'))
