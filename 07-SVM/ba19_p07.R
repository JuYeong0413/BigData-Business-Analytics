##### 서포트 벡터 머신 -------------------
## 예제 : 광학식 문자 인식 ----

## 1단계 : 데이터 살펴보기 ----
# 데이터 읽기와 구조
##############CODE##############
letters <- read.csv("letterdata.csv")
str(letters)




## 2단계 : 데이터 준비하기 ----
# 훈련 데이터와 테스터 데이터 구분
letters_train <- letters[1:16000, ]
letters_test <- letters[16001:20000, ]




## 3단계 : 데이터로 모델 훈련 ----
# 단순 선형 SVM을 훈련으로 시작
install.packages("kernlab")
library(kernlab)

letter_classifier <- ksvm(letter ~ .,
                          data = letters_train,
                          kernel = "vanilladot")

# 모델에 대한 기본 정보 확인
letter_classifier




## 4단계 : 모델 성능 평가 ----
# 테스트 데이터셋에 대한 예측
letter_predictions <- predict(letter_classifier, letters_test)
head(letter_predictions)

table(letter_predictions, letters_test$letter)




# 일치/불일치 예측을 표시하는 TRUE/FALSE 벡터 생성
agreement <- letter_predictions == letters_test$letter
table(agreement)
prop.table(table(agreement))


# 가우시안 RBF 커널을 이용
# 좀더 높은 차원 공간 모델 만들기 때문에 시간이 걸림 
set.seed(12345)
letter_classifier_rbf <- ksvm(letter~., data=letters_train, kernel = "rbfdot")

# 예측(시간이 조금 걸림)
letter_predictions_rbf <- predict(letter_classifier_rbf, letters_test)

# 예측값과 실제값의 일치/불일치를 표시하는 TRUE/FALSE 벡터 생성
agreement_rbf <- letter_predictions_rbf == letters_test$letter

# 4000개 사례 중 불일치/일치 사례 수를 표로 변환
table(agreement_rbf)

# 비율 확인
prop.table(table(agreement_rbf))

