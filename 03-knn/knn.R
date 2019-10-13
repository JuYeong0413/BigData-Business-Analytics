#####  최근접 이웃(Nearest Neighbors)을 사용한 분류(Classification)  --------------------

## 예제 : 유방암 샘플 분류 ----

##########################################################
############### step.1 preparing the data ################
##########################################################

# CSV 파일 임포트
wbcd <- read.csv("cancer(knn).csv", stringsAsFactors = FALSE)

# wbcd 데이터 프레임의 구조
str(wbcd)

# id 속성 제거
wbcd <- wbcd[-1]

# 진단 테이블
table(wbcd$diagnosis)

# 팩터로서 진단 변수 변환
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"), labels = c("Benign", "Malignant"))

# 진단 변수의 비율
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)

# 세 속성에 대한 요약
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])

# 정규화 함수
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# 정규화 함수 테스트 - 결과는 일치함
normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40, 50))

# wbcd 데이터 정규화
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))

# 정규화가 잘 되었는지 확인
summary(wbcd_n$area_mean)

# 훈련 데이터와 테스트 데이터 생성
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]

# 훈련 데이터와 테스트 데이터에 대한 라벨 생성

wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

##########################################################
###### Step.2 Training a model on the data  ##############
##########################################################

# "class" 라이브러리 로드
library(class)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=21)


##########################################################
########## Step.3  Evaluating model performance ##########
##########################################################

# "gmodels" 라이브러리 로드
library(gmodels)

# 예측값과 실제값의 교차표 생성
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq=FALSE)



### 표준화 작업 
# 데이터 프레임를 z-score 표준화하기 위해 scale() 함수 사용
wbcd_z <- as.data.frame(scale(wbcd[-1]))

# 변환이 정확하게 적용되었는지 확인
summary(wbcd_z$area_mean)

# 훈련과 테스트 데이터셋 생성
wbcd_train <- wbcd_z[1:469, ]
wbcd_test <- wbcd_z[470:569, ]

# 변경한 데이터로 분류 
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k=21)

# 예측값과 실제값의 교차표 생성
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq=FALSE)

# 다른 k 값으로 분류
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=1)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=5)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=11)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=15)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=27)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)
