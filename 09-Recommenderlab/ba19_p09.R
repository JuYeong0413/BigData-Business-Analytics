##########################################
## 온라인 유통회사의 추천 시스템 사례 ####
##########################################
## step.1 preparing the data #####

# 데이터 로딩
data <- read.csv("sample.csv", head=FALSE, sep=",")


# 추천시스템 패키지를 인스톨
install.packages("recommenderlab")

# 패키지를 로딩
library(recommenderlab)


# 데이터형을 realRatingMatrix로 변환
r <- as(data, "realRatingMatrix")


## Step.2 Training a model on the data ###

# 머신러닝을 위해 학습데이터선정
# 전체데이터의 90%를 학습데이터로 선정
trainingData <- sample(4945, 4500)
trainingSet <- r[trainingData]


# 행의 총합 5개 이상의 아이템만 선택, 많이 팔리지 않은 아이템을 제외시킴
trainingSet <- trainingSet[rowCounts(trainingSet) > 5]


# scheme 지정과 표시
scheme <- evaluationScheme(trainingSet,
                            method = "split",
                            train = .8,
                            given = 6,
                            goodRating = 4,
                            k = 3)

scheme


# 추천모델생성
mUBCF <- Recommender(trainingSet,
                     method = "UBCF",
                     parameter = "Cosine")

mIBCF <- Recommender(trainingSet,
                     method = "IBCF",
                     parameter = "Cosine")


# 추천을 받을 사용자리스트 (Testset)
recommenderUserList <- r[-trainingData]


## Step.3  Evaluating model performance ###

#추천실시
UBCFlist <- predict(mUBCF,
                    recommenderUserList,
                    n = 5)

IBCFlist <- predict(mIBCF,
                    recommenderUserList,
                    n = 5)


#추천받은 리스트 보기
as(UBCFlist, "list")
as(IBCFlist, "list")


## Step.4 making model results ###


# 시뮬레이션에 사용할 알고리즘과 유사도 지표 종류 지정
alUBCF <- list(
  "user-based CF_Cosine" = list(name = "UBCF",
                                param = list(method = "Cosine")),
  "user-based CF_Pearson" = list(name = "UBCF",
                                 param = list(method = "Pearson")))

alIBCF <- list(
  "item-based CF_Cosine" = list(name = "IBCF",
                                param = list(method = "Cosine")),
  "item-based CF_Pearson" = list(name = "IBCF",
                                 param = list(method = "Pearson")))


# 실행 및 결과 표시
result1 <- evaluate(scheme, alUBCF, n = c(1, 3, 5))
result1
avg(result1)

result2 <- evaluate(scheme, alIBCF, n = c(1, 3, 5))
result2
avg(result2)
