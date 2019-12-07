##########################################
# 과목명 : 빅데이터와 비즈니스 애널리틱스#
# 과제명 : 8 - Recommender System        #  
# 이름 : 이주영                          #
# 학번 : 2016111540                      #
# 학과 : 경영학부                        #
##########################################
rm(list = ls()) # object remove
##########################################

##########################################################
########## step.1 preparing the data (문제 1) ###########
##########################################################

# 데이터 로딩
naver <- read.csv("naver.csv", head=FALSE, sep=",")

# 패키지를 로딩
library(recommenderlab)

# 데이터형을 realRatingMatrix로 변환
r <- as(naver, "realRatingMatrix")

##########################################################
########### Step.2 Training a model (문제 2) #############
##########################################################

# 머신러닝을 위해 학습데이터선정
trainingData <- sample(690, 483)
trainingSet <- r[trainingData]

# 행의 총합 4개 이상의 아이템만 선택, 많이 팔리지 않은 아이템을 제외시킴
trainingSet <- trainingSet[rowCounts(trainingSet) >= 4]

# scheme 지정과 표시
scheme <- evaluationScheme(trainingSet,
                           method = "split",
                           train = .8,
                           given = 4,
                           goodRating = 6,
                           k = 5)

# 추천의 평가방식을 저장 
mUBCF <- Recommender(trainingSet,
                     method = "UBCF",
                     parameter = "Cosine")

mIBCF <- Recommender(trainingSet,
                     method = "IBCF",
                     parameter = "Cosine")

recommenderUserList <- r[-trainingData]


##########################################################
#### Step.3 Evaluating model performance (문제 3) ########
##########################################################

# 추천실시
UBCFlist <- predict(mUBCF,
                    recommenderUserList,
                    n = 5)

IBCFlist <- predict(mIBCF,
                    recommenderUserList,
                    n = 5)

as(UBCFlist, "list")
as(IBCFlist, "list")


##########################################################
###### Step.4  Making model performance (문제 4) #########
##########################################################



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
result1 <- evaluate(scheme, alUBCF, n = c(6, 7, 8))
result2 <- evaluate(scheme, alIBCF, n = c(6, 7, 8))

avg(result1)
avg(result2)
