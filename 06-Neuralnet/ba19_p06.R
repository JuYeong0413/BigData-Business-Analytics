########################################
###   신경망을 이용한 콘크리트 분석  ###
########################################

####### Part 1. Neural Networks ##########
## Example: Modeling the Strength of Concrete


####### Step 2. Exploring and preparing the data 
# 데이터 읽기와 구조 확인
concrete <- read.csv("concrete.csv")
str(concrete)
View(concrete)

#  정규화 함수
normalize <- function(x) {
    return((x - min(x)) / (max(x) - min(x)))
}

# 전체 데이터 프레임에 정규화 적용
concrete_norm <- as.data.frame(lapply(concrete, normalize))

# 0과1 사이에 범위 확인
summary(concrete_norm$strength)

# 본래 데이터의 최소값, 최대값 비교
summary(concrete$strength)

# 훈련과 테스트 데이터 생성
concrete_train <- concrete_norm[1:773, ]
concrete_test <- concrete_norm[774:1030, ]

####### Step 3. Training a model on the data
# neuralnet 모델 훈련
install.packages("neuralnet")
library(neuralnet)

set.seed(12345)

# 하나의 은닉 뉴런에 대한 단순한 ANN
concrete_model <- neuralnet(strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + age, data = concrete_train)


# 망(network) 시각화
plot(concrete_model)


####### Step 4. Evaluating model performance

# 모델 결과
model_results <- compute(concrete_model, concrete_test[1:8])

# 강도값을 예측하기 위하여 결과값만 받아서 진행 
predicted_strength <- model_results$net.result

# 예측값과 실제값간의 상관 관계 확인
# 결과 값이 다양하게 측정됨(매번 다르게 측정)
cor(predicted_strength, concrete_test$strength)



#######  Step 5. Improving model performance
# 5개 은닉 뉴런인 복잡한 뉴런망
set.seed(12345)
concrete_model2 <- neuralnet(strength ~ cement + slag +
                             ash + water + superplastic + 
                             coarseagg + fineagg + age,
                             data = concrete_train, 
                             hidden = 5)

# 망(network) 시각화
plot(concrete_model2)

# 결과 평가
model_results2 <- compute(concrete_model2, concrete_test[1:8])
predicted_strength2 <- model_results2$net.result
cor(predicted_strength2, concrete_test$strength)
