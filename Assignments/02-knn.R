##########################################
# 과목명 : 빅데이터와 비즈니스 애널리틱스#
# 과제명 : 2 - KNN 만들기                #  
# 이름 : 이주영                          #
# 학번 : 2016111540                      #
# 학과 : 경영학부                        #
##########################################
rm(list = ls()) # object remove 
##########################################


##########################################################
########## step.1 preparing the data (문제 1,2,3) ########
##########################################################
# 문제 1-1
credit <- read.csv("credittw.csv", stringsAsFactors = FALSE)

# 문제 1-2
View(credit)

# 문제 2
credit_rand <- credit[,-c(1, 3, 4, 5, 13, 14, 15, 16, 17, 18, 25)]

# 문제 3-1
credit_train <- credit_rand[1:6000, ]
credit_test <- credit_rand[6001:10000, ]

# 문제 3-2
credit_train_labels <- credit[1:6000, 25]
credit_test_labels <- credit[6001:10000, 25]

##########################################################
###### Step.2 Training a model on the data (문제 4) ######
##########################################################
install.packages("class")
library(class)

credit_test_pred <- knn(train = credit_train, test = credit_test, cl = credit_train_labels, k=17)

##########################################################
### Step.3  Evaluating model performance (문제 5) ########
##########################################################
install.packages("gmodels")
library(gmodels)

CrossTable(x = credit_test_labels, y = credit_test_pred, prop.chisq=FALSE)

##########################################################
### Step.4 Other K-value experiments  (문제 6) ###########
##########################################################
# k=1
credit_test_pred <- knn(train = credit_train, test = credit_test, cl = credit_train_labels, k=1)
CrossTable(x = credit_test_labels, y = credit_test_pred, prop.chisq=FALSE)

# k=5
credit_test_pred <- knn(train = credit_train, test = credit_test, cl = credit_train_labels, k=5)
CrossTable(x = credit_test_labels, y = credit_test_pred, prop.chisq=FALSE)

# k=11
credit_test_pred <- knn(train = credit_train, test = credit_test, cl = credit_train_labels, k=11)
CrossTable(x = credit_test_labels, y = credit_test_pred, prop.chisq=FALSE)


##########################################################
#################### 보너스  #############################
##########################################################
# k를 1부터 훈련데이터 개수의 제곱근(63)까지 넣어보며 최적의 k값을 찾아낸다.
# k를 변경한 실험결과
result <- numeric()
k = 1:63
for(i in k ) {
  credit_pred <- knn(credit_train, credit_test, credit_train_labels, k=i)
  t <- table(credit_pred, credit_test_labels)
  result[i-4] <- (t[1,1]+t[2,2])/sum(t)
}

k_value <- which(result==max(result))
k_value # 최적의 k값

credit_test_pred <- knn(train = credit_train, test = credit_test, cl = credit_train_labels, k=k_value)
CrossTable(x = credit_test_labels, y = credit_test_pred, prop.chisq=FALSE)

# 정규화
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

credit_n <- as.data.frame(lapply(credit[,-c(1, 3, 4, 5, 13, 14, 15, 16, 17, 18, 25)], normalize))

credit_n_train <- credit_n[1:6000, ]
credit_n_test <- credit_n[6001:10000, ]

result_n <- numeric()
for(i in k ) {
  credit_n_pred <- knn(credit_n_train, credit_n_test, credit_train_labels, k=i)
  t_n <- table(credit_n_pred, credit_test_labels)
  result_n[i-4] <- (t_n[1,1]+t_n[2,2])/sum(t_n)
}

k_n <- which(result_n==max(result_n))
k_n # 정규화를 이용했을 때 최적의 k값

credit_n_test_pred <- knn(train = credit_n_train, test = credit_n_test, cl = credit_train_labels, k=k_n)
CrossTable(x = credit_test_labels, y = credit_n_test_pred, prop.chisq=FALSE)

# 표준화
credit_z <- as.data.frame(scale(credit[-25]))

credit_z_train <- credit_z[1:6000, ]
credit_z_test <- credit_z[6001:10000, ]

result_z <- numeric()
for(i in k ) {
  credit_z_pred <- knn(credit_z_train, credit_z_test, credit_train_labels, k=i)
  t_z <- table(credit_z_pred, credit_test_labels)
  result_z[i-4] <- (t_z[1,1]+t_z[2,2])/sum(t_z)
}

k_z <- which(result_z==max(result_z))
k_z # 표준화를 이용했을 때 최적의 k값

credit_z_test_pred <- knn(train = credit_z_train, test = credit_z_test, cl = credit_train_labels, k=k_z)
CrossTable(x = credit_test_labels, y = credit_z_test_pred, prop.chisq=FALSE)
