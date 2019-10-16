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
# k를 변경한 실험결과
# k=1부터 k=30까지의 결과를 확인해봤을 때, k=8에서 가장 좋은 결과가 나온다.
credit_test_pred <- knn(train = credit_train, test = credit_test, cl = credit_train_labels, k=8)
CrossTable(x = credit_test_labels, y = credit_test_pred, prop.chisq=FALSE)

# 정규화
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

credit_n <- as.data.frame(lapply(credit[2:25], normalize))
summary(credit_n$LIMIT_BAL)

credit_n_test_pred <- knn(train = credit_train, test = credit_test, cl = credit_train_labels, k=10)
CrossTable(x = credit_test_labels, y = credit_n_test_pred, prop.chisq=FALSE)

# 표준화
credit_z <- as.data.frame(scale(credit[-1]))
summary(credit_z$LIMIT_BAL)

credit_z_test_pred <- knn(train = credit_train, test = credit_test, cl = credit_train_labels, k=1)
CrossTable(x = credit_test_labels, y = credit_z_test_pred, prop.chisq=FALSE)
