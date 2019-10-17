########################################
#####   스팸 SMS 메시지 제거 분류  #####    
########################################
# 필요 라이브러리 설치 (해당 라이브러리가 있을 경우 건너뛰고 진행) 
# 없을 경우, 반드시 설치하고 진행해야 함

# "#" 주석풀기 : ctrl + shift + c 키
# install.packages("tm")
# install.packages("wordcloud")
# install.packages("gmodels")
# install.packages("e1071")
# install.packages("RColorBrewer")


## step.1 preparing the data #####

# CSV file import
sms_raw <- read.csv("spam.csv", stringsAsFactors = FALSE)

# sms 데이터 구조
str(sms_raw)

# 팩터로 spam/ham으로 변환
sms_raw$type <- factor(sms_raw$type)

# 변수형 확인
str(sms_raw$type)
table(sms_raw$type)

# 텍스트 마이닝(tm) 패키지를 사용하여 말뭉치 생성
library(tm)
sms_corpus <- VCorpus(VectorSource(sms_raw$text))

# 말뭉치 확인 과정 1 - print로 document 개수 확인
print(sms_corpus)

# 말뭉치 확인 과정 2 – 각각의 Text의 Metadata 구성 확인 
inspect(sms_corpus[1:2])

# 말뭉치 확인 과정 3 – 실제 문자 내용 확인 
as.character(sms_corpus[[1]])
lapply(sms_corpus[1:2], as.character)


# tm_map() 사용하여 말뭉치 정리
# 소문자 변환 
sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))

# 소문자 변환 후 비교 확인
as.character(sms_corpus[[1]])
as.character(sms_corpus_clean[[1]])


# remove numbers
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)
# remove stop words
sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords()) 
# remove punctuation
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)


# return the same vector of terms in its root form
sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)
# eliminate unneeded whitespace
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)


# 말뭉치 정리 전후 비교 (SMS message cleaning - before & after)
lapply(sms_corpus[1:3], as.character)
lapply(sms_corpus_clean[1:3], as.character)

# 문서-용어 희소 매트릭스 생성
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)
sms_dtm
inspect(sms_dtm)

# encoding 문제 대비 코드 
Sys.setlocale(category = "LC_ALL", locale = "us")

# 훈련과 테스트 데이터셋 생성
sms_raw_train <- sms_raw[1:4169,]
sms_raw_test <- sms_raw[4170:5559,]

sms_dtm_train <- sms_dtm[1:4169,]
sms_dtm_test <- sms_dtm[4170:5559,]

sms_corpus_train <- sms_corpus_clean[1:4169]
sms_corpus_test <- sms_corpus_clean[4170:5559]


# 스팸 비율 확인
prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))


# 워드(단어) 클라우드 시각화
library(wordcloud)
wordcloud(sms_corpus_train, min.freq = 50, random.order = FALSE)


# 훈련 데이터를 스팸과 햄으로 구분하여 시각화 
spam <- subset(sms_raw_train, type == "spam")
ham <- subset(sms_raw_train, type == "ham")

wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))


# 빈번한 단어에 대한 속성 지시자
sms_dict <- findFreqTerms(sms_dtm_train,5)
sms_train <- sms_dtm_train[ ,sms_dict]
sms_test <- sms_dtm_test[ ,sms_dict]


# 개수를 팩터로 변환
convert_counts <- function(x){
  x <- ifelse(x > 0, "Yes", "No")
  return(x)
}




# apply() convert_counts()를 사용한 훈련/테스트 데이터 추출
sms_train <- apply(sms_train, MARGIN = 2, convert_counts)
sms_test <- apply(sms_test, MARGIN = 2, convert_counts)



## Step.2 Training a model on the data ###
library(e1071)
sms_classifier <- naiveBayes(sms_train, sms_raw_train$type)
sms_classifier


## Step.3  Evaluating model performance ###
# 계산 2분 정도 걸림
sms_test_pred <- predict(sms_classifier, sms_test)
sms_test_pred

library(gmodels)
CrossTable(sms_test_pred, sms_raw_test$type,
           prop.chisq = FALSE, prop.r = FALSE, prop.t = FALSE, 
           dnn = c('predicted', 'actual'))


## Step.4 Improving model performance ###
# 라플라스 추정기 이용 (추정 설정 : 1 / 설정 안 할 경우 : 0)
sms_classifier2 <- naiveBayes(sms_train, 
                              sms_raw_train$type,
                              laplace = 1)

sms_test_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_raw_test$type,
           prop.chisq = FALSE, prop.r = FALSE, prop.t = FALSE, 
           dnn = c('predicted', 'actual'))


