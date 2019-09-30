# 매트릭스 실습
# nrow : 2행 나누기
m1 <- matrix(c('a', 'b', 'c', 'd'), nrow = 2)
# ncol : 2열 나누기
m2 <- matrix(c('a', 'b', 'c', 'd'), ncol = 2)
# nrow : 3행 나누기
m3 <- matrix(c('a', 'b', 'c', 'd', 'e', 'f'), nrow = 3)
# ncol : 3열 나누기
m4 <- matrix(c('a', 'b', 'c' ,'d', 'e', 'f'), ncol = 3)

# m4 변수 1행 1열 출력
m4[1,1]
# m4 변수 2행 3열 출력
m4[2,3]
# m3 변수 1행 전체 출력
m3[1,]
# m3 변수 1열 전체 출력
m3[,1]

# RData 저장 및 로드
save(m1,m2,m3,m4, file = "mydata.RData") # 지금의 변수들을 저장
load("mydata.RData") # 데이터 변수 출력
# people 데이터프레임을 "first.csv" 파일로 저장
write.csv(people, file = "first.csv")
# "first.csv" 파일을 first로 불러오기("file="은 생략 가능)
first <- read.csv(file = "first.csv")
# CSV 파일을 factor 구조로 불러오지 않음
first <- read.csv("first.csv", stringsAsFactors = FALSE)
first <- first[,-1]
# read.csv의 header 비교
first1 <- read.csv("first.csv", stringsAsFactors = FALSE, header = TRUE)
first2 <- read.csv("first.csv", stringsAsFactors = FALSE, header = FALSE)

