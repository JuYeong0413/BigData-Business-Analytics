### R의 데이터 구조
## 벡터
subject_name <- c("John", "Dunpy", "Steve")
temperature <- c(32.2, 33.0, 37.9)
flu_status <- c(FALSE, FALSE, TRUE)


## 데이터 구조 요소 확인 
# 실수형
temperature <- c(32.2, 33.0, 37.9)
is(temperature)

# 문자형
subject_name <- c("John", "Dunpy", "Steve")
is(subject_name)

# 논리형
flu_status <- c(FALSE, FALSE, TRUE)
is(flu_status)


## 팩터
# 명목형
gender <- factor(c("MALE", "FEMALE", "MALE"))
gender

# 순서형
gender <- factor(gender, ordered=T)
gender

# 명목형 직접
blood <- factor(c("O", "AB", "A"),
                levels = c("A", "B", "AB", "O"))
blood


## 리스트
John <- list(fullname = subject_name[1],
             temperature = temperature[1],
             flu_status = flu_status[1],
             gender = gender[1],
             blood = blood[1])

John$fullname
John$temperature
John$flu_status
John$gender
John$blood

## 데이터 프레임 (people)
people <- data.frame(subject_name, temperature,
                     flu_status, gender, blood,
                     stringAsFactors=FALSE)


## 데이터 프레임 실습 1
people$subject_name
people[c("temperature", "flu_status")]
people[2:3] ## 2번째 열과 3번째 열의 예제 출력
people[1,2] ## 1번째 행과 2번째 열
people[c(1,3), c(2,4)] ## 1,3번째 행/2,4번째 열
people[1, ] ## 1행에 대한 모든 정보 출력
people[, 1] ## 1열에 대한 모든 정보 출력
people[ , ] ## 모든 행과 열

## 데이터 프레임 실습 2
## 2행과 3, 5열 정보 빼기 (Console)
people[-2, c(-3, -5)]

## 2행과 3, 5열 정보 빼기 (View)
View(people[-2, c(-3, -5)])


## 매트릭스 실습
#nrow : 2행 나누기
m1 <- matrix(c('a', 'b', 'c', 'd'), nrow = 2)

#ncol : 2열 나누기
m2 <- matrix(c('a', 'b', 'c', 'd'), ncol = 2)

#nrow : 3행 나누기
m3 <- matrix(c('a', 'b', 'c', 'd', 'e', 'f'), nrow = 3)

#ncol : 3열 나누기
m4 <- matrix(c('a', 'b', 'c' ,'d', 'e', 'f'), ncol = 3)

m4[1,1] # m4변수 1행 1열 출력
m4[2,3] # m4변수 2행 3열 출력

m3[1,] # m3변수 1행 전체 출력
m3[,1] # m3변수 1열 전체 출력


### R 데이터를 CSV에 저장하고 불러오기
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

