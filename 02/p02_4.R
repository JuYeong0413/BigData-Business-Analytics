## 데이터 이해와 탐구
# str() : 구조 확인 
usedcars <- read.csv("usedcars.csv", stringsAsFactors = FALSE)
str(usedcars)

# summary() : 요약 통계
summary(usedcars$year)
summary(usedcars[c("price", "mileage")])

# 평균과 중앙값
(36000 + 44000 + 56000) / 3
mean(c(36000, 44000, 56000))
median(c(36000, 44000, 56000))

# IQR : 사분위수
range(usedcars$price)
diff(range(usedcars$price))

IQR(usedcars$price)
quantile(usedcars$price)
quantile(usedcars$price, probs = c(0.01, 0.99))
quantile(usedcars$price, seq(from = 0, to = 1, by = 0.20))

# Boxplot 1
boxplot(usedcars$price,
        main="Boxplot of Used Car Prices",
        ylab="Price ($)")

# Boxplot 2
boxplot(usedcars$mileage,
        main="Boxplot of Used Car Mileage",
        ylab="Odometer (mi.)")

# Histogram 1
hist(usedcars$price,
     main = "Histogram of Used Car Prices",
     xlab = "Price ($)")
?hist()

# Histogram 2
hist(usedcars$mileage,
     main = "Histogram of Used Car Mileage",
     xlab = "Odometer (mi.)")


# 분산과 표준 편차
var(usedcars$price)
sd(usedcars$price)
var(usedcars$mileage)
sd(usedcars$mileage)

# 일원배치
# table () : 표 비율
table(usedcars$year)
table(usedcars$model)
table(usedcars$color)

# 표의 비율 알아보기
mt <- table(usedcars$model)
prop.table(mt)
# round () : 반올림
rmt <- prop.table(mt)
rmt <- prop.table(mt) * 100
round(rmt, digits = 1)

# 산포도
plot(x = usedcars$mileage, y = usedcars$price,
     main = "Scatterplot of Price vs. Mileage",
     xlab = "Used Car Odometer (mi.)",
     ylab = "Used Car Price ($)")
