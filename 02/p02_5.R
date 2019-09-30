# gmodels 패키지 설치
install.packages("gmodels")

# gmodels 라이브러리 불러오기
library(gmodels)

# 변수 넣기
usedcars$conservative <- usedcars$color %in% c("Black", "Gray", "Silver", "White")
table(usedcars$conservative)

# CrossTable() : 이원배치표
CrossTable(x = usedcars$model, y = usedcars$conservative)

# function() : 함수 만들기
getCircleArea <- function(r) {
  area = 3.14 * r^2
  return(area)
}

getCircleArea(3)
