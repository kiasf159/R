exam <- read.csv("csv_exam.csv")
head(exam)
head(exam, 10)
tail(exam)
View(exam) # data Viewer
dim(exam) # 행, 열 출력
str(exam) # data Attribute
summary(exam) #요약 통계량 출력
install.package("ggplot2")
library(ggplot2)
mpg <- as.data.frame(mpg) # as로 data.frame 형태로 변환
head(mpg)
tail(mpg)
dim(mpg)
View(mpg)
summary(mpg)

# --------------- #
df_raw = data.frame(var1 = c(1, 2, 4), var2 = c(3,7,8))
df_raw
install.packages("dplyr") # rename package
library(dplyr)
df_new <- rename(df_raw, v2 = var2) #이름 바꾸기
df_new
# ---------------- 혼자서 해보기 #
str(mpg)
mpg_copy <- rename(mpg, city = cty, highway = hwy)
head(mpg_copy)
# 변수 추가하기
df_raw$var_sum <- df_raw$var1 + df_raw$var2
df_raw$var_mean <- (df_raw$var1 + df_raw$var2) / 2
mpg_copy$total <- (mpg_copy$city + mpg_copy$highway) / 2 # 연비 합쳐서 보여주기
mean(mpg_copy$total)
summary(mpg_copy$total)
hist(mpg_copy$total)
# 조건문을 이용한 변수 추가
mpg_copy$test <- ifelse(mpg_copy$total >= 20, "pass", "fail")
head(mpg_copy, 20)
library(ggplot2)
qplot(mpg_copy$test)
mpg_copy$grade <- ifelse(mpg_copy$total >= 30, "A", ifelse(mpg_copy$total >= 20, "B", "C"))
head(mpg_copy, 20)
table(mpg_copy$grade)
qplot(mpg_copy$grade)
mpg_copy$grade <- ifelse(mpg_copy$total >= 30, "S", ifelse(mpg_copy$total >= 25, "A", ifelse(mpg_copy$total >=20, "B", ifelse(mpg_copy$total >= 15, "C", ifelse(mpg_copy$total >= 10, "D", "E")))))
table(mpg_copy$grade)
qplot(mpg_copy$grade, ylab = "count")

# 분석 도전
midwest <- as.data.frame(ggplot2::midwest)
View(midwest)
head(midwest)
library(dplyr)
midwest <- replace(midwest, total = poptotal) #오류 ㅡㅡ Error in replace = unused argument(copy 있으나 없으나 같음)
midwest$asianPer <- midwest$popasian / midwest$poptotal * 100
hist(midwest$asianPer)
midwest$grade <- ifelse(midwest$asianPer > 0.4872462, "largge", "small")
qplot(midwest$grade)
