df <- data.frame(a = c(1, 7, 4, 3, NA), b = c(5, NA, 10, NA, 8))
is.na(df)
table(is.na(df))

#결측치 제거하기
library(dplyr)
df %>%filter(is.na(a))
df %>% filter(!is.na(a)) #결측치 제거한 행만 표시
df_copy <- df %>% filter(!is.na(a) & !is.na(b))
df_copy
df_copy2 <- na.omit(df)
df_copy2 #위와 동일

#결측치 제외기능 이용하기
mean(df$a, na.rm = TRUE)
exam <- read.csv("csv_exam.csv")
exam[c(3, 8, 15), "math"] <- NA #결측값 부여
exam %>% summarise(mean_math = mean(math, na.rm = T), sum_math = sum(math, na.rm = T), median_math = median(math, na.rm = T))

#평균값으로 결측값 대체하기
exam$math <- ifelse(is.na(exam$math), 55, exam$math)
outliner <- data.frame(score = c(1,3,5,7,9), score2 = c(11,6,8,3,4))
outliner$score <- ifelse(outliner$score > 10, NA, outliner$score)
outliner$score2 <- ifelse(outliner$score2 > 10, NA, outliner$score2)
outliner
library(ggplot2)
mpg <- as.data.frame(mpg)
boxplot(mpg$hwy)$stats #1 quarter, median, 3 quarter etc
mpg$hwy <- ifelse(mpg$hwy < 12 | mpg$hwy > 37, NA, mpg$hwy)
table(is.na(mpg$hwy))

# 혼자서 해보기
mpg <- as.data.frame(mpg)
mpg[c(10, 14, 58, 93), "drv"] <- "k"
mpg[c(29, 43, 129, 203), "cty"] <- c(3, 4, 39, 42)
table(mpg$drv)
mpg$drv <- ifelse(mpg$drv %in% c("4", "f", "r"), mpg$drv, NA)
boxplot(mpg$cty)$stats
mpg$cty <- ifelse(mpg$cty > 26 | mpg$cty < 9, NA, mpg$cty)
boxplot(mpg$cty)
mean(mpg$hwy, na.rm = T)
mpg %>% filter(!is.na(drv) & !is.na(cty)) %>% group_by(drv) %>% summarise(mean_cty = mean(cty))
