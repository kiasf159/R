library(ggplot2)
library(dplyr)
exam <- read.csv("csv_exam.csv")
# 조건에 맞는 데이터만 추출하기
exam %>% filter(class == 1) # %>%은 파이프 연산자 ctrl + shift + M
exam %>% filter(class != 2)
exam %>% filter(math >= 50)
exam %>% filter(math >= 70 & science >= 70)
exam %>% filter(math >= 90 | english >= 90 | science >= 90)
exam %>% filter(class == 1 | class == 3 | class == 5)
exam %>% filter(class %in% c(1,3,5)) #위와 동일한 결과값
class1 <- exam %>% filter(class == 1)
high_score <- exam %>% filter(math >= 90 | english >= 90 | science >= 90)
mean(high_score$english)

#혼자서 해보기
mpg <- as.data.frame(mpg)
q1 <- mpg %>% filter(displ <= 4)
q2 <- mpg %>% filter(displ >= 5)
mean(q1$hwy)
mean(q2$hwy) #배기량이 4 이하인 자동차가 고속도로 연비가 평균적으로 더 높더라
audi <- mpg %>%  filter(manufacturer == "audi")
toyota <- mpg %>% filter(manufacturer == "toyota")
mean(audi$cty)
mean(toyota$cty)
car <- mpg %>% filter(manufacturer %in% c("chevrolet","ford", "honda"))
mean(car$hwy)

# 필요한 변수만 추출하기
exam %>% select(math, english)
exam %>% select(-math) # math 제외
exam %>% filter(class == 1) %>% select(english)
exam %>% 
  filter(class == 2 | class == 4) %>% 
  select(-science)
exam %>% filter(id, class, english) %>% 
  head(10)

#혼자 해보기
mpg <- as.data.frame(mpg)
mpg_test <- mpg %>% select(class, cty)
mpg_suv <- mpg_test %>% filter(class == "suv")
mean(mpg_suv$cty)
mpg_compact <- mpg_test %>%  filter(class == "compact")
mean(mpg_compact$cty)

# 데이터 순서대로 정렬
exam %>% arrange(english)
exam %>% arrange(desc(english))
exam %>% arrange(class, desc(english))

# 혼자 해보기
number1 <- audi %>% arrange(desc(hwy))
head(number1)                

# 파생변수 추가하기 
exam %>% mutate(total = math + english + science, mean = (math + english + science) / 3 )
exam %>% mutate(total = math + english + science, result = ifelse(total >= 210, "Pass", "Fail")) %>% head
exam %>% mutate(total = math + english + science) %>% arrange(desc(total)) %>% head

# 혼자 해보기
mpg1 <- mpg %>% mutate(add_opk = (cty + hwy) / 2)
mpg1 %>% arrange(desc(add_opk)) %>% head(3)
mpg %>% mutate(add_opk = (cty + hwy) / 2) %>% arrange(desc(add_opk)) %>% head(3)

# 집단별로 요약하기
exam %>% summarise(mean_math = mean(math))
exam %>% group_by(class) %>% summarise(mean_math = mean(math))
exam %>% group_by(class) %>% summarise(mean_math = mean(math), mean_english = mean(english))
mpg %>% group_by(manufacturer) %>% filter(class == "suv") %>% mutate(add_opk = (cty + hwy) / 2) %>% summarise(mean_opk = mean(add_opk)) %>% arrange(desc(mean_opk)) 

# 혼자 해보기
mpg %>% group_by(class) %>% summarise(cty_mean = mean(cty))
mpg %>% group_by(class) %>% summarise(cty_mean = mean(cty)) %>% arrange(desc(cty_mean))
mpg %>% group_by(manufacturer) %>% summarise(hwy_mean = mean(hwy)) %>% arrange(desc(hwy_mean)) %>% head(3)
mpg %>% group_by(manufacturer) %>% filter(class == "compact") %>% summarise(number = n()) %>% arrange(desc(number))

# 분석 도전
midwest <- as.data.frame(ggplot2::midwest)
midwest1 <- midwest %>% mutate(teenager = (poptotal - popadults) / poptotal * 100) %>% arrange(desc(teenager)) %>% select(county, teenager)
midwest1 <- midwest1 %>% mutate(ratio = ifelse(teenager >= 40, "large", ifelse(teenager >= 30, "middle", "small")))
table(midwest1$ratio)
midwest %>% mutate(asia_ratio = (popasian / poptotal) * 100) %>% select(state, county, asia_ratio) %>% arrange(asia_ratio) %>% head(10)
