install.packages("foreign")
library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)

raw_walfare <- read.spss(file = "Koweps_hpc13_2018_beta1.sav", to.data.frame = T) # 2018년 데이터
welfare <- raw_walfare
View(welfare)
welfare <- rename(welfare,
                  sex = h13_g3, # 성별
                  birth = h13_g4, # 태어난 연도
                  marriage = h13_g10, # 혼인 상태
                  religion = h13_g11, # 종교
                  income = p1302_8aq1, # 월급
                  code_job = h13_eco9, # 직업 코드
                  code_region = h13_reg7) # 지역 코드

#1. 성별 간 월급의 차이가 있는가?
table(welfare$sex) # 1 = male , 2 = female
welfare$sex <- ifelse(welfare$sex == 1, "male", "female")
qplot(welfare$sex)
ggplot(welfare, aes(x = sex)) + geom_bar() #위와 같음
qplot(welfare$income) + xlim(0, 1500) # 수입 분석, 0 ~ 250이 가장 많음
### 전치리
summary(welfare$income) # 결측치 확인 10360
welfare$income <- ifelse(welfare$income %in% c(0,9999), NA, welfare$income) # 0 or 9999는 공란으로 처리된다.
table(is.na(welfare$income)) #결측값 재확인
sex_income <- welfare %>% filter(!is.na(income)) %>% group_by(sex) %>% summarise(mean_income = mean(income))
ggplot(sex_income, aes(x = sex, y = mean_income)) + geom_col() # female = 179.x , male = 347.

#2. 나이와 월급의 관계
qplot(welfare$birth)
ggplot(welfare, aes(x = birth)) + geom_bar()
### 전처리
summary(welfare$birth)
table(is.na(welfare$birth))
welfare$birth <- ifelse(welfare$birth == 9999, NA, welfare$birth) # 9999는 모름/무응답
welfare$age <- 2018 - welfare$birth + 1 # 나이 만들기
qplot(welfare$age)
age_income <- welfare %>% filter(!is.na(income)) %>% group_by(age) %>% summarise(mean_income = mean(income))
ggplot(age_income, aes(x = age, y = mean_income)) + geom_line() # 40 ~ 50에서 가장 많게 월급을 번다
table(age_income)

#3. 연령대에 따른 월급의 차이
welfare <- welfare %>% mutate(age_grade = ifelse(age < 35, "Young", ifelse(age < 64, "Middle", "Old")))
qplot(welfare$age_grade)
age_income <- welfare %>% filter(!is.na(income)) %>% group_by(age_grade) %>% summarise(mean_income2 = mean(income))
ggplot(age_income, aes(x = age_grade, y = mean_income2)) + geom_col() + scale_x_discrete(limits = c("Young", "Middle", "Old"))

#4. 성별 + 연령대 월급 차이
sex_income2 <- welfare %>% filter(!is.na(income)) %>% group_by(age_grade, sex) %>% summarise(mean_income = mean(income))
sex_income2 #연령대별 + 성별 차이이
ggplot(sex_income2, aes(x = age_grade, y = mean_income, fill = sex)) + geom_col() + scale_x_discrete(limits = c("Young", "Middle", "Old"))
ggplot(sex_income2, aes(x = age_grade, y = mean_income, fill = sex)) + geom_col(position = "dodge") + scale_x_discrete(limits = c("Young", "Middle", "Old")) # 그래프로 보는 차이
sex_age <- welfare %>% filter(!is.na(income)) %>% group_by(age, sex) %>% summarise(mean_income = mean(income)) # 나이에 따른 성별 월급 차이 분석
ggplot(sex_age, aes(x = age, y = mean_income, col = sex)) + geom_line()

#5. 직업별 월급 차이
table(welfare$code_job) # 직업명이 코드로 되어있기 때문에 코드표를 엑셀에서 읽은 후 join 과정을 거쳐야 함.
library(readxl)
list_job <- read_excel("Koweps_Codebook.xlsx", col_names = T, sheet = 2)
welfare <- left_join(welfare, list_job, id = "code_job") # code_job 기준으로 결합
welfare %>% filter(!is.na(code_job)) %>% select(code_job, job)
job_income <- welfare %>% filter(!is.na(income)) %>% group_by(job) %>% summarise(mean_income = mean(income))
head(job_income) #원래는 filter 부분에 직업이 없는 사람 확인도 해야 하지만 직업 NA는 없으므로 생략략
top20_job <- job_income %>% arrange(desc(mean_income)) %>% head(20)
ggplot(top20_job, aes(x = reorder(job, mean_income), y = mean_income)) + geom_col(fill = "#30A9DE") + coord_flip()#coord_flip은 90도 회전시켜주는 함수
bottom20_job <- job_income %>% arrange(mean_income) %>% head(20)
ggplot(bottom20_job, aes(x = reorder(job, -mean_income), y = mean_income)) + geom_col(fill = "#EFDC05") + coord_flip()

#6. 성별 직업 분포
job_male <- welfare %>% filter(!is.na(job) & sex == "male") %>% group_by(job) %>% summarise(n = n()) %>% arrange(desc(n)) %>% head(30)
job_female <- welfare %>% filter(!is.na(job) & sex == "female") %>% group_by(job) %>% summarise(n = n()) %>% arrange(desc(n)) %>% head(30)
ggplot(job_male, aes(x = reorder(job, n), y = n)) + geom_col(fill = "#A593E0") + coord_flip()
ggplot(job_female, aes(x = reorder(job, n), y = n)) + geom_col(fill = "#35E701") + coord_flip()

#7. 종교와 이혼의 상관관계
table(welfare$religion) # 1 = Yes, 2 = No
welfare$religion <- ifelse(welfare$religion == 1, "Yes", "No")
table(welfare$marriage) # 0 = 18세 미만, 1 = 배우자 있음, 2 = 사별, 3 = 이혼, 4 = 별거, 5 = 미혼(18세 이상, 미혼모 포함), 6 = 기타(사망 등)
welfare$group_marriage <- ifelse(welfare$marriage == 1, "Marriage",
                          ifelse(welfare$marriage == 3, "Divorce", NA)) # 변수 지정
qplot(welfare$group_marriage)
religion_marriage <- welfare %>% filter(!is.na(group_marriage)) %>% group_by(religion, group_marriage) %>% summarise(n = n()) %>% mutate(total_group = sum(n)) %>% mutate(percent = round(n/total_group * 100, 1))
# religion_marriage <- welfare %>% filter(!is.na(group_marriage)) %>% count(religion, group_marriage) %>% group_by(religion, group_marriage) %>% mutate(percent = round(n/total_group * 100, 1)) #위와 동일
divorce <- religion_marriage %>% filter(group_marriage == "Divorce") %>% select(religion, percent)
ggplot(divorce, aes(x = religion, y = percent)) + geom_col(fill = "skyblue")
# 연령대별 이혼
age_marriage <- welfare %>% filter(!is.na(group_marriage)) %>% count(age_grade, group_marriage) %>% group_by(age_grade) %>% mutate(percent = round(n/sum(n) * 100, 1))
age_divorce <- age_marriage %>% filter(group_marriage == "Divorce") %>% select(age_grade, percent)
ggplot(age_divorce, aes(x = age_grade, y = percent)) + geom_col(fill = 'black') # 10.5, 6.6, 2.5 순서이며, 중년 이혼율이 가장 높음.
#연령대 + 이혼율 + 종교 여부
age_religion_marriage <- welfare %>% filter(!is.na(group_marriage)) %>% count(age_grade, religion, group_marriage) %>% group_by(age_grade, religion) %>% mutate(percent = round(n/sum(n) * 100, 1))
age_religion_marriage
age_religion_divorce <- age_religion_marriage %>% filter(group_marriage == "Divorce") %>% select(age_grade, religion, percent)
age_religion_divorce
ggplot(age_religion_divorce, aes(x = age_grade, y = percent, fill = religion)) + geom_col(position = "dodge")
# 종교가 있을 때, 중년층의 이혼율은 낮은 편, but 나머지는 높은 편

#8. 지역별 연령대 비율
table(welfare$code_region) # 1 = 서울, 2 = 수도권, 3 = 경남(울산, 부산), 4 = 경북(대구), 5 = 충남(대전), 6 = 강원/충북, 7 = 전남/전북/제주(광주)
list_region <- data.frame(code_region = c(1:7),
                          region = c("서울", "인천/경기", "부산/울산/경남", "대구/경북", "대전/충남", "강원/충북", "광주/전북/전남/제주")) # 목록 만든 후 Join
welfare <- left_join(welfare, list_region, id = "code_region")
region_age <- welfare %>% count(region, age_grade) %>% group_by(region) %>% mutate(percent = round(n/sum(n)*100, 2))
ggplot(region_age, aes(x = region, y = percent, fill = age_grade)) + geom_col() + coord_flip()
region_age %>% filter(age_grade == "Old") #노년층 인구 통계
# 그래프 정렬 과정
list_order_old <- region_age %>% filter(age_grade == "Old") %>% arrange(percent)
order <- list_order_old$region
region_age$age_grade <- factor(region_age$age_grade, level = c("Old", "Middle", "Young"))
ggplot(region_age, aes(x = region, y = percent, fill = age_grade)) + geom_col() + coord_flip() + scale_x_discrete(limits = order)
View(raw_walfare)
