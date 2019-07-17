library(ggplot2)
ggplot(data = mpg, aes(x = displ, y = hwy)) #배경 설정
ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point() #산점도 추가
ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point() + xlim(3,6) # x축 범위 3 , 6 지정
ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point() + ylim(10, 35)
# ggplot은 최종 분서 결과를 보기 위해 주로 사용, qplot은 전처리 단계에서 빠르게 보기 위해
# 혼자서 해보기
ggplot(data = mpg, aes(x = cty, y = hwy)) + geom_point()
ggplot(data = midwest, aes(x = poptotal, y = popasian)) + geom_point() + xlim(0, 500000) + ylim(0, 10000)

library(dplyr)
df_mpg <- mpg %>% group_by(drv) %>% summarise(mean_hwy = mean(hwy))
ggplot(df_mpg, aes(x = drv, y = mean_hwy)) + geom_col() # 막대그래프 추가
ggplot(df_mpg, aes(x = reorder(drv,-mean_hwy), y = mean_hwy)) + geom_col() # 오름차순 정렬
ggplot(mpg, aes(x = hwy)) + geom_bar() #col은 요약표, bar은 원자료를 이용한다.

# 혼자서 해보기
library(dplyr)
mpg_col <- mpg %>% filter(class == "suv") %>% group_by(manufacturer) %>% summarise(cty_mean_suv = mean(cty)) %>% arrange(desc(cty_mean_suv)) %>% head(5)
ggplot(mpg_col, aes(x = reorder(manufacturer, -cty_mean_suv), y = cty_mean_suv)) + geom_col()
ggplot(mpg, aes(x = class)) + geom_bar()

# 시계열 그래프
ggplot(economics, aes(x = date, y = unemploy)) + geom_line()
ggplot(economics, aes(x = date, y = psavert)) + geom_line()
mpg_class <- mpg %>% filter(class == "compact" | class == "subcompact" | class == "suv")
# mpg_class <- mpg %>% filter(class %in% c("compact", "subcompact", "suv"))
ggplot(mpg_class, aes(x = class, y = cty)) + geom_boxplot()
