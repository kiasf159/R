install.packages("rJava")
install.packages("memoise")
install.packages("KoNLP")
install.packages("stringr")
install.packages("wordcloud")
Sys.setenv(JAVA_HOME = "C:\\Program Files\\Java\\jdk-11.0.4") #환경변수 설정 필수, 12버전 이상 지원 x
# 재실행시 이 부분부터 실행.
library(KoNLP)
library(dplyr)
useNIADic()
library(stringr)
library(wordcloud)
library(RColorBrewer)
extractNoun("대한민국의 영토는 한반도와 그 부속도서를 지역으로 한다.")
txt <- readLines("music.txt")
txt <- str_replace_all(txt, "\\W", " ")
nouns <- extractNoun(txt) # 명사 추출
wordcount <- table(unlist(nouns)) # 추출한 명사 list를 문자열 벡터로 변환, 단어별 빈도표 생성
df_word <- as.data.frame(wordcount, stringsAsFactors = F)
df_word <- rename(df_word, word = Var1, freq = Freq)
df_word <- filter(df_word, nchar(word) >= 2)
top_20 <- df_word %>% arrange(desc(freq)) %>% head(20)
pal <- brewer.pal(11,"RdYlBu")
set.seed(1234) #wordcloud는 함수 실행마다 난수를 이용해 매번 다른 모양의 워드 클라우드를 만들어 냄.
wordcloud(words = df_word$word,
          freq = df_word$freq,
          min.freq = 2,
          max.words = 350,
          random.order = F,
          rot.per = .1,
          scale = c(4, 0.5),
          colors = pal)

# 트위터? 텍스트 마이닝
twitter <- read.csv("twitter.csv", header = T, stringsAsFactors = F, fileEncoding = "UTF-8")
twitter <- rename(twitter, no = 번호, id = 계정이름, date = 작성일, tw = 내용)
twitter$tw <- str_replace_all(twitter$tw, "\\W", " ")
head(twitter$tw)
nouns2 <- extractNoun(twitter$tw)
wordcount2 <- table(unlist(nouns2))
df_word2 <- as.data.frame(wordcount2, stringsAsFactors = F)
df_word2 <- rename(df_word2, word = Var1, freq = Freq)
df_word2 <- filter(df_word2, nchar(word) >= 2)
df_word2 %>% arrange(desc(freq)) %>% head(10)
library(ggplot2)
top_25 <- df_word2 %>% arrange(desc(freq)) %>% head(25)
order <- arrange(top_25, freq)$word
ggplot(top_25, aes(x = word, y = freq)) + ylim(0, 2500) + geom_col() + coord_flip() + scale_x_discrete(limit = order) + geom_text(aes(label = freq), hjust = - 0.3) # 마지막은 빈도 표시시
pal <- brewer.pal(9, "Blues")[6:9]
set.seed(1234)
wordcloud(words = df_word2$word,
          freq = df_word2$freq,
          min.freq = 5,
          max.words = 350,
          random.order = F,
          rot.per = .1,
          scale = c(6, 0.2),
          colors = pal)
