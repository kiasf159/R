library(readxl)
df_exam <- read_excel("excel_exam.xlsx")
head(df_exam)
mean(df_exam$english)
mean(df_exam$science)
df_exam_novar <- read_excel("excel_exam_novar.xlsx", col_names = F)
head(df_exam_novar, 10)
df_exam_sheet <- read_excel("excel_exam_sheet.xlsx", sheet = 3)
head(df_exam_sheet)
df_csv_exam <- read.csv("csv_exam.csv", stringsAsFactors = F)
head(df_csv_exam)
########################################
df_midterm <- data.frame(english = c(90,80,60,70), math = c(50,60,100,80), class = c(1,2,3,4))
df_midterm
write.csv(df_midterm, file = "df_midterm.csv")
save(df_midterm, file = "df_midterm.rda")
rm(df_midterm)
df_midterm
load("df_midterm.rda")
df_midterm

