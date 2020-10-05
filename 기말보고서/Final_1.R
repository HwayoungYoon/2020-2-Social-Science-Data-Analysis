# 작업공간 설정
setwd("C:/R/Social Science Data Analysis")

# SPSS 파일 불러오기 위한 패키지 install 및 load
install.packages("Hmisc")
library(Hmisc)

# 데이터 불러오기
## use.value.labels=TRUE : factor 형태의 데이터(분석 어려움)
raw_data <- spss.get("기말보고서/(HRC) 1-1. 학업중단 청소년의 삶과 의식에 대한 종단조사_1차년도 데이터_776명(180326).sav",
                 use.value.labels=FALSE)
## names(test) : test 데이터셋에 저장된 변수들의 이름

################################################################################

# 변수 선택 - 숫자형 벡터 만들기
select_variables <- c(1,2,14,18,20,21,69,73:93,263,280,297,314,315,
                      339:395,398:403,445,447,449,451,453,455,457,
                      459,461,463,465,467,469,471,473,475,477,479,
                      481,483,485:514,525,530)

# 선택된 변수만 가지는 새로운 데이터셋 select_raw_data 생성
select_raw_data <- raw_data[,select_variables]

################################################################################





attach(raw_data)
table(SEX, useNA="ifany")
table(T1.Q4.1, useNA="ifany")
table(T1.Q5, useNA="ifany")
table(T1.Q6, useNA="ifany")
table(T1.Q7, useNA="ifany")
table(T1.Q15, useNA="ifany")
table(T1.Q16.1, useNA="ifany")
table(T1.Q16.2, useNA="ifany")
table(T1.Q16.3, useNA="ifany")
table(T1.Q16.4, useNA="ifany")
table(T1.Q16.5, useNA="ifany")
table(T1.Q16.6, useNA="ifany")
table(T1.Q16.7, useNA="ifany")
table(T1.Q16.8, useNA="ifany")
table(T1.Q16.9, useNA="ifany")
table(T1.Q16.10, useNA="ifany")
table(T1.Q17.1, useNA="ifany")
table(T1.Q17.2, useNA="ifany")
table(T1.Q17.3, useNA="ifany")
table(T1.Q17.3, useNA="ifany")
table(T1.Q18.1, useNA="ifany")
table(T1.Q18.2, useNA="ifany")
table(T1.Q18.3, useNA="ifany")
table(T1.Q18.4, useNA="ifany")
table(T1.Q18.5, useNA="ifany")
table(T1.Q18.6, useNA="ifany")
table(T1.Q18.7, useNA="ifany")
table(T1.Q20.1.99M, useNA="ifany")
table(T1.Q20.2.99M, useNA="ifany")
table(T1.Q20.3.99M, useNA="ifany")
table(T1.Q20.4.99M, useNA="ifany")
table(T1.Q21, useNA="ifany")
table(T1.Q27.1, useNA="ifany")
table(T1.Q27.2, useNA="ifany")
table(T1.Q27.3, useNA="ifany")
table(T1.Q27.4, useNA="ifany")
table(T1.Q27.5, useNA="ifany")
table(T1.Q27.6, useNA="ifany")
table(T1.Q28.1, useNA="ifany")
table(T1.Q28.2, useNA="ifany")
table(T1.Q28.3, useNA="ifany")
table(T1.Q28.4, useNA="ifany")
table(T1.Q28.5, useNA="ifany")
table(T1.Q28.6, useNA="ifany")
table(T1.Q28.7, useNA="ifany")
table(T1.Q28.8, useNA="ifany")
table(T1.Q28.9, useNA="ifany")
table(T1.Q28.10, useNA="ifany")
table(T1.Q29.1, useNA="ifany")
table(T1.Q29.2, useNA="ifany")
table(T1.Q29.3, useNA="ifany")
table(T1.Q29.4, useNA="ifany")
table(T1.Q29.5, useNA="ifany")
table(T1.Q29.6, useNA="ifany")
table(T1.Q29.7, useNA="ifany")
table(T1.Q29.8, useNA="ifany")
table(T1.Q29.9, useNA="ifany")
table(T1.Q29.10, useNA="ifany")
table(T1.Q29.11, useNA="ifany")
table(T1.Q29.12, useNA="ifany")
table(T1.Q29.13, useNA="ifany")
table(T1.Q29.14, useNA="ifany")
table(T1.Q29.15, useNA="ifany")
table(T1.Q30.1, useNA="ifany")
table(T1.Q30.2, useNA="ifany")
table(T1.Q30.3, useNA="ifany")
table(T1.Q30.4, useNA="ifany")
table(T1.Q30.5, useNA="ifany")
table(T1.Q30.6, useNA="ifany")
table(T1.Q30.7, useNA="ifany")
table(T1.Q30.8, useNA="ifany")
table(T1.Q31.1, useNA="ifany")
table(T1.Q31.2, useNA="ifany")
table(T1.Q31.3, useNA="ifany")
table(T1.Q31.4, useNA="ifany")
table(T1.Q31.5, useNA="ifany")
table(T1.Q31.6, useNA="ifany")
table(T1.Q31.7, useNA="ifany")
table(T1.Q31.8, useNA="ifany")
table(T1.Q32.1, useNA="ifany")
table(T1.Q32.2, useNA="ifany")
table(T1.Q32.3, useNA="ifany")
table(T1.Q32.4, useNA="ifany")
table(T1.Q32.5, useNA="ifany")
table(T1.Q32.6, useNA="ifany")
table(T1.Q32.7, useNA="ifany")
table(T1.Q32.8, useNA="ifany")
table(T1.Q32.9, useNA="ifany")
table(T1.Q34.1, useNA="ifany")
table(T1.Q34.2, useNA="ifany")
table(T1.Q34.3, useNA="ifany")
table(T1.Q34.4, useNA="ifany")
table(T1.Q34.5, useNA="ifany")
table(T1.Q34.6, useNA="ifany")
table(T1.Q33, useNA="ifany")
table(T1.Q40.1.1, useNA="ifany")
table(T1.Q44, useNA="ifany")
table(T1.Q45, useNA="ifany")
table(T1.Q48, useNA="ifany")


install.packages("writexl")
library(writexl)
write_xlsx(raw_data, path = "raw_data.xlsx")

colkk <- c("T1.Q30.1","T1.Q30.2","T1.Q30.3","T1.Q30.4","T1.Q30.5","T1.Q30.6","T1.Q30.7","T1.Q30.8")
sampletest <- raw_data[colkk]
head(sampletest)

tab1(sampletest$T1.Q30.1, cum.percent = TRUE)
tab1(sampletest$T1.Q30.2, cum.percent = TRUE)
tab1(sampletest$T1.Q30.3, cum.percent = TRUE)
tab1(sampletest$T1.Q30.4, cum.percent = TRUE)
tab1(sampletest$T1.Q30.5, cum.percent = TRUE)
tab1(sampletest$T1.Q30.6, cum.percent = TRUE)
tab1(sampletest$T1.Q30.7, cum.percent = TRUE)
tab1(sampletest$T1.Q30.8, cum.percent = TRUE)

tab1(raw_data$T1.Q25, cum.percent = TRUE)
tab1(raw_data$T1.Q26, cum.percent = TRUE)

tab1(raw_data$T1.Q28.1, cum.percent = TRUE)
tab1(raw_data$T1.Q28.2, cum.percent = TRUE)
tab1(raw_data$T1.Q28.3, cum.percent = TRUE)
tab1(raw_data$T1.Q28.4, cum.percent = TRUE)
tab1(raw_data$T1.Q28.5, cum.percent = TRUE)

tab1(raw_data$T1.Q28.6, cum.percent = TRUE)
tab1(raw_data$T1.Q28.7, cum.percent = TRUE)
tab1(raw_data$T1.Q28.8, cum.percent = TRUE)
tab1(raw_data$T1.Q28.9, cum.percent = TRUE)

tab1(raw_data$T1.Q29.1, cum.percent = TRUE)
tab1(raw_data$T1.Q29.2, cum.percent = TRUE)
tab1(raw_data$T1.Q29.3, cum.percent = TRUE)
tab1(raw_data$T1.Q29.4, cum.percent = TRUE)
tab1(raw_data$T1.Q29.5, cum.percent = TRUE)
tab1(raw_data$T1.Q29.6, cum.percent = TRUE)
tab1(raw_data$T1.Q29.7, cum.percent = TRUE)
tab1(raw_data$T1.Q29.8, cum.percent = TRUE)
tab1(raw_data$T1.Q29.9, cum.percent = TRUE)
tab1(raw_data$T1.Q29.10, cum.percent = TRUE)

tab1(raw_data$T1.Q29.11, cum.percent = TRUE)
tab1(raw_data$T1.Q29.12, cum.percent = TRUE)
tab1(raw_data$T1.Q29.13, cum.percent = TRUE)
tab1(raw_data$T1.Q29.14, cum.percent = TRUE)
tab1(raw_data$T1.Q29.15, cum.percent = TRUE)

tab1(raw_data$T1.Q38.1, cum.percent = TRUE)
tab1(raw_data$T1.Q38.2, cum.percent = TRUE)
tab1(raw_data$T1.Q38.3, cum.percent = TRUE)

tab1(raw_data$T1.Q39.1, cum.percent = TRUE)
tab1(raw_data$T1.Q39.2, cum.percent = TRUE)
tab1(raw_data$T1.Q39.3, cum.percent = TRUE)

tab1(raw_data$T1.Q36.1.2T, cum.percent = TRUE)
tab1(raw_data$T1.Q36.1.3T, cum.percent = TRUE)

tab1(raw_data$T1.Q36.2.2T, cum.percent = TRUE)
tab1(raw_data$T1.Q36.2.3T, cum.percent = TRUE)

tab1(raw_data$T1.Q36.3.2T, cum.percent = TRUE)
tab1(raw_data$T1.Q36.3.3T, cum.percent = TRUE)

tab1(raw_data$T1.Q36.5.2T, cum.percent = TRUE)
tab1(raw_data$T1.Q36.5.3T, cum.percent = TRUE)

tab1(raw_data$T1.Q36.6.2T, cum.percent = TRUE)
tab1(raw_data$T1.Q36.6.3T, cum.percent = TRUE)

tab1(raw_data$T1.Q32.1, cum.percent = TRUE)
tab1(raw_data$T1.Q32.2, cum.percent = TRUE)
tab1(raw_data$T1.Q32.3, cum.percent = TRUE)
tab1(raw_data$T1.Q32.4, cum.percent = TRUE)
tab1(raw_data$T1.Q32.5, cum.percent = TRUE)

tab1(raw_data$T1.Q32.6, cum.percent = TRUE)
tab1(raw_data$T1.Q32.7, cum.percent = TRUE)
tab1(raw_data$T1.Q32.8, cum.percent = TRUE)
tab1(raw_data$T1.Q32.9, cum.percent = TRUE)
