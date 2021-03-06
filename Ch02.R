# 작업공간 설정
setwd("C:/R/Social Science Data Analysis")

# SPSS 파일 불러오기 위한 패키지 install 및 load
install.packages("Hmisc")
library(Hmisc)

# 중2패널 데이터 불러오기
## use.value.labels=TRUE : factor 형태의 데이터(분석 어려움)
test <- spss.get("[중2패널] 1차년도_6차년도 데이터(SPSS)/04-1 중2 패널 1차년도 데이터(SPSS).sav",
                 use.value.labels=FALSE)
## names(test) : test 데이터셋에 저장된 변수들의 이름

################################################################################

# 변수 선택 - 숫자형 벡터 만들기
select_variables <- c(1,4,9,10,19,101,102,103,328:337,339:348,372,375,377,379,
                      484:489, 496:504, 532)

# 선택된 변수만 가지는 새로운 데이터셋 test1 생성
## test1 <- test[select_variables]
test1 <- test[,select_variables]

# 학교 위치에 대한 값이 100이상, 200미만인 데이터(서울)만 선택
spssdata <- test1[which(test1$scharew1 >= 100 & test1$scharew1 <200), ]

################################################################################

# sjmisc, sjlabelled 패키지 install 및 load
install.packages("sjmisc")
install.packages("sjlabelled")
library(sjmisc)
library(sjlabelled)

# label을 가진 중2패널 데이터 불러오기
test.labels <- spss.get("[중2패널] 1차년도_6차년도 데이터(SPSS)/04-1 중2 패널 1차년도 데이터(SPSS).sav",
                        use.value.labels=TRUE)

# 선택된 변수만 가지는 새로운 데이터셋 test.lavels1 생성
test.labels1 <- test.labels[select_variables]

# 숫자로 변경한 뒤 학교 위치에 대한 데이터 중 26 미만인 데이터(서울)만 선택
## as.numeric : 문자를 숫자로 변경
spssdata1 <- test.labels1[which(as.numeric(test.labels1$scharew1) < 26),]

# 라벨로 저장
labels.spss.values <- get_labels(spssdata1)
# 관측값과 라벨 일치시킴(각 관측값들이 라벨 가짐)
spssdata1 <- set_labels(spssdata, labels=labels.spss.values, force.values=FALSE, force.labels=TRUE)

# 데이터 저장하기
save(spssdata1, file="spssdata1.RData")

################################################################################

# 변수생성(attachment, grade)
attach(spssdata)
spssdata$attachment = q33a01w1+q33a02w1+q33a03w1+q33a04w1+q33a05w1+q33a06w1
#spssdata <- transform(spssdata, attachment=q33a01w1+q33a02w1+q33a03w1+q33a04w1+q33a05w1+q33a06w1)
spssdata$grade = q18a1w1+q18a2w1+q18a3w1
#detach(spssdata)

################################################################################

# epiDisplay 패키지 install 및 load : 변수의 최댓값과 최솟값 사이의 분포를 알아보기 위함
install.packages("epiDisplay")
library(epiDisplay)

# grade 변수에 대한 plot, table
## cum.percent = TRUE : 누적 확률값 함께 출력
tab1(spssdata$grade, cum.percent = TRUE)

# 새로운 변수 attachment, grade를 불러들이기 위해 다시 한 번 attach
attach(spssdata)

# grp.grade 변수 생성
spssdata$grp.grade[grade>=min(grade) & grade<=8] <- 1
spssdata$grp.grade[grade>=9 & grade<=10] <- 2
spssdata$grp.grade[grade>=11 & grade<=max(grade)] <- 3

# grp.grade 변수에 대한 plot, table
tab1(spssdata$grp.grade, cum.percent = TRUE)

################################################################################

# 역방향 코딩 : 응답의 방향성을 맞추기 위해
attach(spssdata)
spssdata$rq48a04w1[q48a04w1==1] <- 5
spssdata$rq48a04w1[q48a04w1==2] <- 4
spssdata$rq48a04w1[q48a04w1==3] <- 3
spssdata$rq48a04w1[q48a04w1==4] <- 2
spssdata$rq48a04w1[q48a04w1==5] <- 1

spssdata$rq48a05w1[q48a05w1==1] <- 5
spssdata$rq48a05w1[q48a05w1==2] <- 4
spssdata$rq48a05w1[q48a05w1==3] <- 3
spssdata$rq48a05w1[q48a05w1==4] <- 2
spssdata$rq48a05w1[q48a05w1==5] <- 1

spssdata$rq48a06w1[q48a06w1==1] <- 5
spssdata$rq48a06w1[q48a06w1==2] <- 4
spssdata$rq48a06w1[q48a06w1==3] <- 3
spssdata$rq48a06w1[q48a06w1==4] <- 2
spssdata$rq48a06w1[q48a06w1==5] <- 1
#detach(spssdata)

# 두 변수의 변수값(sexw1, grp.grade)을 교차시켜 하나의 집단변수 만들기
attach(spssdata)
spssdata$grp.sex.grade[sexw1==1 & grp.grade==1] <- 11
spssdata$grp.sex.grade[sexw1==1 & grp.grade==2] <- 12
spssdata$grp.sex.grade[sexw1==1 & grp.grade==3] <- 13
spssdata$grp.sex.grade[sexw1==2 & grp.grade==1] <- 21
spssdata$grp.sex.grade[sexw1==2 & grp.grade==2] <- 22
spssdata$grp.sex.grade[sexw1==2 & grp.grade==3] <- 23
#detach(spssdata)

# table 결과의 합이 전체 응답수와 다른 경우 결측값 존재
# 결측값이 특정한 값으로 지정된 경우 NA로 변경
## useNA="ifany" : 결측값의 갯수 확인
q33a07w1
table(q33a07w1)
table(q33a07w1, useNA="ifany")

################################################################################

# 데이터 저장하기
save(spssdata, file="spssdata.RData")
save(spssdata1, file="spssdata1.RData")

# workspace 저장하기(전부 저장하므로 저장할 파일 선택X)
save.image(file="myspssdata.RData")

################################################################################

# 데이터 병합하기 
second <- spss.get("[중2패널] 1차년도_6차년도 데이터(SPSS)/04-2 중2 패널 2차년도 데이터(SPSS).sav",
                 use.value.labels=FALSE)
mergedata <- merge(spssdata, second, by="id")

################################################################################

# 데이터 분할하기 
newdata <- subset(test, scharew1 >= 100 & scharew1 < 200, 
                  select=c("id", "sexw1", "scharew1", "areaw1", "q2w1", "q18a1w1", 
                           "q18a2w1", "q18a3w1"))
